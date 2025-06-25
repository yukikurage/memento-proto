{-# LANGUAGE OverloadedStrings #-}

{- | Type inference engine for the Memento type solver
Orchestrates the complete type inference pipeline using modular components
-}
module Language.Memento.TypeSolver.TypeInference (
  inferTypes,
  inferTypesWithTypedAST,
) where

import Control.Monad (foldM)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (traceM)

-- Syntax imports
import Language.Memento.Syntax
import qualified Language.Memento.Syntax.Definition as SDefinition
import Language.Memento.Syntax.Metadata (Metadata(..))
import qualified Language.Memento.Syntax.Program as SProgram
import Language.Memento.Syntax.Tag (KDefinition, KExpr, KProgram, KType, KTypeVariable, KVariable)
import qualified Language.Memento.Syntax.Variable as SVariable

-- Modular components

import Language.Memento.TypeSolver.ConstraintGenerator
import Language.Memento.TypeSolver.DataTypeAnalysis
import Language.Memento.TypeSolver.TypeEnvironment (TypeEnvironment (..), emptyTypeEnv, extendEnv, extendEnvScheme)
import Language.Memento.TypeSolver.TypeInfo (TypedAST)
import Language.Memento.TypeSolver.TypedASTBuilder
import Language.Memento.TypeSolver.VarianceAnalysis

-- Core solver components

import Language.Memento.TypeSolver.SolverPipeline (SolverState (..), createDefaultPipeline, executePipeline)
import Language.Memento.TypeSolver.Types

-- ============================================================================
-- Main Interface Functions
-- ============================================================================

-- | Main type inference function using modular architecture
inferTypes :: AST KProgram -> Either TypeError (Map.Map T.Text TypeScheme)
inferTypes ast = do
  -- Phase 0: Pure analysis of data types and variances
  let dataTypeAnalysis = analyzeDataTypes ast
      varianceAnalysis = analyzeVariances dataTypeAnalysis

  -- Phase 1: Build type environment with data types
  initialTypeEnv <- buildInitialTypeEnvironment dataTypeAnalysis varianceAnalysis

  -- Phase 2: Collect all definitions with proper polymorphic types
  programTypeEnv <- buildProgramTypeEnvironment ast initialTypeEnv

  -- Phase 3: Generate and solve constraints for each definition
  checkProgramBodies ast programTypeEnv (vaVariances varianceAnalysis)

-- | Type inference that also returns typed AST 
inferTypesWithTypedAST :: AST KProgram -> Either TypeError (Map.Map T.Text TypeScheme, TypedAST KProgram)
inferTypesWithTypedAST ast = do
  -- For now, just return the type environment - TypedAST construction is future work
  typeEnv <- inferTypes ast
  Left $ InternalTypeError "Full compositional TypedAST construction deferred to future iteration"

-- ============================================================================
-- Implementation Functions
-- ============================================================================

-- | Build initial type environment with data types and constructors
buildInitialTypeEnvironment :: DataTypeAnalysis -> VarianceAnalysis -> Either TypeError TypeEnvironment
buildInitialTypeEnvironment dataAnalysis varianceAnalysis = do
  let initialEnv = emptyTypeEnv
      variances = vaVariances varianceAnalysis

  -- Add type constructor information
  let envWithTypes = foldl (addDataTypeToEnv variances) initialEnv (Map.elems $ dtaDataTypes dataAnalysis)

  return envWithTypes

-- | Add a single data type to the type environment
addDataTypeToEnv :: Map.Map T.Text [Variance] -> TypeEnvironment -> DataTypeInfo -> TypeEnvironment
addDataTypeToEnv varianceMap env dataType =
  let typeName = dtiName dataType
      typeKind = dtiKind dataType
      constructorNames = map ciName (dtiConstructors dataType)
      typeVariances = Map.findWithDefault (replicate typeKind Bivariant) typeName varianceMap

      typeConstructorInfo =
        TypeConstructorInfo
          { tciKind = typeKind
          , tciVariance = typeVariances
          , tciConstructors = constructorNames
          }

      -- Add type constructor info
      envWithTypeCtor =
        env
          { teTypeConstructors = Map.insert typeName typeConstructorInfo (teTypeConstructors env)
          , teConstructors = Set.union (Set.fromList constructorNames) (teConstructors env)
          }
   in envWithTypeCtor

-- | Build program-level type environment with all definitions
buildProgramTypeEnvironment :: AST KProgram -> TypeEnvironment -> Either TypeError TypeEnvironment
buildProgramTypeEnvironment ast initialEnv = do
  case unProgram (extractSyntax ast) of
    SProgram.Program declAsts -> do
      -- Process each declaration to extract type signatures
      foldM processDeclarationSignature initialEnv declAsts

-- | Process a single declaration to extract its type signature
processDeclarationSignature :: TypeEnvironment -> AST KDefinition -> Either TypeError TypeEnvironment
processDeclarationSignature env declAst = do
  case unDefinition (extractSyntax declAst) of
    SDefinition.ValDef varAst typeParams typeAst _exprAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      processPolymorphicValue env name typeParams typeAst
    SDefinition.DataDef dataNameAst constructorDefs -> do
      -- Data types already processed in buildInitialTypeEnvironment
      -- But we need to add constructor functions to the type environment
      processDataConstructors env constructorDefs
    SDefinition.TypeDef aliasAst _params typeAst -> do
      let SVariable.Var aliasName = unVariable (extractSyntax aliasAst)
      aliasType <- convertASTTypeInEnv env typeAst
      return $ extendEnv aliasName aliasType env

-- | Process a polymorphic value declaration
processPolymorphicValue :: TypeEnvironment -> T.Text -> [AST KTypeVariable] -> AST KType -> Either TypeError TypeEnvironment
processPolymorphicValue env name typeParams typeAst = do
  let paramNames = extractTypeParamNames typeParams

  -- Create consistent generic variable names
  let genericVars = map (\param -> "_" <> param <> "_gen") paramNames
      genericContext = Map.fromList (zip paramNames genericVars)
      envWithGenerics = env{teGenericContext = Map.union genericContext (teGenericContext env)}

  -- Convert declared type in the context of type parameters
  declaredType <- convertASTTypeInEnv envWithGenerics typeAst

  -- Create type scheme with the same generic variables used in the type
  let typeScheme = TypeScheme genericVars declaredType

  return $ extendEnvScheme name typeScheme env

-- | Process data constructors to add them to type environment
processDataConstructors :: TypeEnvironment -> [SDefinition.ConstructorDef AST] -> Either TypeError TypeEnvironment
processDataConstructors env constructorDefs = do
  foldM processConstructor env constructorDefs
 where
  processConstructor currentEnv (SDefinition.ConstructorDef ctorNameAst typeParams fullTypeAst) = do
    let SVariable.Var ctorName = unVariable (extractSyntax ctorNameAst)
        paramNames = extractTypeParamNames typeParams

    -- Create consistent generic variable names for constructors
    let genericVars = map (\param -> "_" <> param <> "_ctor") paramNames
        genericContext = Map.fromList (zip paramNames genericVars)
        envWithGenerics = currentEnv{teGenericContext = Map.union genericContext (teGenericContext currentEnv)}

    -- Convert constructor type
    fullType <- convertASTTypeInEnv envWithGenerics fullTypeAst

    -- Create type scheme with the same generic variables used in the type
    let typeScheme = TypeScheme genericVars fullType

    return $ extendEnvScheme ctorName typeScheme currentEnv

-- | Check all program bodies (expressions) with constraint generation and solving
checkProgramBodies :: AST KProgram -> TypeEnvironment -> Map.Map T.Text [Variance] -> Either TypeError (Map.Map T.Text TypeScheme)
checkProgramBodies ast typeEnv variances = do
  case unProgram (extractSyntax ast) of
    SProgram.Program declAsts -> do
      -- Process each value definition body
      foldM (checkDeclarationBody variances) (teGlobal typeEnv) declAsts

-- | Check a single declaration body
checkDeclarationBody :: Map.Map T.Text [Variance] -> Map.Map T.Text TypeScheme -> AST KDefinition -> Either TypeError (Map.Map T.Text TypeScheme)
checkDeclarationBody variances currentEnv declAst = do
  case unDefinition (extractSyntax declAst) of
    SDefinition.ValDef varAst typeParams typeAst exprAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
          paramNames = extractTypeParamNames typeParams

      -- Get the declared type scheme
      case Map.lookup name currentEnv of
        Just declaredScheme -> do
          -- Create generic context for this function's type parameters
          let genericVars = map (\param -> "_" <> param <> "_gen") paramNames
              genericContext = Map.fromList (zip paramNames genericVars)

          -- Generate constraints for the expression with proper generic context
          constraintEnv <- buildConstraintEnvWithGenerics currentEnv variances genericContext
          -- traceM $ "=== Generating constraints for: " <> T.unpack name
          -- traceM $ "=== Type environment: " <> T.unpack (T.unlines [k <> " : " <> (let (vs, ts) = formatTypeScheme v in vs <> " " <> ts) | (k, v) <- Map.toList currentEnv])
          result <- generateExprConstraints constraintEnv exprAst
          -- traceM $ "=== Generated constraints: " <> T.unpack (formatConstraintSet (cgrConstraints result))
          -- traceM $ "=== Inferred type: " <> T.unpack (formatType (cgrInferredType result))

          -- Solve the constraints using new modular pipeline
          let constraints = cgrConstraints result
              varianceMap = Map.map (const []) variances -- Convert to TypeConstructorVariances format
              solverState =
                SolverState
                  { ssConstraints = constraints
                  , ssAssumptions = Set.empty
                  , ssGenericBounds = Map.empty
                  , ssVariances = varianceMap
                  , ssStageHistory = []
                  }
              pipeline = createDefaultPipeline

          case runIdentity $ runExceptT $ executePipeline pipeline solverState of
            Left err -> Left err
            Right _ -> return currentEnv -- Type checking succeeded
        Nothing -> Left $ SomeTypeError $ "Declaration not found in environment: " <> name
    _ -> return currentEnv -- Skip non-value declarations

-- | Build constraint environment from type environment
buildConstraintEnv :: Map.Map T.Text TypeScheme -> Map.Map T.Text [Variance] -> Either TypeError ConstraintEnv
buildConstraintEnv typeEnv variances = buildConstraintEnvWithGenerics typeEnv variances Map.empty

-- | Build constraint environment with specific generic context
buildConstraintEnvWithGenerics :: Map.Map T.Text TypeScheme -> Map.Map T.Text [Variance] -> Map.Map T.Text T.Text -> Either TypeError ConstraintEnv
buildConstraintEnvWithGenerics typeEnv variances genericContext = do
  let typeConstructors = Map.empty -- Would need to rebuild this from typeEnv
      constructors = Set.empty -- Would need to extract constructor names
  return $
    ConstraintEnv
      { ceTypeEnv = typeEnv
      , ceTypeConstructors = typeConstructors
      , ceConstructors = constructors
      , ceGenericTypes = genericContext
      , ceVarCounter = 0
      }

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Extract type parameter names from AST
extractTypeParamNames :: [AST KTypeVariable] -> [T.Text]
extractTypeParamNames typeParams =
  [ case unTypeVariable (extractSyntax paramAst) of
    SVariable.TypeVar paramName -> paramName
  | paramAst <- typeParams
  ]

-- | Convert AST type to solver type in environment context (simplified)
convertASTTypeInEnv :: TypeEnvironment -> AST KType -> Either TypeError Type
convertASTTypeInEnv env ast =
  -- For now, use a simplified conversion - this is a placeholder
  -- In a full implementation, we'd need to handle the environment context properly
  convertASTType emptyConstraintEnv ast
 where
  emptyConstraintEnv =
    ConstraintEnv
      { ceTypeEnv = teGlobal env
      , ceTypeConstructors = teTypeConstructors env
      , ceConstructors = teConstructors env
      , ceGenericTypes = teGenericContext env
      , ceVarCounter = 0
      }

-- ============================================================================
-- TypedAST Construction (Future Work)
-- ============================================================================

-- Note: Full compositional TypedAST construction is deferred to future iteration
-- due to architectural complexity of AST/TypedAST type compatibility.
-- The core TypedAST infrastructure (TypeInfo, TypedASTBuilder) is implemented
-- and ready for use when the compositional transformation is completed.

-- ============================================================================
-- Debug Output
-- ============================================================================

-- | Debug trace for modular type checking
debugTypeChecking :: AST KProgram -> T.Text
debugTypeChecking ast =
  let dataAnalysis = analyzeDataTypes ast
      varianceAnalysis = analyzeVariances dataAnalysis
   in T.unlines
        [ "=== Modular Type Checking ==="
        , "Data Types: " <> T.pack (show $ Map.keys $ dtaDataTypes dataAnalysis)
        , "Variances: " <> T.pack (show $ Map.toList $ vaVariances varianceAnalysis)
        ]
