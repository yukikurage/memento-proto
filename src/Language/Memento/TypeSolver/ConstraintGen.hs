{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

-- | Constraint generation for the Memento type solver
module Language.Memento.TypeSolver.ConstraintGen (
  inferProgram,
  typeCheckAST,
) where

import Control.Monad (foldM, forM, forM_, unless, when, zipWithM, zipWithM_)
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Base (List)
import Text.Megaparsec (SourcePos)

import Control.Exception (throw)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Debug.Trace (traceM)
import Language.Memento.Data.HFix (HFix (..), unHFix)
import Language.Memento.Data.HProduct (type (:*:) (..))
import Language.Memento.Syntax
import Language.Memento.Syntax.BinOp (BinOp (Add))
import qualified Language.Memento.Syntax.BinOp as SBinOp
import qualified Language.Memento.Syntax.Definition as SDefinition
import Language.Memento.Syntax.Expr (Expr (..), Let (..))
import qualified Language.Memento.Syntax.Literal as SLiteral
import qualified Language.Memento.Syntax.MType as SMType
import Language.Memento.Syntax.Metadata (Metadata (..))
import qualified Language.Memento.Syntax.Pattern as SPattern
import qualified Language.Memento.Syntax.Program as SProgram
import Language.Memento.Syntax.Tag (KBinOp, KDefinition, KExpr, KLet, KLiteral, KPattern, KProgram, KType, KTypeVariable, KVariable)
import qualified Language.Memento.Syntax.Variable as SVariable
import Language.Memento.TypeSolver.Assumption (calculateGenericBounds)
import Language.Memento.TypeSolver.Solver (solve)
import Language.Memento.TypeSolver.Types
import Language.Memento.TypeSolver.DataTypeAnalysis (analyzeDataTypes, DataTypeAnalysis (..), DataTypeInfo (..), ConstructorInfo (..))
import Language.Memento.TypeSolver.VarianceAnalysis (analyzeVariances, vaVariances)
import Language.Memento.TypeSolver.VarianceSolver (generateVarianceEquations, solveVarianceSystem)

{-
NOTE: TypeVar from Solver is pure "internal" type variable.
SVariable.Var is the "external" variable that is used in the source code. (has binding to type constructor or generics)
-}

-- ============================================================================
-- Core Types
-- ============================================================================

data InferContext = InferContext
  { icVarCounter :: Int
  , icConstraints :: ConstraintSet
  , icTypeEnv :: Map.Map T.Text TypeScheme -- Maps variable names to their type schemes
  , icTypeConstructors :: Map.Map T.Text TypeConstructorInfo
  , icConstructors :: Set.Set T.Text
  , icGenericTypes :: Map.Map T.Text T.Text
  , icAssumptions :: AssumptionSet -- Type variable substitutions
  }
  deriving (Show)

type InferM = StateT InferContext (Except TypeError)

-- ============================================================================
-- Main Entry Points
-- ============================================================================

typeCheckAST :: AST KProgram -> Either TypeError (Map.Map T.Text TypeScheme)
typeCheckAST ast =
  let initialCtx =
        InferContext
          { icVarCounter = 0
          , icConstraints = Set.empty
          , icTypeEnv = Map.empty
          , icTypeConstructors = Map.empty
          , icConstructors = Set.empty
          , icGenericTypes = Map.empty
          , icAssumptions = Set.empty
          }
   in runExcept (evalStateT (inferProgram ast) initialCtx)

-- | Three-phase type checking: extract data types, collect definitions, then check bodies
inferProgram :: AST KProgram -> InferM (Map.Map T.Text TypeScheme)
inferProgram ast = do
  case unProgram (extractSyntax ast) of
    SProgram.Program declAsts -> do
      -- Phase 0: Extract data type information (pure analysis)
      let dataTypeAnalysis = analyzeDataTypes ast
      preregisterDataTypes dataTypeAnalysis

      -- Phase 1: Collect all definition types and data constructors
      mapM_ collectDefinitionType declAsts

      -- Phase 1.5: Solve variance equations for all data types
      solveAllVariancesNew ast

      -- Phase 2: Check each definition body individually
      mapM_ (withIsolatedEnv . checkSingleDefinition) declAsts
      gets icTypeEnv

-- ============================================================================
-- Type algebras
-- ============================================================================

appendTypeConstructorInfo :: TypeConstructorInfo -> TypeConstructorInfo -> Maybe TypeConstructorInfo
appendTypeConstructorInfo (TypeConstructorInfo k1 v1 cs1) (TypeConstructorInfo k2 v2 cs2)
  | k1 == k2 = Just $ TypeConstructorInfo k1 (zipWith combineVariance v1 v2) (cs1 ++ cs2)
  | otherwise = Nothing

-- ============================================================================
-- Core Inference Operations
-- ============================================================================

withIsolatedEnv :: (MonadState env m) => m a -> m a
withIsolatedEnv f = do
  ctx <- get
  result <- f
  put ctx
  return result

freshVar :: InferM TypeVar
freshVar = do
  ctx <- get
  let counter = icVarCounter ctx
  put ctx{icVarCounter = counter + 1}
  return $ TypeVar $ T.pack ("t" ++ show counter)

-- | For implicit generics introduction
freshGenerics :: InferM T.Text
freshGenerics = do
  ctx <- get
  let counter = icVarCounter ctx
  put ctx{icVarCounter = counter + 1}
  return $ T.pack ("_T" ++ show counter)

freshGenericsWithPrefix :: T.Text -> InferM T.Text
freshGenericsWithPrefix prefix = do
  ctx <- get
  let counter = icVarCounter ctx
  put ctx{icVarCounter = counter + 1}
  return $ "_" <> prefix <> T.pack (show counter)

addConstraint :: Constraint -> InferM ()
addConstraint c = modify $ \ctx -> ctx{icConstraints = Set.insert c (icConstraints ctx)}

addVar :: T.Text -> Type -> InferM ()
addVar name typ = addVarScheme name (TypeScheme [] typ)

addVarScheme :: T.Text -> TypeScheme -> InferM ()
addVarScheme name scheme = modify $ \ctx -> ctx{icTypeEnv = Map.insert name scheme (icTypeEnv ctx)}

addConstructor :: T.Text -> InferM ()
addConstructor name = modify $ \ctx -> ctx{icConstructors = Set.insert name (icConstructors ctx)}

-- | Add type var to generics map
addGenericType :: T.Text -> T.Text -> InferM ()
addGenericType name g = modify $ \ctx -> ctx{icGenericTypes = Map.insert name g (icGenericTypes ctx)}

-- | Add assumption for type variable substitution
addAssumption :: Type -> Type -> InferM ()
addAssumption left right = modify $ \ctx -> ctx{icAssumptions = Set.insert (Subtype left right) (icAssumptions ctx)}

addEqAssumption :: Type -> Type -> InferM ()
addEqAssumption t1 t2 = do
  addAssumption t1 t2
  addAssumption t2 t1

removeGenericType :: T.Text -> InferM ()
removeGenericType name = modify $ \ctx -> ctx{icGenericTypes = Map.delete name (icGenericTypes ctx)}

getTypeConstructorVariances :: InferM TypeConstructorVariances
getTypeConstructorVariances = gets (Map.map tciVariance . icTypeConstructors)

-- Assumption management
getAssumptions :: InferM AssumptionSet
getAssumptions = gets icAssumptions

saveAssumptions :: InferM AssumptionSet
saveAssumptions = gets icAssumptions

restoreAssumptions :: AssumptionSet -> InferM ()
restoreAssumptions assumptions = modify $ \ctx -> ctx{icAssumptions = assumptions}

-- | Pre-register data types to avoid forward reference issues
preregisterDataTypes :: DataTypeAnalysis -> InferM ()
preregisterDataTypes analysis = do
  -- Add placeholder type constructor info for all data types
  forM_ (Map.elems $ dtaDataTypes analysis) $ \dt -> do
    let placeholderInfo = TypeConstructorInfo
          { tciKind = dtiKind dt
          , tciVariance = replicate (dtiKind dt) Bivariant  -- Will be updated later
          , tciConstructors = map ciName (dtiConstructors dt)
          }
    modify $ \ctx -> ctx { icTypeConstructors = Map.insert (dtiName dt) placeholderInfo (icTypeConstructors ctx) }

-- | Solve variance equations using the new analysis system
solveAllVariancesNew :: AST KProgram -> InferM ()
solveAllVariancesNew ast = do
  -- Use the new pure data type analysis
  let dataTypeAnalysis = analyzeDataTypes ast
      varianceAnalysis = analyzeVariances dataTypeAnalysis
      solvedVariances = vaVariances varianceAnalysis

  -- Debug output
  traceM $ "New variance analysis results: " <> show (Map.toList solvedVariances)

  -- Update type constructor info with solved variances
  typeCtors <- gets icTypeConstructors
  forM_ (Map.toList typeCtors) $ \(typeName, info) -> do
    case Map.lookup typeName solvedVariances of
      Just variances -> do
        let updatedInfo = info{tciVariance = variances}
        modify $ \ctx -> ctx{icTypeConstructors = Map.insert typeName updatedInfo (icTypeConstructors ctx)}
      Nothing -> 
        traceM $ "Warning: No variance information found for type " <> T.unpack typeName

-- | Old variance solver (kept for comparison)
solveAllVariances :: InferM ()
solveAllVariances = do
  -- traceM "Starting variance solving phase..."
  typeCtors <- gets icTypeConstructors
  typeEnv <- gets icTypeEnv

  -- Generate variance equations from all type constructors
  let equations = generateVarianceEquations typeCtors typeEnv

  -- Debug output
  -- traceM $ "Generated variance equations: " <> show (Map.toList equations)

  -- Solve the variance system
  let solution = solveVarianceSystem equations

  -- Debug output
  -- traceM $ "Solved variances: " <> show (Map.toList solution)

  -- Update type constructor info with solved variances
  forM_ (Map.toList typeCtors) $ \(typeName, info) -> do
    -- Extract type parameters for this type
    let typeParams = getTypeParams typeName info
    -- Get solved variances for each parameter
    let solvedVariances = map (\param -> Map.findWithDefault Bivariant (typeName <> "_" <> param) solution) typeParams
    -- Update the type constructor info
    let updatedInfo = info{tciVariance = solvedVariances}
    modify $ \ctx -> ctx{icTypeConstructors = Map.insert typeName updatedInfo (icTypeConstructors ctx)}
 where
  -- Extract type parameter names from a type constructor
  getTypeParams :: T.Text -> TypeConstructorInfo -> [T.Text]
  getTypeParams _ (TypeConstructorInfo arity _ _) =
    -- For now, assume parameters are named T1, T2, etc.
    -- This should match the naming convention used in VarianceSolver
    map (\i -> "T" <> T.pack (show i)) [1 .. arity]

-- ============================================================================
-- Type Conversion and Instantiation
-- ============================================================================

convertMType :: AST KType -> InferM Type
convertMType ast = do
  let meta = extractMetadata ast
  case unMType (extractSyntax ast) of
    SMType.TNumber -> return TNumber
    SMType.TInt -> return TNumber
    SMType.TBool -> return TBool
    SMType.TString -> return TString
    SMType.TNever -> return TNever
    SMType.TUnknown -> return TUnknown
    SMType.TVar varAst -> do
      let SVariable.TypeVar name = unTypeVariable (extractSyntax varAst)
      case name of
        "string" -> return TString
        "number" -> return TNumber
        "bool" -> return TBool
        "never" -> return TNever
        "unknown" -> return TUnknown
        _ -> do
          ctx <- get
          case Map.lookup name (icTypeConstructors ctx) of
            Just (TypeConstructorInfo k _ _) | k == 0 -> return $ TApplication name []
            Just _ -> 
              -- Type constructor exists, should be applied with type arguments
              throwError $ UnboundTypeVariable (Just meta) name  -- Missing type arguments
            Nothing -> do
              -- Check if the name is a generic type
              case Map.lookup name (icGenericTypes ctx) of
                Just g -> do
                  -- If the name is in generic types, treat it as a generic type
                  return $ TGeneric g
                Nothing -> do
                  -- Var is not a generic type nor type constructor, then throw an error
                  throwError $ UnboundTypeVariable (Just meta) name
    SMType.TLiteral litAst ->
      return $ convertLiteral litAst
    SMType.TFunction params retType -> do
      paramTypes <- mapM (convertMType . snd) params
      returnType <- convertMType retType
      return $ TFunction paramTypes returnType
    SMType.TUnion types -> do
      convertedTypes <- mapM convertMType types
      return $ mkUnion convertedTypes
    SMType.TIntersection types -> do
      convertedTypes <- mapM convertMType types
      return $ mkIntersection convertedTypes
    SMType.TApplication baseAst argAsts -> do
      let SVariable.TypeVar baseName = unTypeVariable (extractSyntax baseAst)
      ctx <- get

      case Map.lookup baseName (icTypeConstructors ctx) of
        Just (TypeConstructorInfo k _ _) -> do
          -- If the base is a type constructor, we need to instantiate it with arguments
          when (k /= length argAsts) $
            throwError $
              SomeTypeError $
                "Type constructor " <> baseName <> " expects " <> T.pack (show k) <> " arguments, got " <> T.pack (show (length argAsts))
        Nothing -> throwError $ UnboundTypeVariable (Just meta) baseName

      argTypes <- mapM convertMType argAsts
      return $ TApplication baseName argTypes
 where
  convertLiteral :: AST KLiteral -> Type
  convertLiteral ast =
    case unLiteral (extractSyntax ast) of
      SLiteral.NumberLiteral n -> TLiteral (LNumber n)
      SLiteral.IntLiteral i -> TLiteral (LNumber (fromIntegral i))
      SLiteral.BoolLiteral b -> TLiteral (LBool b)
      SLiteral.StringLiteral s -> TLiteral (LString s)

instantiatePolymorphicType :: TypeScheme -> InferM Type
instantiatePolymorphicType (TypeScheme [] typ) = return typ
instantiatePolymorphicType (TypeScheme vars typ) = do
  -- Assign fresh vars to generics.
  -- ex)  id<T> : (v : T) => T     // Definition
  --      val a : number = id(1)  // Usage <- this time, T is substituted by fresh variable (that will be resolved to "number" by solver)
  freshVars <- mapM (const freshVar) vars
  let substitution = Map.fromList (zip vars (map TVar freshVars))
  substGenerics substitution typ

-- | Instantiate with fresh generics for GADT pattern matching
instantiatePolymorphicTypeWithScopeAwareness :: TypeScheme -> InferM Type
instantiatePolymorphicTypeWithScopeAwareness (TypeScheme [] typ) = return typ
instantiatePolymorphicTypeWithScopeAwareness (TypeScheme vars typ) = do
  -- For GADT pattern matching, we need fresh generics, not type variables
  freshGenericNames <- mapM freshGenericsWithPrefix vars
  let substitution = Map.fromList (zip vars (map TGeneric freshGenericNames))
  substGenerics substitution typ

-- ============================================================================
-- Phase 1: Type Collection
-- ============================================================================

-- | Phase 1: Collect definition type without checking body
collectDefinitionType :: AST KDefinition -> InferM ()
collectDefinitionType ast =
  case unDefinition (extractSyntax ast) of
    SDefinition.ValDef varAst typeParams typeAst _exprAst -> do
      -- Extract declared type and add to environment (skip body)
      let SVariable.Var name = unVariable (extractSyntax varAst)
      collectPolymorphicValType name typeParams typeAst
    SDefinition.DataDef dataNameAst constructorDefs -> do
      -- Data definitions: register constructor functions
      let SVariable.Var dataName = unVariable (extractSyntax dataNameAst)
      
      -- Process each constructor and add to type environment
      mapM_ (inferConstructorDefSimplified dataName) constructorDefs
    SDefinition.TypeDef aliasAst _params typeAst -> do
      -- Type definitions work the same
      let SVariable.Var aliasName = unVariable (extractSyntax aliasAst)
      aliasType <- convertMType typeAst
      addVar aliasName aliasType
 where
  collectPolymorphicValType name typeParams typeAst = do
    let paramNames =
          [ case unTypeVariable (extractSyntax paramAst) of
            SVariable.TypeVar paramName -> paramName
          | paramAst <- typeParams
          ]

    savedGenericTypes <- gets icGenericTypes

    -- Add type parameters to generic context
    freshGenerics <-
      mapM
        ( \paramName -> do
            g <- freshGenericsWithPrefix paramName
            addGenericType paramName g
            return g
        )
        paramNames

    -- Convert declared type and add to environment
    declaredType <- convertMType typeAst

    -- Restore generic context
    modify $ \ctx -> ctx{icGenericTypes = savedGenericTypes}

    let typeScheme = TypeScheme freshGenerics declaredType
    addVarScheme name typeScheme

-- ============================================================================
-- Phase 2: Body Checking
-- ============================================================================

-- | Phase 2: Check a single definition body with full environment available
checkSingleDefinition :: AST KDefinition -> InferM ()
checkSingleDefinition ast =
  case unDefinition (extractSyntax ast) of
    SDefinition.ValDef varAst typeParams typeAst exprAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      checkPolymorphicValBody name typeParams typeAst exprAst
    SDefinition.DataDef _ _ ->
      -- Data definitions already processed in phase 1
      return ()
    SDefinition.TypeDef _ _ _ ->
      -- Type definitions already processed in phase 1
      return ()
 where
  checkPolymorphicValBody name typeParams typeAst exprAst = do
    checkValBody name typeParams typeAst exprAst
    finalCtx <- get
    let constraints = icConstraints finalCtx
        varMap = Map.map (\(TypeConstructorInfo _ variances _) -> variances) $ icTypeConstructors finalCtx

    solve varMap (icAssumptions finalCtx) constraints

  checkValBody name typeParams typeAst exprAst = do
    let paramNames =
          [ case unTypeVariable (extractSyntax paramAst) of
            SVariable.TypeVar paramName -> paramName
          | paramAst <- typeParams
          ]

    savedGenericTypes <- gets icGenericTypes

    -- Add type parameters to context
    freshGenerics <-
      mapM
        ( \paramName -> do
            g <- freshGenericsWithPrefix paramName
            addGenericType paramName g
            return g
        )
        paramNames

    declaredType <- convertMType typeAst
    inferredType <- inferExpr exprAst
    addConstraint $ Subtype inferredType declaredType

    -- Restore generic context
    modify $ \ctx -> ctx{icGenericTypes = savedGenericTypes}

-- ============================================================================
-- Constructor Definition Inference
-- ============================================================================

-- | Simplified constructor definition that only adds to type environment
inferConstructorDefSimplified :: T.Text -> SDefinition.ConstructorDef AST -> InferM ()
inferConstructorDefSimplified dataName (SDefinition.ConstructorDef ctorNameAst typeParams fullTypeAst) = do
  let SVariable.Var ctorName = unVariable (extractSyntax ctorNameAst)
      constructorTypeParamNames =
        [ case unTypeVariable (extractSyntax paramAst) of
          SVariable.TypeVar paramName -> paramName
        | paramAst <- typeParams
        ]

  -- Save and add generic types to context
  savedGenericTypes <- gets icGenericTypes
  freshGenerics <-
    mapM
      ( \paramName -> do
          g <- freshGenericsWithPrefix paramName
          addGenericType paramName g
          return g
      )
      constructorTypeParamNames

  -- Convert constructor type and add to environment
  fullType <- convertMType fullTypeAst
  let typeScheme = TypeScheme freshGenerics fullType
  addVarScheme ctorName typeScheme

  -- Restore generic context
  modify $ \ctx -> ctx { icGenericTypes = savedGenericTypes }

inferConstructorDef :: T.Text -> SDefinition.ConstructorDef AST -> InferM TypeConstructorInfo
inferConstructorDef dataName (SDefinition.ConstructorDef ctorNameAst typeParams fullTypeAst) = do
  let SVariable.Var ctorName = unVariable (extractSyntax ctorNameAst)

  -- It is important that there is distinction between constructor type param & type parm.
  -- e. g. `Cons<T>: (x : T, y : number) => Typ<T, number>`
  -- Here, `<T, number> of Typ<T, number>` are type parameters, `<T>` of `Cons<T>` is a constructor type parameters.

  let constructorTypeParamNames =
        [ case unTypeVariable (extractSyntax paramAst) of
          SVariable.TypeVar paramName -> paramName
        | paramAst <- typeParams
        ]

  -- Save
  savedGenericTypes <- gets icGenericTypes

  -- Add generic types to the context
  freshGenerics <-
    mapM
      ( \paramName -> do
          g <- freshGenericsWithPrefix paramName
          addGenericType paramName g
          return g
      )
      constructorTypeParamNames

  -- Convert fullType  i.e. `Cons<T>: (x : T, y : number) => Typ<T>  become (Generic T, number) => Typ<Generic T>
  fullType <- convertMType fullTypeAst

  -- Restore
  modify $ \ctx ->
    ctx
      { icGenericTypes = savedGenericTypes
      }

  let
    fullTypeMeta = extractMetadata fullTypeAst

  case unfoldFunctionType fullType of
    Just (argTypes, retType) -> do
      case retType of
        TApplication dn typeParams | dn == dataName -> do
          let tciKind = length typeParams
          -- Use placeholder variances (Bivariant) - will be solved later
          let tciVariance = replicate tciKind Bivariant
          let typeScheme = TypeScheme freshGenerics fullType
          addVarScheme ctorName typeScheme
          -- traceM $ "Using placeholder variances for " <> T.unpack dn <> ": " <> show tciVariance
          return TypeConstructorInfo{tciKind = tciKind, tciVariance = tciVariance, tciConstructors = [ctorName]}
        TVar (TypeVar dn) | dn == dataName -> do
          -- Zero-argument type constructor case
          let tciKind = 0
          let tciVariance = []
          let typeScheme = TypeScheme freshGenerics fullType
          addVarScheme ctorName typeScheme
          return TypeConstructorInfo{tciKind = tciKind, tciVariance = tciVariance, tciConstructors = [ctorName]}
        _ -> throwError $ TypeMismatch (Just fullTypeMeta) (TVar (TypeVar dataName)) retType
    Nothing ->
      throwError $ SomeTypeError $ "Constructor type must be a function, got: " <> formatType fullType
 where
  unfoldFunctionType :: Type -> Maybe ([Type], Type)
  unfoldFunctionType (TFunction argTypes returnType) = Just (argTypes, returnType)
  unfoldFunctionType nonFunctionType = Nothing

-- ============================================================================
-- Expression Inference
-- ============================================================================

inferExpr :: AST KExpr -> InferM Type
inferExpr ast = do
  let meta = extractMetadata ast
  case unExpr (extractSyntax ast) of
    EVar varAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      ctx <- get
      case Map.lookup name (icTypeEnv ctx) of
        Just scheme -> instantiatePolymorphicType scheme
        Nothing -> throwError $ UnboundTypeVariable (Just meta) name
    ELiteral litAst -> return $ convertLiteral litAst
    ELambda params bodyAst -> do
      savedEnv <- gets icTypeEnv
      paramTypes <- mapM inferLambdaParam params -- possible to introduce new var
      bodyType <- inferExpr bodyAst
      -- Restore type environment
      modify $ \ctx -> ctx{icTypeEnv = savedEnv}
      return $ TFunction paramTypes bodyType
    EApply funcAst argAsts -> do
      funcType <- inferExpr funcAst
      argTypes <- mapM inferExpr argAsts
      resultType <- TVar <$> freshVar
      addConstraint $ Subtype funcType (TFunction argTypes resultType)
      return resultType
    EIf condAst thenAst elseAst -> do
      condType <- inferExpr condAst
      addConstraint $ Subtype condType TBool
      thenType <- inferExpr thenAst
      elseType <- inferExpr elseAst
      resultType <- TVar <$> freshVar
      addConstraint $ Subtype thenType resultType
      addConstraint $ Subtype elseType resultType
      return resultType
    EBinOp opAst leftAst rightAst -> do
      leftType <- inferExpr leftAst
      rightType <- inferExpr rightAst
      inferBinOp (unBinOp (extractSyntax opAst)) leftType rightType
    EBlock letAsts exprAst -> do
      -- Save current type environment
      savedEnv <- gets icTypeEnv
      mapM_ inferLet letAsts -- possible to introduce new var
      result <- inferExpr exprAst
      -- Restore type environment
      modify $ \ctx -> ctx{icTypeEnv = savedEnv}
      return result
    EMatch scrutinees cases -> do
      scrutineeTypes <- mapM inferExpr scrutinees
      resultType <- TVar <$> freshVar

      mapM_ (inferMatchCase scrutineeTypes resultType) cases

      -- Simple exhaustivity check
      checkExhaustivity scrutineeTypes cases

      return resultType
 where
  convertLiteral :: AST KLiteral -> Type
  convertLiteral ast =
    case unLiteral (extractSyntax ast) of
      SLiteral.NumberLiteral n -> TLiteral (LNumber n)
      SLiteral.IntLiteral i -> TLiteral (LNumber (fromIntegral i))
      SLiteral.BoolLiteral b -> TLiteral (LBool b)
      SLiteral.StringLiteral s -> TLiteral (LString s)

  inferLambdaParam (patAst, mTypeAst) = do
    paramType <- case mTypeAst of
      Just typeAst -> convertMType typeAst
      Nothing -> TVar <$> freshVar
    case unPattern (extractSyntax patAst) of
      SPattern.PVar varAst -> do
        let SVariable.Var name = unVariable (extractSyntax varAst)
        addVar name paramType
        return paramType
      SPattern.PWildcard ->
        return paramType
      _ -> throwError $ SomeTypeError "Complex patterns not yet supported in lambda"

  inferBinOp :: SBinOp.BinOp f a -> Type -> Type -> InferM Type
  inferBinOp op leftType rightType = case op of
    SBinOp.Add -> numericBinOp
    SBinOp.Sub -> numericBinOp
    SBinOp.Mul -> numericBinOp
    SBinOp.Div -> numericBinOp
    SBinOp.Lt -> comparisonOp
    SBinOp.Gt -> comparisonOp
    SBinOp.Eq -> do
      unifiedType <- TVar <$> freshVar
      addConstraint $ Subtype leftType unifiedType
      addConstraint $ Subtype rightType unifiedType
      return TBool
   where
    numericBinOp = do
      addConstraint $ Subtype leftType TNumber
      addConstraint $ Subtype rightType TNumber
      return TNumber
    comparisonOp = do
      addConstraint $ Subtype leftType TNumber
      addConstraint $ Subtype rightType TNumber
      return TBool

  inferLet letAst = case unLet (extractSyntax letAst) of
    Let patAst mTypeAst exprAst -> do
      exprType <- inferExpr exprAst
      declaredType <- case mTypeAst of
        Just typeAst -> do
          dt <- convertMType typeAst
          addConstraint $ Subtype exprType dt
          return dt
        Nothing -> return exprType
      case unPattern (extractSyntax patAst) of
        SPattern.PVar varAst -> do
          let SVariable.Var name = unVariable (extractSyntax varAst)
          addVar name declaredType
        SPattern.PWildcard ->
          return ()
        _ -> throwError $ SomeTypeError "Complex patterns not yet supported in let"

-- NOTE: After this line, the code may be incomplete or not fully functional.

-- ============================================================================
-- Pattern Matching
-- ============================================================================

inferMatchCase :: [Type] -> Type -> (List (AST KPattern, Maybe (AST KType)), AST KExpr) -> InferM ()
inferMatchCase scrutineeTypes resultType (patterns, resultExpr) = do
  savedEnv <- gets icTypeEnv

  zipWithM_ inferPattern patterns scrutineeTypes
  caseResultType <- inferExpr resultExpr

  ctx <- get
  let resultVars = typeVars caseResultType
  addConstraint $ Subtype caseResultType resultType

  modify $ \ctx -> ctx{icTypeEnv = savedEnv}
 where
  inferPattern (patAst, mTypeAst) scrutineeType = do
    declaredType <- case mTypeAst of
      Just typeAst -> do
        dt <- convertMType typeAst
        addConstraint $ Subtype scrutineeType dt
        return dt
      Nothing -> return scrutineeType
    processPattern patAst declaredType

  processPattern patAst expectedType = case unPattern (extractSyntax patAst) of
    SPattern.PVar varAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      addVar name expectedType
    SPattern.PWildcard ->
      return ()
    SPattern.PLiteral litAst -> do
      let literalType = convertLiteral litAst
      addConstraint $ Subtype literalType expectedType
    SPattern.PCons constructorAst argPatterns -> do
      let SVariable.Var constructorName = unVariable (extractSyntax constructorAst)
      ctx <- get
      case Map.lookup constructorName (icTypeEnv ctx) of
        Just scheme -> do
          constructorType <- instantiatePolymorphicTypeWithScopeAwareness scheme
          let (argTypes, returnType) = unfoldFunctionType constructorType -- Constructor's arg & return type
          when (length argPatterns /= length argTypes) $
            error $
              "Constructor "
                ++ T.unpack constructorName
                ++ " expects "
                ++ show (length argTypes)
                ++ " arguments, got "
                ++ show (length argPatterns)

          -- Generate GADT assumptions by unifying constructor return type with scrutinee type
          addAssumption returnType expectedType

          zipWithM_ processPattern argPatterns argTypes
        Nothing ->
          error $ "Unknown constructor: " ++ T.unpack constructorName

  convertLiteral ast =
    case unLiteral (extractSyntax ast) of
      SLiteral.NumberLiteral n -> TLiteral (LNumber n)
      SLiteral.IntLiteral i -> TLiteral (LNumber (fromIntegral i))
      SLiteral.BoolLiteral b -> TLiteral (LBool b)
      SLiteral.StringLiteral s -> TLiteral (LString s)

  unfoldFunctionType (TFunction argTypes returnType) = (argTypes, returnType)
  unfoldFunctionType _ = ([], TUnknown)

-- ============================================================================
-- Exhaustivity Checking (Simplified)
-- ============================================================================

checkExhaustivity :: [Type] -> [(List (AST KPattern, Maybe (AST KType)), AST KExpr)] -> InferM ()
checkExhaustivity scrutineeTypes cases = do
  -- Extract patterns from each case
  let patterns = [map fst pats | (pats, _) <- cases]

  -- For each scrutinee position, check if patterns are exhaustive
  zipWithM_ checkPositionExhaustivity scrutineeTypes (transpose patterns)
 where
  -- Transpose list of lists safely
  transpose :: [[a]] -> [[a]]
  transpose [] = []
  transpose ([] : _) = []
  transpose rows = map head rows : transpose (map tail rows)

  -- Check if patterns at a specific position are exhaustive
  checkPositionExhaustivity :: Type -> [AST KPattern] -> InferM ()
  checkPositionExhaustivity scrutineeType patterns = do
    let hasWildcard = any isWildcardOrVar patterns
    if hasWildcard
      then return () -- Wildcard or variable covers everything
      else do
        -- Check constructor coverage
        ctx <- get
        case scrutineeType of
          TVar (TypeVar typeName) -> do
            -- Look up possible constructors for this type
            case Map.lookup typeName (icTypeConstructors ctx) of
              Just tci -> do
                let expectedConstructors = Set.fromList (tciConstructors tci)
                let coveredConstructors = Set.fromList $ concatMap getConstructors patterns
                when (expectedConstructors /= coveredConstructors) $
                  addConstraint $
                    Subtype TUnknown TNever -- Force type error
              Nothing -> return () -- Unknown type, can't check
          _ -> return () -- Not a data type
  isWildcardOrVar :: AST KPattern -> Bool
  isWildcardOrVar pat = case unPattern (extractSyntax pat) of
    SPattern.PWildcard -> True
    SPattern.PVar _ -> True
    _ -> False

  getConstructors :: AST KPattern -> [T.Text]
  getConstructors pat = case unPattern (extractSyntax pat) of
    SPattern.PCons ctorAst _ ->
      let SVariable.Var name = unVariable (extractSyntax ctorAst)
       in [name]
    _ -> []
