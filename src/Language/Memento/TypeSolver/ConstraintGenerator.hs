{-# LANGUAGE OverloadedStrings #-}

-- | Pure constraint generation for the Memento type solver
-- Separates constraint generation logic from stateful type checking
module Language.Memento.TypeSolver.ConstraintGenerator
  ( ConstraintGenerationResult (..),
    ConstraintEnv (..),
    generateConstraints,
    generateExprConstraints,
    convertASTType,
    emptyConstraintEnv,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.Syntax
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
import Language.Memento.TypeSolver.Types

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Environment for constraint generation (immutable)
data ConstraintEnv = ConstraintEnv
  { ceTypeEnv :: Map.Map T.Text TypeScheme,           -- ^ Variable name to type scheme mapping
    ceTypeConstructors :: Map.Map T.Text TypeConstructorInfo, -- ^ Type constructor information
    ceConstructors :: Set.Set T.Text,                 -- ^ Available constructor names
    ceGenericTypes :: Map.Map T.Text T.Text,          -- ^ Generic type mappings
    ceVarCounter :: Int                               -- ^ Counter for fresh variable generation
  }
  deriving (Show, Eq)

-- | Result of constraint generation
data ConstraintGenerationResult = ConstraintGenerationResult
  { cgrConstraints :: ConstraintSet,                  -- ^ Generated constraints
    cgrInferredType :: Type,                          -- ^ Inferred type for the expression
    cgrNewVarCounter :: Int,                          -- ^ Updated variable counter
    cgrNewBindings :: Map.Map T.Text TypeScheme       -- ^ New variable bindings (for let/lambda)
  }
  deriving (Show, Eq)

-- | Empty constraint environment
emptyConstraintEnv :: ConstraintEnv
emptyConstraintEnv = ConstraintEnv
  { ceTypeEnv = Map.empty
  , ceTypeConstructors = Map.empty
  , ceConstructors = Set.empty
  , ceGenericTypes = Map.empty
  , ceVarCounter = 0
  }

-- ============================================================================
-- Core Constraint Generation
-- ============================================================================

-- | Generate constraints for a complete program
generateConstraints :: ConstraintEnv -> AST KProgram -> Either TypeError ConstraintGenerationResult
generateConstraints env ast = case unProgram (extractSyntax ast) of
  SProgram.Program declAsts -> 
    -- For now, just process the first expression-like declaration
    -- Full program constraint generation would need more sophisticated handling
    Right $ ConstraintGenerationResult
      { cgrConstraints = Set.empty
      , cgrInferredType = TUnknown
      , cgrNewVarCounter = ceVarCounter env
      , cgrNewBindings = Map.empty
      }

-- | Generate constraints for an expression
generateExprConstraints :: ConstraintEnv -> AST KExpr -> Either TypeError ConstraintGenerationResult
generateExprConstraints env ast = 
  let (constraints, inferredType, newCounter, newBindings) = inferExprPure env ast
  in Right $ ConstraintGenerationResult
    { cgrConstraints = constraints
    , cgrInferredType = inferredType
    , cgrNewVarCounter = newCounter
    , cgrNewBindings = newBindings
    }

-- ============================================================================
-- Pure Expression Inference
-- ============================================================================

-- | Pure expression inference that generates constraints
inferExprPure :: ConstraintEnv -> AST KExpr -> (ConstraintSet, Type, Int, Map.Map T.Text TypeScheme)
inferExprPure env ast = 
  let meta = extractMetadata ast
  in case unExpr (extractSyntax ast) of
    EVar varAst -> 
      let SVariable.Var name = unVariable (extractSyntax varAst)
      in case Map.lookup name (ceTypeEnv env) of
        Just scheme -> 
          let (instType, newCounter) = instantiatePolymorphicTypePure env scheme
          in (Set.empty, instType, newCounter, Map.empty)
        Nothing -> 
          -- Create a fresh variable for unbound names
          let (freshVar, newCounter) = freshVarPure env
          in (Set.empty, TVar freshVar, newCounter, Map.empty)
    
    ELiteral litAst -> 
      let literalType = convertLiteralPure litAst
      in (Set.empty, literalType, ceVarCounter env, Map.empty)
    
    ELambda params bodyAst ->
      let (paramTypes, paramBindings, counterAfterParams) = processLambdaParams env params
          envWithParams = env { ceTypeEnv = Map.union paramBindings (ceTypeEnv env), ceVarCounter = counterAfterParams }
          (bodyConstraints, bodyType, finalCounter, _) = inferExprPure envWithParams bodyAst
          functionType = TFunction paramTypes bodyType
      in (bodyConstraints, functionType, finalCounter, paramBindings)
    
    EApply funcAst argAsts ->
      let (funcConstraints, funcType, counterAfterFunc, funcBindings) = inferExprPure env funcAst
          envAfterFunc = env { ceVarCounter = counterAfterFunc }
          (argResults, finalCounter) = processArguments envAfterFunc argAsts
          argConstraints = Set.unions [cs | (cs, _, _, _) <- argResults]
          argTypes = [t | (_, t, _, _) <- argResults]
          (resultVar, counterAfterResult) = freshVarPure env { ceVarCounter = finalCounter }
          resultType = TVar resultVar
          applicationConstraint = Subtype funcType (TFunction argTypes resultType)
          allConstraints = Set.unions [funcConstraints, argConstraints, Set.singleton applicationConstraint]
      in (allConstraints, resultType, counterAfterResult, funcBindings)
    
    EIf condAst thenAst elseAst ->
      let (condConstraints, condType, counterAfterCond, condBindings) = inferExprPure env condAst
          envAfterCond = env { ceVarCounter = counterAfterCond }
          (thenConstraints, thenType, counterAfterThen, thenBindings) = inferExprPure envAfterCond thenAst
          envAfterThen = envAfterCond { ceVarCounter = counterAfterThen }
          (elseConstraints, elseType, counterAfterElse, elseBindings) = inferExprPure envAfterThen elseAst
          (resultVar, finalCounter) = freshVarPure env { ceVarCounter = counterAfterElse }
          resultType = TVar resultVar
          typeConstraints = Set.fromList
            [ Subtype condType TBool
            , Subtype thenType resultType
            , Subtype elseType resultType
            ]
          allConstraints = Set.unions [condConstraints, thenConstraints, elseConstraints, typeConstraints]
          allBindings = Map.unions [condBindings, thenBindings, elseBindings]
      in (allConstraints, resultType, finalCounter, allBindings)
    
    EBinOp opAst leftAst rightAst ->
      let (leftConstraints, leftType, counterAfterLeft, leftBindings) = inferExprPure env leftAst
          envAfterLeft = env { ceVarCounter = counterAfterLeft }
          (rightConstraints, rightType, counterAfterRight, rightBindings) = inferExprPure envAfterLeft rightAst
          op = unBinOp (extractSyntax opAst)
          (opConstraints, resultType) = inferBinOpPure op leftType rightType
          allConstraints = Set.unions [leftConstraints, rightConstraints, opConstraints]
          allBindings = Map.union leftBindings rightBindings
      in (allConstraints, resultType, counterAfterRight, allBindings)
    
    EBlock letAsts exprAst ->
      let (letConstraints, letBindings, counterAfterLets) = processLets env letAsts
          envWithLets = env { ceTypeEnv = Map.union letBindings (ceTypeEnv env), ceVarCounter = counterAfterLets }
          (exprConstraints, exprType, finalCounter, exprBindings) = inferExprPure envWithLets exprAst
          allConstraints = Set.union letConstraints exprConstraints
          allBindings = Map.union letBindings exprBindings
      in (allConstraints, exprType, finalCounter, allBindings)
    
    EMatch scrutinees cases ->
      -- Simplified match handling for now
      let (scrutineeResults, counterAfterScrutinees) = processArguments env scrutinees
          scrutineeConstraints = Set.unions [cs | (cs, _, _, _) <- scrutineeResults]
          scrutineeTypes = [t | (_, t, _, _) <- scrutineeResults]
          (resultVar, finalCounter) = freshVarPure env { ceVarCounter = counterAfterScrutinees }
          resultType = TVar resultVar
          -- TODO: Process match cases properly
          allConstraints = scrutineeConstraints
      in (allConstraints, resultType, finalCounter, Map.empty)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Generate fresh type variable
freshVarPure :: ConstraintEnv -> (TypeVar, Int)
freshVarPure env = 
  let counter = ceVarCounter env
      newVar = TypeVar $ T.pack ("t" ++ show counter)
  in (newVar, counter + 1)

-- | Process lambda parameters
processLambdaParams :: ConstraintEnv -> [(AST KPattern, Maybe (AST KType))] -> ([Type], Map.Map T.Text TypeScheme, Int)
processLambdaParams env params = 
  let (types, bindings, counter) = foldl processParam ([], Map.empty, ceVarCounter env) params
  in (reverse types, bindings, counter)
  where
    processParam (accTypes, accBindings, currentCounter) (patAst, mTypeAst) =
      let paramType = case mTypeAst of
            Just typeAst -> case convertASTType env typeAst of
              Right t -> t
              Left _ -> TUnknown  -- Fallback on error
            Nothing -> 
              let (freshVar, newCounter) = freshVarPure env { ceVarCounter = currentCounter }
              in TVar freshVar
          newCounter = case mTypeAst of
            Nothing -> currentCounter + 1
            Just _ -> currentCounter
      in case unPattern (extractSyntax patAst) of
        SPattern.PVar varAst ->
          let SVariable.Var name = unVariable (extractSyntax varAst)
              newBindings = Map.insert name (TypeScheme [] paramType) accBindings
          in (paramType : accTypes, newBindings, newCounter)
        SPattern.PWildcard ->
          (paramType : accTypes, accBindings, newCounter)
        _ -> (paramType : accTypes, accBindings, newCounter)  -- TODO: Handle complex patterns

-- | Process argument expressions
processArguments :: ConstraintEnv -> [AST KExpr] -> ([(ConstraintSet, Type, Int, Map.Map T.Text TypeScheme)], Int)
processArguments env args = 
  let (results, finalCounter) = foldl processArg ([], ceVarCounter env) args
  in (reverse results, finalCounter)
  where
    processArg (accResults, currentCounter) argAst =
      let envWithCounter = env { ceVarCounter = currentCounter }
          result@(_, _, newCounter, _) = inferExprPure envWithCounter argAst
      in (result : accResults, newCounter)

-- | Process let bindings
processLets :: ConstraintEnv -> [AST KLet] -> (ConstraintSet, Map.Map T.Text TypeScheme, Int)
processLets env lets = 
  let (allConstraints, allBindings, finalCounter) = foldl processLet (Set.empty, Map.empty, ceVarCounter env) lets
  in (allConstraints, allBindings, finalCounter)
  where
    processLet (accConstraints, accBindings, currentCounter) letAst =
      case unLet (extractSyntax letAst) of
        Let patAst mTypeAst exprAst ->
          let envWithCounter = env { ceVarCounter = currentCounter, ceTypeEnv = Map.union accBindings (ceTypeEnv env) }
              (exprConstraints, exprType, counterAfterExpr, _) = inferExprPure envWithCounter exprAst
              (declaredType, typeConstraints, finalCounter) = case mTypeAst of
                Just typeAst -> case convertASTType envWithCounter typeAst of
                  Right dt -> (dt, Set.singleton (Subtype exprType dt), counterAfterExpr)
                  Left _ -> (exprType, Set.empty, counterAfterExpr)
                Nothing -> (exprType, Set.empty, counterAfterExpr)
              newBindings = case unPattern (extractSyntax patAst) of
                SPattern.PVar varAst ->
                  let SVariable.Var name = unVariable (extractSyntax varAst)
                  in Map.insert name (TypeScheme [] declaredType) accBindings
                _ -> accBindings  -- TODO: Handle complex patterns
              newConstraints = Set.unions [accConstraints, exprConstraints, typeConstraints]
          in (newConstraints, newBindings, finalCounter)

-- | Pure binary operation inference
inferBinOpPure :: SBinOp.BinOp f a -> Type -> Type -> (ConstraintSet, Type)
inferBinOpPure op leftType rightType = case op of
  SBinOp.Add -> (numericConstraints, TNumber)
  SBinOp.Sub -> (numericConstraints, TNumber)
  SBinOp.Mul -> (numericConstraints, TNumber)
  SBinOp.Div -> (numericConstraints, TNumber)
  SBinOp.Lt -> (numericConstraints, TBool)
  SBinOp.Gt -> (numericConstraints, TBool)
  SBinOp.Eq -> (equalityConstraints, TBool)
  where
    numericConstraints = Set.fromList [Subtype leftType TNumber, Subtype rightType TNumber]
    equalityConstraints = Set.empty  -- For equality, we don't need specific type constraints

-- | Convert literal to type
convertLiteralPure :: AST KLiteral -> Type
convertLiteralPure ast = case unLiteral (extractSyntax ast) of
  SLiteral.NumberLiteral n -> TLiteral (LNumber n)
  SLiteral.IntLiteral i -> TLiteral (LNumber (fromIntegral i))
  SLiteral.BoolLiteral b -> TLiteral (LBool b)
  SLiteral.StringLiteral s -> TLiteral (LString s)

-- | Instantiate polymorphic type without monadic context
instantiatePolymorphicTypePure :: ConstraintEnv -> TypeScheme -> (Type, Int)
instantiatePolymorphicTypePure env (TypeScheme [] typ) = (typ, ceVarCounter env)
instantiatePolymorphicTypePure env (TypeScheme vars typ) = 
  let (freshVars, newCounter) = generateFreshVars env vars
      substitution = Map.fromList (zip vars (map TVar freshVars))
      instantiatedType = substGenericsPure substitution typ
  in (instantiatedType, newCounter)
  where
    generateFreshVars currentEnv [] = ([], ceVarCounter currentEnv)
    generateFreshVars currentEnv (_:rest) = 
      let (freshVar, counterAfterOne) = freshVarPure currentEnv
          (restVars, finalCounter) = generateFreshVars (currentEnv { ceVarCounter = counterAfterOne }) rest
      in (freshVar : restVars, finalCounter)

-- | Pure generic substitution
substGenericsPure :: Map.Map T.Text Type -> Type -> Type
substGenericsPure subst typ = case typ of
  TVar v -> typ
  TGeneric g -> Map.findWithDefault (TGeneric g) g subst
  TNumber -> typ
  TBool -> typ
  TString -> typ
  TNever -> typ
  TUnknown -> typ
  TLiteral l -> typ
  TFunction argTypes retType -> 
    TFunction (map (substGenericsPure subst) argTypes) (substGenericsPure subst retType)
  TUnion types -> TUnion (Set.map (substGenericsPure subst) types)
  TIntersection types -> TIntersection (Set.map (substGenericsPure subst) types)
  TApplication name argTypes -> 
    TApplication name (map (substGenericsPure subst) argTypes)

-- ============================================================================
-- Type Conversion
-- ============================================================================

-- | Convert AST type to solver type (pure version)
convertASTType :: ConstraintEnv -> AST KType -> Either TypeError Type
convertASTType env ast = 
  let meta = extractMetadata ast
  in case unMType (extractSyntax ast) of
    SMType.TNumber -> Right TNumber
    SMType.TInt -> Right TNumber
    SMType.TBool -> Right TBool
    SMType.TString -> Right TString
    SMType.TNever -> Right TNever
    SMType.TUnknown -> Right TUnknown
    
    SMType.TVar varAst -> do
      let SVariable.TypeVar name = unTypeVariable (extractSyntax varAst)
      case name of
        "string" -> Right TString
        "number" -> Right TNumber
        "bool" -> Right TBool
        "never" -> Right TNever
        "unknown" -> Right TUnknown
        _ -> case Map.lookup name (ceTypeConstructors env) of
          Just (TypeConstructorInfo k _ _) | k == 0 -> Right $ TApplication name []
          Just _ -> Left $ UnboundTypeVariable (Just meta) name  -- Missing type arguments
          Nothing -> case Map.lookup name (ceGenericTypes env) of
            Just g -> Right $ TGeneric g
            Nothing -> Left $ UnboundTypeVariable (Just meta) name
    
    SMType.TLiteral litAst -> Right $ convertLiteralPure litAst
    
    SMType.TFunction params retType -> do
      paramTypes <- mapM (convertASTType env . snd) params
      returnType <- convertASTType env retType
      return $ TFunction paramTypes returnType
    
    SMType.TUnion types -> do
      convertedTypes <- mapM (convertASTType env) types
      return $ mkUnion convertedTypes
    
    SMType.TIntersection types -> do
      convertedTypes <- mapM (convertASTType env) types
      return $ mkIntersection convertedTypes
    
    SMType.TApplication baseAst argAsts -> do
      let SVariable.TypeVar baseName = unTypeVariable (extractSyntax baseAst)
      case Map.lookup baseName (ceTypeConstructors env) of
        Just (TypeConstructorInfo k _ _) -> do
          if k /= length argAsts
            then Left $ SomeTypeError $ 
              "Type constructor " <> baseName <> " expects " <> T.pack (show k) <> 
              " arguments, got " <> T.pack (show (length argAsts))
            else do
              argTypes <- mapM (convertASTType env) argAsts
              return $ TApplication baseName argTypes
        Nothing -> Left $ UnboundTypeVariable (Just meta) baseName