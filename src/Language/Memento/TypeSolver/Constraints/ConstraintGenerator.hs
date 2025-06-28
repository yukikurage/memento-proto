{-# LANGUAGE OverloadedStrings #-}

-- | Pure constraint generation for the Memento type solver
-- Separates constraint generation logic from stateful type checking
module Language.Memento.TypeSolver.Constraints.ConstraintGenerator
  ( ConstraintGenerationResult (..),
    ConstraintEnv (..),
    generateConstraints,
    generateExprConstraints,
    convertASTType,
    emptyConstraintEnv,
  )
where

import Control.Monad (foldM)
import Control.Monad.Error.Class (MonadError, throwError)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace)
import Language.Memento.Syntax
import qualified Language.Memento.Syntax.BinOp as SBinOp
import qualified Language.Memento.Syntax.Definition as SDefinition
import Language.Memento.Syntax.Expr (Expr (..), Let (..))
import Language.Memento.Syntax.Pattern (Pattern (..))
import qualified Language.Memento.Syntax.Literal as SLiteral
import qualified Language.Memento.Syntax.MType as SMType
import Language.Memento.Syntax.Metadata (Metadata (..))
import qualified Language.Memento.Syntax.Pattern as SPattern
import qualified Language.Memento.Syntax.Program as SProgram
import Language.Memento.Syntax.Tag (KBinOp, KDefinition, KExpr, KLet, KLiteral, KPattern, KProgram, KType, KTypeVariable, KVariable)
import qualified Language.Memento.Syntax.Variable as SVariable
import Language.Memento.TypeSolver.Core.Types

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
generateExprConstraints env ast = do
  (constraints, inferredType, newCounter, newBindings) <- inferExprMonadic env ast
  return $ ConstraintGenerationResult
    { cgrConstraints = constraints
    , cgrInferredType = inferredType
    , cgrNewVarCounter = newCounter
    , cgrNewBindings = newBindings
    }

-- ============================================================================
-- Monadic Expression Inference
-- ============================================================================

-- | Monadic expression inference that generates constraints and can report errors
inferExprMonadic :: ConstraintEnv -> AST KExpr -> Either TypeError (ConstraintSet, Type, Int, Map.Map T.Text TypeScheme)
inferExprMonadic env ast = 
  let meta = extractMetadata ast
  in case unExpr (extractSyntax ast) of
    EVar varAst -> 
      let SVariable.Var name = unVariable (extractSyntax varAst)
      in case Map.lookup name (ceTypeEnv env) of
        Just scheme -> 
          -- Debug: trace polymorphic instantiation
          -- let _ = trace ("=== Instantiating polymorphic var: " ++ T.unpack name ++ " with scheme: " ++ show scheme) ()
          let (instType, newCounter) = instantiatePolymorphicTypePure env scheme
              -- _ = trace ("=== Instantiated to: " ++ T.unpack (formatType instType)) ()
          in Right (Set.empty, instType, newCounter, Map.empty)
        Nothing -> 
          -- Report error for unbound variables
          Left $ UnboundVariable (Just meta) name
    
    ELiteral litAst -> 
      let literalType = convertLiteralPure litAst
      in Right (Set.empty, literalType, ceVarCounter env, Map.empty)
    
    ELambda params bodyAst -> do
      let (paramTypes, paramBindings, counterAfterParams) = processLambdaParams env params
          envWithParams = env { ceTypeEnv = Map.union paramBindings (ceTypeEnv env), ceVarCounter = counterAfterParams }
      (bodyConstraints, bodyType, finalCounter, _) <- inferExprMonadic envWithParams bodyAst
      let functionType = TFunction paramTypes bodyType
      return (bodyConstraints, functionType, finalCounter, paramBindings)
    
    EApply funcAst argAsts -> do
      (funcConstraints, funcType, counterAfterFunc, funcBindings) <- inferExprMonadic env funcAst
      let envAfterFunc = env { ceVarCounter = counterAfterFunc }
      (argResults, finalCounter) <- processArgumentsMonadic envAfterFunc argAsts
      let argConstraints = Set.unions [cs | (cs, _, _, _) <- argResults]
          argTypes = [t | (_, t, _, _) <- argResults]
          (resultVar, counterAfterResult) = freshVarPure env { ceVarCounter = finalCounter }
          resultType = TVar resultVar
          applicationConstraint = Subtype funcType (TFunction argTypes resultType)
          allConstraints = Set.unions [funcConstraints, argConstraints, Set.singleton applicationConstraint]
      return (allConstraints, resultType, counterAfterResult, funcBindings)
    
    EIf condAst thenAst elseAst -> do
      (condConstraints, condType, counterAfterCond, condBindings) <- inferExprMonadic env condAst
      let envAfterCond = env { ceVarCounter = counterAfterCond }
      (thenConstraints, thenType, counterAfterThen, thenBindings) <- inferExprMonadic envAfterCond thenAst
      let envAfterThen = envAfterCond { ceVarCounter = counterAfterThen }
      (elseConstraints, elseType, counterAfterElse, elseBindings) <- inferExprMonadic envAfterThen elseAst
      let (resultVar, finalCounter) = freshVarPure env { ceVarCounter = counterAfterElse }
          resultType = TVar resultVar
          typeConstraints = Set.fromList
            [ Subtype condType TBool
            , Subtype thenType resultType
            , Subtype elseType resultType
            ]
          allConstraints = Set.unions [condConstraints, thenConstraints, elseConstraints, typeConstraints]
          allBindings = Map.unions [condBindings, thenBindings, elseBindings]
      return (allConstraints, resultType, finalCounter, allBindings)
    
    EBinOp opAst leftAst rightAst -> do
      (leftConstraints, leftType, counterAfterLeft, leftBindings) <- inferExprMonadic env leftAst
      let envAfterLeft = env { ceVarCounter = counterAfterLeft }
      (rightConstraints, rightType, counterAfterRight, rightBindings) <- inferExprMonadic envAfterLeft rightAst
      let op = unBinOp (extractSyntax opAst)
          (opConstraints, resultType) = inferBinOpPure op leftType rightType
          allConstraints = Set.unions [leftConstraints, rightConstraints, opConstraints]
          allBindings = Map.union leftBindings rightBindings
      return (allConstraints, resultType, counterAfterRight, allBindings)
    
    EBlock letAsts exprAst -> do
      (letConstraints, letBindings, counterAfterLets) <- processLetsMonadic env letAsts
      let envWithLets = env { ceTypeEnv = Map.union letBindings (ceTypeEnv env), ceVarCounter = counterAfterLets }
      (exprConstraints, exprType, finalCounter, exprBindings) <- inferExprMonadic envWithLets exprAst
      let allConstraints = Set.union letConstraints exprConstraints
          allBindings = Map.union letBindings exprBindings
      return (allConstraints, exprType, finalCounter, allBindings)
    
    EMatch scrutinees cases -> do
      -- Generate constraints for scrutinees
      (scrutineeResults, counterAfterScrutinees) <- processArgumentsMonadic env scrutinees
      let scrutineeConstraints = Set.unions [cs | (cs, _, _, _) <- scrutineeResults]
          scrutineeTypes = [t | (_, t, _, _) <- scrutineeResults]
          (resultVar, counterAfterCases) = freshVarPure env { ceVarCounter = counterAfterScrutinees }
          resultType = TVar resultVar
      
      -- Process match cases and generate constraints
      (caseConstraints, finalCounter) <- processCases env { ceVarCounter = counterAfterCases } cases scrutineeTypes resultType
      let allConstraints = Set.union scrutineeConstraints caseConstraints
      return (allConstraints, resultType, finalCounter, Map.empty)

-- ============================================================================
-- Pattern Matching Support
-- ============================================================================

-- | Process pattern match cases and generate constraints
processCases :: ConstraintEnv -> [([(AST KPattern, Maybe (AST KType))], AST KExpr)] -> [Type] -> Type -> Either TypeError (ConstraintSet, Int)
processCases env cases scrutineeTypes resultType = do
  (allConstraints, finalCounter) <- foldM processCase (Set.empty, ceVarCounter env) cases
  return (allConstraints, finalCounter)
  where
    processCase :: (ConstraintSet, Int) -> ([(AST KPattern, Maybe (AST KType))], AST KExpr) -> Either TypeError (ConstraintSet, Int)
    processCase (accConstraints, currentCounter) (patternParams, exprAst) = do
      -- For now, handle only single pattern cases (lambda-style matching)
      case patternParams of
        [(patAst, _)] -> do
          -- Generate pattern constraints against scrutinee types
          (patConstraints, patBindings, counterAfterPat) <- processPatternConstraints env { ceVarCounter = currentCounter } patAst scrutineeTypes
          
          -- Generate expression constraints with pattern bindings in scope
          let envWithPattern = env { ceTypeEnv = Map.union patBindings (ceTypeEnv env), ceVarCounter = counterAfterPat }
          (exprConstraints, exprType, counterAfterExpr, _) <- inferExprMonadic envWithPattern exprAst
          
          -- Generate constraint that expression type matches result type
          let resultConstraint = Subtype exprType resultType
              caseConstraints = Set.unions [accConstraints, patConstraints, exprConstraints, Set.singleton resultConstraint]
          
          return (caseConstraints, counterAfterExpr)
        _ -> 
          -- Multiple patterns in one case not yet supported
          Left $ SomeTypeError "Multiple patterns per case not yet supported"

-- | Process pattern constraints against scrutinee types
processPatternConstraints :: ConstraintEnv -> AST KPattern -> [Type] -> Either TypeError (ConstraintSet, Map.Map T.Text TypeScheme, Int)
processPatternConstraints env patAst scrutineeTypes = 
  case unPattern (extractSyntax patAst) of
    SPattern.PVar varAst -> do
      -- Variable pattern: bind variable to scrutinee type
      let SVariable.Var name = unVariable (extractSyntax varAst)
          -- For multiple scrutinees, create union type
          patternType = case scrutineeTypes of
            [] -> TUnknown
            [t] -> t
            ts -> TUnion (Set.fromList ts)
          binding = Map.singleton name (TypeScheme [] patternType)
      return (Set.empty, binding, ceVarCounter env)
    
    SPattern.PWildcard -> 
      -- Wildcard pattern: no constraints or bindings
      return (Set.empty, Map.empty, ceVarCounter env)
    
    SPattern.PLiteral litAst -> do
      -- Literal pattern: generate equality constraint
      let literalType = convertLiteralPure litAst
          constraints = Set.fromList [Subtype scrutineeType literalType | scrutineeType <- scrutineeTypes]
      return (constraints, Map.empty, ceVarCounter env)
    
    SPattern.PCons ctorAst argPats -> do
      -- Constructor pattern: handle constructor application
      let SVariable.Var ctorName = unVariable (extractSyntax ctorAst)
      case Set.member ctorName (ceConstructors env) of
        False -> Left $ UnboundVariable (Just $ extractMetadata patAst) ctorName
        True -> do
          -- Generate fresh variable for constructor result type
          let (ctorResultVar, counterAfterCtor) = freshVarPure env
              ctorResultType = TVar ctorResultVar
              
          -- Process argument patterns
          (argConstraints, argBindings, finalCounter) <- processArgumentPatterns env { ceVarCounter = counterAfterCtor } argPats
          
          -- Create constructor application constraint
          let ctorConstraints = Set.fromList [Subtype scrutineeType ctorResultType | scrutineeType <- scrutineeTypes]
              allConstraints = Set.union argConstraints ctorConstraints
              
          return (allConstraints, argBindings, finalCounter)

-- | Process argument patterns for constructor patterns
processArgumentPatterns :: ConstraintEnv -> [AST KPattern] -> Either TypeError (ConstraintSet, Map.Map T.Text TypeScheme, Int)
processArgumentPatterns env argPats = do
  (allConstraints, allBindings, finalCounter) <- foldM processArgPattern (Set.empty, Map.empty, ceVarCounter env) argPats
  return (allConstraints, allBindings, finalCounter)
  where
    processArgPattern :: (ConstraintSet, Map.Map T.Text TypeScheme, Int) -> AST KPattern -> Either TypeError (ConstraintSet, Map.Map T.Text TypeScheme, Int)
    processArgPattern (accConstraints, accBindings, currentCounter) argPat = do
      -- Generate fresh type variable for argument
      let (argVar, counterAfterArg) = freshVarPure env { ceVarCounter = currentCounter }
          argType = TVar argVar
      
      -- Process argument pattern
      (argConstraints, argBindings, finalCounter) <- processPatternConstraints env { ceVarCounter = counterAfterArg } argPat [argType]
      
      let combinedConstraints = Set.union accConstraints argConstraints
          combinedBindings = Map.union accBindings argBindings
          
      return (combinedConstraints, combinedBindings, finalCounter)

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
  -- let _ = trace ("=== Processing lambda params: " ++ show (length params) ++ " parameters") ()
  --     _ = trace ("=== Environment generic types: " ++ show (ceGenericTypes env)) ()
  let (types, bindings, counter) = foldl processParam ([], Map.empty, ceVarCounter env) params
      -- _ = trace ("=== Processed param types: " ++ show (map (T.unpack . formatType) (reverse types))) ()
      -- _ = trace ("=== Processed param bindings: " ++ show (Map.keys bindings)) ()
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
        SPattern.PLiteral litAst ->
          -- Literal patterns require type matching
          (paramType : accTypes, accBindings, newCounter)
        SPattern.PCons ctorAst argPats ->
          -- Constructor patterns need destructuring
          (paramType : accTypes, accBindings, newCounter)

-- | Process argument expressions (monadic version)
processArgumentsMonadic :: ConstraintEnv -> [AST KExpr] -> Either TypeError ([(ConstraintSet, Type, Int, Map.Map T.Text TypeScheme)], Int)
processArgumentsMonadic env args = do
  (results, finalCounter) <- foldM processArg ([], ceVarCounter env) args
  return (reverse results, finalCounter)
  where
    processArg (accResults, currentCounter) argAst = do
      let envWithCounter = env { ceVarCounter = currentCounter }
      result@(_, _, newCounter, _) <- inferExprMonadic envWithCounter argAst
      return (result : accResults, newCounter)

-- | Process argument expressions (pure version - kept for compatibility)
processArguments :: ConstraintEnv -> [AST KExpr] -> ([(ConstraintSet, Type, Int, Map.Map T.Text TypeScheme)], Int)
processArguments env args = 
  case processArgumentsMonadic env args of
    Right result -> result
    Left _ -> ([], ceVarCounter env)  -- Fallback on error

-- | Process let bindings (monadic version)
processLetsMonadic :: ConstraintEnv -> [AST KLet] -> Either TypeError (ConstraintSet, Map.Map T.Text TypeScheme, Int)
processLetsMonadic env lets = do
  (allConstraints, allBindings, finalCounter) <- foldM processLet (Set.empty, Map.empty, ceVarCounter env) lets
  return (allConstraints, allBindings, finalCounter)
  where
    processLet (accConstraints, accBindings, currentCounter) letAst = do
      case unLet (extractSyntax letAst) of
        Let patAst mTypeAst exprAst -> do
          let envWithCounter = env { ceVarCounter = currentCounter, ceTypeEnv = Map.union accBindings (ceTypeEnv env) }
          (exprConstraints, exprType, counterAfterExpr, _) <- inferExprMonadic envWithCounter exprAst
          (declaredType, typeConstraints, finalCounter) <- case mTypeAst of
            Just typeAst -> do
              case convertASTType envWithCounter typeAst of
                Right dt -> return (dt, Set.singleton (Subtype exprType dt), counterAfterExpr)
                Left err -> Left err
            Nothing -> return (exprType, Set.empty, counterAfterExpr)
          let newBindings = case unPattern (extractSyntax patAst) of
                SPattern.PVar varAst ->
                  let SVariable.Var name = unVariable (extractSyntax varAst)
                  in Map.insert name (TypeScheme [] declaredType) accBindings
                _ -> accBindings  -- TODO: Handle complex patterns
              newConstraints = Set.unions [accConstraints, exprConstraints, typeConstraints]
          return (newConstraints, newBindings, finalCounter)

-- | Process let bindings (pure version - kept for compatibility)
processLets :: ConstraintEnv -> [AST KLet] -> (ConstraintSet, Map.Map T.Text TypeScheme, Int)
processLets env lets = 
  case processLetsMonadic env lets of
    Right result -> result
    Left _ -> (Set.empty, Map.empty, ceVarCounter env)  -- Fallback on error

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