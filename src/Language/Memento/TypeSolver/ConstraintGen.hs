{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constraint generation for the Memento type solver
module Language.Memento.TypeSolver.ConstraintGen (
  inferProgram,
  typeCheckAST,
) where

import Control.Monad (when, zipWithM, zipWithM_)
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Base (List)
import Text.Megaparsec (SourcePos)

import Debug.Trace (traceM)
import Language.Memento.Data.HFix (HFix (..), unHFix)
import Language.Memento.Data.HProduct (type (:*:) (..))
import Language.Memento.Syntax
import qualified Language.Memento.Syntax.BinOp as SBinOp
import qualified Language.Memento.Syntax.Definition as SDefinition
import Language.Memento.Syntax.Expr (Expr (..), Let (..))
import qualified Language.Memento.Syntax.Literal as SLiteral
import qualified Language.Memento.Syntax.MType as SMType
import Language.Memento.Syntax.Metadata (Metadata (..))
import qualified Language.Memento.Syntax.Pattern as SPattern
import qualified Language.Memento.Syntax.Program as SProgram
import Language.Memento.Syntax.Tag (KBinOp, KDefinition, KExpr, KLet, KLiteral, KPattern, KProgram, KType, KVariable)
import qualified Language.Memento.Syntax.Variable as SVariable
import Language.Memento.TypeSolver.Solver (solve)
import Language.Memento.TypeSolver.Types

-- ============================================================================
-- Core Types
-- ============================================================================

type VarianceMap = [Variance]

data InferContext = InferContext
  { icVarCounter :: Int
  , icConstraints :: ConstraintSet
  , icTypeEnv :: Map.Map T.Text TypeScheme
  , icTypeConstructors :: Map.Map T.Text TypeConstructorInfo
  , icConstructors :: Set.Set T.Text
  , icVariances :: TypeConstructorVariances
  , icExistentialVars :: Set.Set TypeVar
  }
  deriving (Show)

data TypeConstructorInfo = TypeConstructorInfo
  { tciKind :: Int
  , tciVariance :: VarianceMap
  , tciConstructors :: [T.Text]
  }
  deriving (Show, Eq)

type InferM = State InferContext

-- ============================================================================
-- Main Entry Points
-- ============================================================================

typeCheckAST :: AST KProgram -> Either String (Map.Map T.Text TypeScheme)
typeCheckAST = inferProgram

inferProgram :: AST KProgram -> Either String (Map.Map T.Text TypeScheme)
inferProgram ast = do
  let initialCtx =
        InferContext
          { icVarCounter = 0
          , icConstraints = Set.empty
          , icTypeEnv = Map.empty
          , icTypeConstructors = Map.empty
          , icConstructors = Set.empty
          , icVariances = Map.empty
          , icExistentialVars = Set.empty
          }
      ((), finalCtx) = runState (inferProgramM ast) initialCtx
      constraints = icConstraints finalCtx
      filteredConstraints = constraints

  case solve (icVariances finalCtx) filteredConstraints of
    Success ->
      Right (icTypeEnv finalCtx)
    Contradiction msg ->
      Left $ "Type error: " ++ msg ++ "\n" ++ formatConstraintSet filteredConstraints
 where
  -- Process the program
  inferProgramM :: AST KProgram -> InferM ()
  inferProgramM ast =
    case unProgram (extractSyntax ast) of
      SProgram.Program declAsts -> mapM_ inferDecl declAsts

  -- Check if constraint contains generic types
  constraintContainsGeneric :: Constraint -> Bool
  constraintContainsGeneric (Subtype t1 t2) =
    typeIsPurelyGeneric t1 && typeIsPurelyGeneric t2
   where
    typeIsPurelyGeneric t = typeContainsGeneric t && not (containsVar t)

    typeContainsGeneric (TGeneric _) = True
    typeContainsGeneric (TFunction args ret) = any typeContainsGeneric args || typeContainsGeneric ret
    typeContainsGeneric (TUnion ts) = any typeContainsGeneric (Set.toList ts)
    typeContainsGeneric (TIntersection ts) = any typeContainsGeneric (Set.toList ts)
    typeContainsGeneric (TApplication _ args) = any typeContainsGeneric args
    typeContainsGeneric _ = False

-- ============================================================================
-- Core Inference Operations
-- ============================================================================

freshVar :: InferM TypeVar
freshVar = do
  ctx <- get
  let counter = icVarCounter ctx
  put ctx{icVarCounter = counter + 1}
  return $ TypeVar $ T.pack ("t" ++ show counter)

addConstraint :: Constraint -> InferM ()
addConstraint c = modify $ \ctx -> ctx{icConstraints = Set.insert c (icConstraints ctx)}

addVar :: T.Text -> Type -> InferM ()
addVar name typ = addVarScheme name (TypeScheme [] typ)

addVarScheme :: T.Text -> TypeScheme -> InferM ()
addVarScheme name scheme = modify $ \ctx -> ctx{icTypeEnv = Map.insert name scheme (icTypeEnv ctx)}

addConstructor :: T.Text -> InferM ()
addConstructor name = modify $ \ctx -> ctx{icConstructors = Set.insert name (icConstructors ctx)}

addExistentialVar :: TypeVar -> InferM ()
addExistentialVar var = modify $ \ctx -> ctx{icExistentialVars = Set.insert var (icExistentialVars ctx)}

-- ============================================================================
-- Type Conversion and Instantiation
-- ============================================================================

convertMType :: AST KType -> InferM Type
convertMType ast =
  case unMType (extractSyntax ast) of
    SMType.TNumber -> return TNumber
    SMType.TInt -> return TNumber
    SMType.TBool -> return TBool
    SMType.TString -> return TString
    SMType.TNever -> return TNever
    SMType.TUnknown -> return TUnknown
    SMType.TVar varAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      case name of
        "string" -> return TString
        "number" -> return TNumber
        "bool" -> return TBool
        "never" -> return TNever
        "unknown" -> return TUnknown
        _ -> do
          ctx <- get
          case Map.lookup name (icVariances ctx) of
            Just variances | length variances == 0 -> return $ TApplication name []
            _ -> return $ TVar (TypeVar name)
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
      let SVariable.Var baseName = unVariable (extractSyntax baseAst)
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
  freshVars <- mapM (const freshVar) vars
  let substitution = Map.fromList (zip vars (map TVar freshVars))
  return $ substituteGenerics substitution typ

instantiatePolymorphicTypeWithExistentials :: TypeScheme -> InferM Type
instantiatePolymorphicTypeWithExistentials (TypeScheme [] typ) = return typ
instantiatePolymorphicTypeWithExistentials (TypeScheme vars typ) = do
  freshExistentialVars <-
    mapM
      ( \_ -> do
          var <- freshVar
          addExistentialVar var
          return var
      )
      vars
  let substitution = Map.fromList (zip vars (map TVar freshExistentialVars))
  return $ substituteGenerics substitution typ

-- Scope-aware instantiation that unifies with existing generics
instantiatePolymorphicTypeWithScopeAwareness :: TypeScheme -> InferM Type
instantiatePolymorphicTypeWithScopeAwareness (TypeScheme [] typ) = return typ
instantiatePolymorphicTypeWithScopeAwareness (TypeScheme vars typ) = do
  ctx <- get
  substitutionVars <-
    mapM
      ( \varName -> do
          case Map.lookup varName (icTypeEnv ctx) of
            Just (TypeScheme [] (TGeneric existingName))
              | existingName == varName ->
                  -- Type parameter is already bound as generic in current scope - use it
                  return (TGeneric varName)
            _ -> do
              -- Type parameter not bound in scope - create fresh existential
              var <- freshVar
              addExistentialVar var
              return (TVar var)
      )
      vars
  let substitution = Map.fromList (zip vars substitutionVars)
  return $ substituteGenerics substitution typ

-- ============================================================================
-- Declaration Inference
-- ============================================================================

inferDecl :: AST KDefinition -> InferM ()
inferDecl ast =
  case unDefinition (extractSyntax ast) of
    SDefinition.ValDef varAst typeParams typeAst exprAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      if null typeParams
        then inferMonomorphicVal name typeAst exprAst
        else inferPolymorphicVal name typeParams typeAst exprAst
    SDefinition.DataDef dataNameAst constructorDefs -> do
      let SVariable.Var dataName = unVariable (extractSyntax dataNameAst)
      mapM_ (inferConstructorDef dataName) constructorDefs
    SDefinition.TypeDef aliasAst _params typeAst -> do
      let SVariable.Var aliasName = unVariable (extractSyntax aliasAst)
      aliasType <- convertMType typeAst
      addVar aliasName aliasType
 where
  inferMonomorphicVal name typeAst exprAst = do
    declaredType <- convertMType typeAst
    inferredType <- inferExpr exprAst
    addConstraint $ Subtype inferredType declaredType
    addVar name declaredType

  inferPolymorphicVal name typeParams typeAst exprAst = do
    let paramNames =
          [ case unVariable (extractSyntax paramAst) of
            SVariable.Var paramName -> paramName
          | paramAst <- typeParams
          ]

    declaredType <- convertMType typeAst
    let genericType =
          foldr
            ( \paramName acc ->
                substituteTypeVar (TypeVar paramName) (TGeneric paramName) acc
            )
            declaredType
            paramNames

    savedEnv <- gets icTypeEnv
    mapM_ (\paramName -> addVar paramName (TGeneric paramName)) paramNames
    inferredType <- inferExpr exprAst
    addConstraint $ Subtype inferredType genericType
    modify $ \ctx -> ctx{icTypeEnv = savedEnv}

    let typeScheme = generalize genericType
    addVarScheme name typeScheme

  substituteTypeVar :: TypeVar -> Type -> Type -> Type
  substituteTypeVar target replacement = applySubst (Map.singleton target replacement)

-- ============================================================================
-- Constructor Definition Inference
-- ============================================================================

inferConstructorDef :: T.Text -> SDefinition.ConstructorDef AST -> InferM ()
inferConstructorDef dataName (SDefinition.ConstructorDef ctorNameAst typeParams fullTypeAst) = do
  let SVariable.Var ctorName = unVariable (extractSyntax ctorNameAst)
  let typeParamNames =
        [ case unVariable (extractSyntax paramAst) of
          SVariable.Var paramName -> paramName
        | paramAst <- typeParams
        ]

  fullType <- convertMType fullTypeAst
  addConstructor ctorName

  if null typeParams
    then handleMonomorphicConstructor dataName ctorName fullType
    else handlePolymorphicConstructor dataName ctorName typeParamNames fullType
 where
  handleMonomorphicConstructor dataName ctorName fullType = do
    addVar ctorName fullType
    case extractReturnType fullType of
      Just (TApplication baseTypeName _)
        | baseTypeName == dataName ->
            addVar ("TYPE_" <> dataName) (TVar (TypeVar dataName))
      Just (TVar (TypeVar typeName))
        | typeName == dataName ->
            addVar ("TYPE_" <> dataName) (TVar (TypeVar dataName))
      _ -> return ()

  handlePolymorphicConstructor dataName ctorName typeParamNames fullType = do
    let genericType =
          foldr
            ( \paramName acc ->
                substituteTypeVar (TypeVar paramName) (TGeneric paramName) acc
            )
            fullType
            typeParamNames

    case extractReturnType genericType of
      Just returnType -> do
        let (argTypes, _) = unfoldFunctionType genericType
        let ctorArgType = if null argTypes then TNever else TFunction argTypes TNever
        let varianceMap = analyzeVariance typeParamNames ctorArgType returnType

        ctx <- get
        case Map.lookup dataName (icTypeConstructors ctx) of
          Nothing -> do
            let typeInfo = TypeConstructorInfo (length typeParamNames) varianceMap [ctorName]
            put
              ctx
                { icTypeConstructors = Map.insert dataName typeInfo (icTypeConstructors ctx)
                , icVariances = Map.insert dataName varianceMap (icVariances ctx)
                }
          Just tci -> do
            let updatedTci = tci{tciConstructors = ctorName : tciConstructors tci}
            put ctx{icTypeConstructors = Map.insert dataName updatedTci (icTypeConstructors ctx)}

        let typeScheme = generalize genericType
        addVarScheme ctorName typeScheme
      Nothing -> error "Constructor must have function type"

  extractReturnType :: Type -> Maybe Type
  extractReturnType (TFunction _ ret) = extractReturnType ret
  extractReturnType t = Just t

  unfoldFunctionType :: Type -> ([Type], Type)
  unfoldFunctionType (TFunction argTypes returnType) = (argTypes, returnType)
  unfoldFunctionType nonFunctionType = ([], nonFunctionType)

  analyzeVariance :: [T.Text] -> Type -> Type -> VarianceMap
  analyzeVariance paramNames ctorArgsType returnType =
    [analyzeParamVariance param ctorArgsType returnType | param <- paramNames]
   where
    analyzeParamVariance param ctorArgsType returnType =
      let ctorVar = analyzeInType param ctorArgsType Contravariant
          retVar = analyzeInType param returnType Covariant
       in case (ctorVar, retVar) of
            (Nothing, Nothing) -> Invariant
            (Nothing, Just _) -> Bivariant
            (Just cv, Nothing) -> cv
            (Just cv, Just rv) -> combineVariance cv rv

    analyzeInType :: T.Text -> Type -> Variance -> Maybe Variance
    analyzeInType param targetType contextVar = case targetType of
      TGeneric name | name == param -> Just contextVar
      TVar (TypeVar name) | name == param -> Just contextVar
      TFunction argTypes returnType ->
        let argVars = [analyzeInType param argType (flipVariance contextVar) | argType <- argTypes]
            retVar = analyzeInType param returnType contextVar
         in foldr combineOptional retVar argVars
      TApplication _ argTypes ->
        let argVars = [analyzeInType param argType contextVar | argType <- argTypes]
         in foldr combineOptional Nothing argVars
      TUnion types ->
        let vars = [analyzeInType param t contextVar | t <- Set.toList types]
         in foldr combineOptional Nothing vars
      TIntersection types ->
        let vars = [analyzeInType param t contextVar | t <- Set.toList types]
         in foldr combineOptional Nothing vars
      _ -> Nothing

    flipVariance Covariant = Contravariant
    flipVariance Contravariant = Covariant
    flipVariance v = v

    combineVariance Invariant _ = Invariant
    combineVariance _ Invariant = Invariant
    combineVariance Covariant Contravariant = Invariant
    combineVariance Contravariant Covariant = Invariant
    combineVariance v1 v2 | v1 == v2 = v1
    combineVariance _ _ = Invariant

    combineOptional Nothing mv = mv
    combineOptional mv Nothing = mv
    combineOptional (Just v1) (Just v2) = Just (combineVariance v1 v2)

  substituteTypeVar :: TypeVar -> Type -> Type -> Type
  substituteTypeVar target replacement = applySubst (Map.singleton target replacement)

-- ============================================================================
-- Expression Inference
-- ============================================================================

inferExpr :: AST KExpr -> InferM Type
inferExpr ast =
  case unExpr (extractSyntax ast) of
    EVar varAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      ctx <- get
      case Map.lookup name (icTypeEnv ctx) of
        Just scheme -> instantiatePolymorphicType scheme
        Nothing -> errorWithPosition ast $ "Unbound variable: " ++ T.unpack name
    ELiteral litAst -> do
      return $ convertLiteral litAst
    ELambda params bodyAst -> do
      paramTypes <- mapM inferLambdaParam params
      bodyType <- inferExpr bodyAst
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
      mapM_ inferLet letAsts
      inferExpr exprAst
    EMatch scrutinees cases -> do
      scrutineeTypes <- mapM inferExpr scrutinees
      resultType <- TVar <$> freshVar

      mapM_ (inferMatchCase scrutineeTypes resultType) cases

      -- Simple exhaustivity check
      checkExhaustivity scrutineeTypes cases

      return resultType
 where
  errorWithPosition :: AST a -> String -> InferM b
  errorWithPosition ast message =
    let (start, _) = case unHFix ast of
          (Metadata start end :*: _) -> (start, end)
     in error $ message ++ " at " ++ show start

  convertLiteral :: AST KLiteral -> Type
  convertLiteral ast =
    case unLiteral (extractSyntax ast) of
      SLiteral.NumberLiteral n -> TLiteral (LNumber n)
      SLiteral.IntLiteral i -> TLiteral (LNumber (fromIntegral i))
      SLiteral.BoolLiteral b -> TLiteral (LBool b)
      SLiteral.StringLiteral s -> TLiteral (LString s)

  inferLambdaParam (patAst, typeAst) = do
    paramType <- convertMType typeAst
    case unPattern (extractSyntax patAst) of
      SPattern.PVar varAst -> do
        let SVariable.Var name = unVariable (extractSyntax varAst)
        addVar name paramType
        return paramType
      SPattern.PWildcard ->
        return paramType
      _ -> errorWithPosition patAst "Complex patterns not yet supported in lambda"

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
    Let patAst typeAst exprAst -> do
      exprType <- inferExpr exprAst
      declaredType <- convertMType typeAst
      addConstraint $ Subtype exprType declaredType
      case unPattern (extractSyntax patAst) of
        SPattern.PVar varAst -> do
          let SVariable.Var name = unVariable (extractSyntax varAst)
          addVar name declaredType
        SPattern.PWildcard ->
          return ()
        _ -> errorWithPosition patAst "Complex patterns not yet supported in let"

-- ============================================================================
-- Pattern Matching
-- ============================================================================

inferMatchCase :: [Type] -> Type -> (List (AST KPattern, AST KType), AST KExpr) -> InferM ()
inferMatchCase scrutineeTypes resultType (patterns, resultExpr) = do
  savedEnv <- gets icTypeEnv
  savedExistentials <- gets icExistentialVars

  -- Clear existentials for this pattern match
  modify $ \ctx -> ctx{icExistentialVars = Set.empty}

  -- Process patterns
  zipWithM inferPattern patterns scrutineeTypes

  -- Infer result
  caseResultType <- inferExpr resultExpr

  -- Check for escaping existentials
  ctx <- get
  let resultVars = typeVars caseResultType
  let escapingVars = Set.intersection (icExistentialVars ctx) resultVars
  when (not $ Set.null escapingVars) $
    error $
      "Type error: Existential type variables cannot escape their scope in pattern match result. "
        ++ "Result type: "
        ++ show caseResultType

  addConstraint $ Subtype caseResultType resultType

  -- Restore environment
  modify $ \ctx -> ctx{icTypeEnv = savedEnv, icExistentialVars = savedExistentials}
 where
  inferPattern (patAst, typeAst) scrutineeType = do
    declaredType <- convertMType typeAst
    addConstraint $ Subtype scrutineeType declaredType
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
          let (argTypes, _) = unfoldFunctionType constructorType

          when (length argPatterns /= length argTypes) $
            error $
              "Constructor "
                ++ T.unpack constructorName
                ++ " expects "
                ++ show (length argTypes)
                ++ " arguments, got "
                ++ show (length argPatterns)

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

checkExhaustivity :: [Type] -> [(List (AST KPattern, AST KType), AST KExpr)] -> InferM ()
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
