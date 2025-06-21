{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constraint generation for the Memento type solver
module Language.Memento.TypeSolver.ConstraintGen (
  inferProgram,
  typeCheckAST,
) where

import Control.Monad (foldM, unless, when, zipWithM, zipWithM_)
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
import Language.Memento.Syntax.Tag (KBinOp, KDefinition, KExpr, KLet, KLiteral, KPattern, KProgram, KType, KVariable)
import qualified Language.Memento.Syntax.Variable as SVariable
import Language.Memento.TypeSolver.Assumption (calculateGenericBounds)
import Language.Memento.TypeSolver.Solver (solve)
import Language.Memento.TypeSolver.Types

{-
NOTE: TypeVar from Solver is pure "internal" type variable.
SVariable.Var is the "external" variable that is used in the source code. (has binding to type constructor or generics)
-}

-- ============================================================================
-- Core Types
-- ============================================================================

type VarianceMap = [Variance]

data InferContext = InferContext
  { icVarCounter :: Int
  , icConstraints :: ConstraintSet
  , icTypeEnv :: Map.Map T.Text TypeScheme -- Maps variable names to their type schemes
  , icTypeConstructors :: Map.Map T.Text TypeConstructorInfo
  , icTemporaryTypeConstructors :: Set.Set T.Text -- For data definition
  , icConstructors :: Set.Set T.Text
  , icGenericTypes :: Map.Map T.Text T.Text
  , icAssumptions :: AssumptionSet -- Type variable substitutions
  }
  deriving (Show)

data TypeConstructorInfo = TypeConstructorInfo
  { tciKind :: Int
  , tciVariance :: VarianceMap
  , tciConstructors :: [T.Text]
  }
  deriving (Show, Eq)

type InferM = StateT InferContext (Except String)

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
          , icTemporaryTypeConstructors = Set.empty
          , icConstructors = Set.empty
          , icGenericTypes = Map.empty
          , icAssumptions = Set.empty
          }
  case runExcept (runStateT (inferProgramM ast) initialCtx) of
    Left err -> Left err
    Right ((), finalCtx) -> do
      let constraints = icConstraints finalCtx
          filteredConstraints = constraints
          varMap = Map.map (\(TypeConstructorInfo _ variances _) -> variances) $ icTypeConstructors finalCtx

      -- traceM $ formatTypeEnv $ icTypeEnv finalCtx
      let genBndMap = calculateGenericBounds varMap $ icAssumptions finalCtx

      case solve varMap genBndMap filteredConstraints of
        Success ->
          Right (icTypeEnv finalCtx)
        Contradiction msg ->
          Left $ "Type error: " ++ msg ++ "\n" ++ formatConstraintSet filteredConstraints ++ "\n" ++ formatBoundsMap genBndMap
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
-- Type algebras
-- ============================================================================

appendTypeConstructorInfo :: TypeConstructorInfo -> TypeConstructorInfo -> Maybe TypeConstructorInfo
appendTypeConstructorInfo (TypeConstructorInfo k1 v1 cs1) (TypeConstructorInfo k2 v2 cs2)
  | k1 == k2 = Just $ TypeConstructorInfo k1 (zipWith combineVariance v1 v2) (cs1 ++ cs2)
  | otherwise = Nothing

-- ============================================================================
-- Core Inference Operations
-- ============================================================================

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

-- Generate GADT assumptions by attempting to unify constructor return type with scrutinee type
generateGADTAssumptions :: Type -> Type -> InferM ()
generateGADTAssumptions (TApplication name1 args1) (TApplication name2 args2)
  | name1 == name2 = zipWithM_ addEqAssumption args1 args2
generateGADTAssumptions _ _ = throwError "Cannot generate GADT assumptions for non-application types"

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
          case Map.lookup name (icTypeConstructors ctx) of
            Just (TypeConstructorInfo k _ _) | k == 0 -> return $ TApplication name []
            _ ->
              if Set.member name (icTemporaryTypeConstructors ctx)
                then
                  return $ TApplication name [] -- Temporary type constructor, treat as application with no args
                else do
                  -- Check if the name is a generic type
                  case Map.lookup name (icGenericTypes ctx) of
                    Just g -> do
                      -- If the name is in generic types, treat it as a generic type
                      return $ TGeneric g
                    Nothing -> do
                      -- Var is not a generic type nor type constructor, then throw an error
                      throwError $ "Unknown type variable: " ++ T.unpack name
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
      ctx <- get

      case Map.lookup baseName (icTypeConstructors ctx) of
        Just (TypeConstructorInfo k _ _) -> do
          -- If the base is a type constructor, we need to instantiate it with arguments
          when (k /= length argAsts) $
            throwError $
              "Type constructor " ++ T.unpack baseName ++ " expects " ++ show k ++ " arguments, got " ++ show (length argAsts)
        Nothing | Set.member baseName (icTemporaryTypeConstructors ctx) -> pure ()
        Nothing -> throwError $ "Unknown type variable: " ++ T.unpack baseName

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
  --      val a : number := id(1)  // Usage <- this time, T is substituted by fresh variable (that will be resolved to "number" by solver)
  freshVars <- mapM (const freshVar) vars
  let substitution = Map.fromList (zip vars (map TVar freshVars))
  return $ substituteGenerics substitution typ

-- | Instantiate with fresh generics for GADT pattern matching
instantiatePolymorphicTypeWithScopeAwareness :: TypeScheme -> InferM Type
instantiatePolymorphicTypeWithScopeAwareness (TypeScheme [] typ) = return typ
instantiatePolymorphicTypeWithScopeAwareness (TypeScheme vars typ) = do
  -- For GADT pattern matching, we need fresh generics, not type variables
  freshGenericNames <- mapM freshGenericsWithPrefix vars
  let substitution = Map.fromList (zip vars (map TGeneric freshGenericNames))
  return $ substituteGenerics substitution typ

-- ============================================================================
-- Declaration Inference
-- ============================================================================

inferDecl :: AST KDefinition -> InferM ()
inferDecl ast =
  case unDefinition (extractSyntax ast) of
    SDefinition.ValDef varAst typeParams typeAst exprAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      inferPolymorphicVal name typeParams typeAst exprAst
    SDefinition.DataDef dataNameAst constructorDefs -> do
      let SVariable.Var dataName = unVariable (extractSyntax dataNameAst)

      -- Add data type to the temporary context
      modify $ \ctx -> ctx{icTemporaryTypeConstructors = Set.insert dataName (icTemporaryTypeConstructors ctx)}

      tcInfos <- mapM (inferConstructorDef dataName) constructorDefs
      unless (null tcInfos) $ do
        -- Combine all constructor infos for this data type
        combinedInfo <-
          foldM
            ( \acc info -> case appendTypeConstructorInfo acc info of
                Just newInfo -> return newInfo
                Nothing -> throwError $ "Cannot combine type constructor infos: " ++ show acc ++ " and " ++ show info
            )
            (head tcInfos)
            (tail tcInfos)
        modify $ \ctx -> ctx{icTypeConstructors = Map.insert dataName combinedInfo (icTypeConstructors ctx)}

      -- Remove temporary context
      modify $ \ctx ->
        ctx
          { icTemporaryTypeConstructors = Set.empty
          }
    SDefinition.TypeDef aliasAst _params typeAst -> do
      let SVariable.Var aliasName = unVariable (extractSyntax aliasAst)
      aliasType <- convertMType typeAst
      addVar aliasName aliasType
 where
  inferPolymorphicVal name typeParams typeAst exprAst = do
    let paramNames =
          [ case unVariable (extractSyntax paramAst) of
            SVariable.Var paramName -> paramName
          | paramAst <- typeParams
          ]

    savedEnv <- gets icTypeEnv
    savedGenericTypes <- gets icGenericTypes

    -- id<T> : T => T  // <T> adds generic type to the context
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

    -- Revert type envs
    modify $ \ctx -> ctx{icTypeEnv = savedEnv, icGenericTypes = savedGenericTypes}

    let typeScheme = TypeScheme freshGenerics declaredType
    addVarScheme name typeScheme

-- ============================================================================
-- Constructor Definition Inference
-- ============================================================================

inferConstructorDef :: T.Text -> SDefinition.ConstructorDef AST -> InferM TypeConstructorInfo
inferConstructorDef dataName (SDefinition.ConstructorDef ctorNameAst typeParams fullTypeAst) = do
  let SVariable.Var ctorName = unVariable (extractSyntax ctorNameAst)

  -- It is important that there is distinction between constructor type param & type parm.
  -- e. g. `Cons<T>: (x : T, y : number) => Typ<T, number>`
  -- Here, `<T, number> of Typ<T, number>` are type parameters, `<T>` of `Cons<T>` is a constructor type parameters.

  let constructorTypeParamNames =
        [ case unVariable (extractSyntax paramAst) of
          SVariable.Var paramName -> paramName
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

  case unfoldFunctionType fullType of
    Just (argTypes, retType) -> do
      case retType of
        TApplication dn typeParams | dn == dataName -> do
          let tciKind = length typeParams
          varMap <- getTypeConstructorVariances
          let tciVariance =
                [ variance
                | TGeneric varName <- typeParams
                , let variance = foldr (combineVariance . analyzeParameterVariance varMap varName) Bivariant argTypes
                ]
          let typeScheme = TypeScheme freshGenerics fullType
          addVarScheme ctorName typeScheme
          return TypeConstructorInfo{tciKind = tciKind, tciVariance = tciVariance, tciConstructors = [ctorName]}
        TVar (TypeVar dn) | dn == dataName -> do
          -- Zero-argument type constructor case
          let tciKind = 0
          let tciVariance = []
          let typeScheme = TypeScheme freshGenerics fullType
          addVarScheme ctorName typeScheme
          return TypeConstructorInfo{tciKind = tciKind, tciVariance = tciVariance, tciConstructors = [ctorName]}
        _ -> throwError $ "Constructor return type doesn't match data type name. Expected: " ++ T.unpack dataName ++ ", got: " ++ show retType
    Nothing ->
      throwError $ "Constructor type must be a function, got: " ++ show fullType
 where
  unfoldFunctionType :: Type -> Maybe ([Type], Type)
  unfoldFunctionType (TFunction argTypes returnType) = Just (argTypes, returnType)
  unfoldFunctionType nonFunctionType = Nothing

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
  errorWithPosition :: AST a -> String -> InferM b
  errorWithPosition ast message =
    let (start, _) = case unHFix ast of
          (Metadata start end :*: _) -> (start, end)
     in throwError $ message ++ " at " ++ show start

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

-- NOTE: After this line, the code may be incomplete or not fully functional.

-- ============================================================================
-- Pattern Matching
-- ============================================================================

inferMatchCase :: [Type] -> Type -> (List (AST KPattern, AST KType), AST KExpr) -> InferM ()
inferMatchCase scrutineeTypes resultType (patterns, resultExpr) = do
  savedEnv <- gets icTypeEnv
  -- savedAssumptions <- saveAssumptions

  -- Process patterns and generate GADT assumptions
  zipWithM_ inferPattern patterns scrutineeTypes

  -- Infer result with assumptions in scope
  caseResultType <- inferExpr resultExpr

  -- Check for escaping existentials
  ctx <- get
  let resultVars = typeVars caseResultType
  -- assumptionVars = Set.unions (map typeVars (Map.elems (icAssumptions ctx)))
  -- escapingVars = Set.intersection resultVars assumptionVars

  -- when (not (Set.null escapingVars)) $
  --   throwError $
  --     "Existential types cannot escape their scope: " ++ show (Set.toList escapingVars)

  addConstraint $ Subtype caseResultType resultType

  -- Restore environment and assumptions after case
  -- restoreAssumptions savedAssumptions
  modify $ \ctx -> ctx{icTypeEnv = savedEnv}
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
          generateGADTAssumptions returnType expectedType

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
