{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker (
  typeCheck, -- For individual expressions
  typeCheckProgram, -- For whole programs
  TypeError (..), -- Re-export
)
where

import Control.Monad (forM, unless, when, foldM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify)
import Data.List (foldr, zip) -- Added zip
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust) -- Added fromJust
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax (
  BinOp (..),
  Definition (..),
  Effect (..),
  Effects,
  Expr (..),
  Match (..), -- Added Match
  Clause (..), -- Added Clause
  Pattern (..), -- Added Pattern
  PConstructor (..), -- Added PConstructor
  PVar (..), -- Added PVar
  PWildcard (..), -- Added PWildcard
  Program (..),
  Type (..),
  TAlgebraicData (..),
  ConstructorDef (..), 
  DataDef (..),
  AdtInfo (..), -- Added AdtInfo
  ConstructorSignature (..), -- Added ConstructorSignature
  TypeError (..),
 )

-- | Signature of a data constructor
data ConstructorSignature = ConstructorSignature
  { csArgTypes :: [Type] -- Argument types
  , csResultType :: Type -- Result ADT type
  }
  deriving (Show, Eq)

-- | Information about a declared Algebraic Data Type (ADT)
data AdtInfo = AdtInfo
  { adtName :: Text -- Name of the ADT
  , adtConstructors :: Map Text ConstructorSignature -- Constructors (name -> signature)
  }
  deriving (Show, Eq)

-- | Type checking monad: Error handling + State (for environment)
type TypeCheck a = ExceptT TypeError (State TypeState) a

-- | Type checking state
data TypeState = TypeState
  { tsEnv :: Map Text Type -- Type environment for variables and constructors
  , tsAdtEnv :: Map Text AdtInfo -- Environment for ADT definitions
  }

-- | Initial state for type checking
initialState :: TypeState
initialState = TypeState{tsEnv = Map.empty, tsAdtEnv = Map.empty}

-- | Get the current type environment for variables and constructors
getEnv :: TypeCheck (Map Text Type)
getEnv = gets tsEnv

-- | Add a binding to the variable/constructor type environment
addBinding :: Text -> Type -> TypeCheck ()
addBinding name typ = modify $ \st -> st{tsEnv = Map.insert name typ (tsEnv st)}

-- | Add an ADT definition to the ADT environment
addAdtInfo :: Text -> AdtInfo -> TypeCheck ()
addAdtInfo name adtInfo = modify $ \st -> st{tsAdtEnv = Map.insert name adtInfo (tsAdtEnv st)}

-- | Get the current ADT environment
getAdtEnv :: TypeCheck (Map Text AdtInfo)
getAdtEnv = gets tsAdtEnv

-- | Run a computation in a temporarily extended environment
withBinding :: Text -> Type -> TypeCheck a -> TypeCheck a
withBinding name typ action = do
  oldEnv <- getEnv
  addBinding name typ
  result <- action
  modify $ \st -> st{tsEnv = oldEnv} -- Restore original environment
  return result

-- | Run a computation in an environment temporarily extended with multiple bindings
withBindings :: [(Text, Type)] -> TypeCheck a -> TypeCheck a
withBindings bindings action = do
  oldEnv <- getEnv
  mapM_ (uncurry addBinding) bindings
  result <- action
  modify $ \st -> st{tsEnv = oldEnv} -- Restore original environment
  return result

-- | Resolve a type to ensure it refers to known ADTs or primitive types.
-- currentAdtName is for allowing recursive definitions.
resolveType :: Type -> Maybe Text -> TypeCheck ()
resolveType typ currentAdtName = do
  adtEnv <- getAdtEnv
  case typ of
    TNumber -> return ()
    TBool -> return ()
    TFunction argT retT _ -> do
      resolveType argT currentAdtName
      resolveType retT currentAdtName
    TAlgebraicData name -> do
      let isCurrent = Just name == currentAdtName
      unless (isCurrent || Map.member name adtEnv) $
        throwError $ CustomErrorType $ "Undefined algebraic data type: " <> name
    -- Potentially other types in the future
    -- _ -> throwError $ CustomErrorType $ "Type validation not implemented for: " <> T.pack (show typ)

{- | Effect operations mapping (name, (arg_type, return_type, effect_produced))
This remains useful for the 'Do' expression.
-}
effectOps :: [(Text, (Type, Type, Effect))]
effectOps = [("throw", (TNumber, TNumber, Throw))] -- Example, can be expanded

-- | Lookup an effect operation by its name
lookupEffectOp :: Text -> Maybe (Type, Type, Effect)
lookupEffectOp name = lookup name effectOps

{- | エフェクトのサブタイピングをチェック
   S1 ⊂ S2 のとき S1 サブタイプ S2
-}
isSubEffects :: Effects -> Effects -> Bool
isSubEffects e1 e2 = e1 `Set.isSubsetOf` e2

{- | Unify two types. Throws TypeMismatch if they are not equal.
   関数型の場合は引数と戻り値の型を再帰的に照合し、エフェクトについてはサブタイピングを適用する
   エフェクトのサブタイピング: S1 ⊂ S2 のとき T with S1 は T with S2 に代入可能
-}
unify :: Type -> Type -> TypeCheck ()
unify (TFunction argT1 retT1 eff1) (TFunction argT2 retT2 eff2) = do
  -- 引数の型と戻り値の型は通常通り単一化する
  unify argT1 argT2
  unify retT1 retT2

  -- エフェクトのサブタイピング: eff1 ⊆ eff2 であれば OK
  -- 実際の型のエフェクトが期待される型のサブセットであれば互換性あり
  unless (eff1 `isSubEffects` eff2) $
    throwError $
      EffectMismatch eff1 eff2
unify expected actual =
  when (expected /= actual) $
    throwError $
      TypeMismatch expected actual

{- | Type check a single expression (e.g., for testing or REPL)
This function is simplified as tsEffects is not part of TypeState for accumulation here.
-}
typeCheck :: Expr -> Either TypeError (Type, Effects)
typeCheck expr = evalState (runExceptT (inferType expr)) initialState

{- | Infer the type and effects of an expression.
Effects are accumulated and returned alongside the type.
-}
inferType :: Expr -> TypeCheck (Type, Effects)
inferType expr = case expr of
  Number _ -> return (TNumber, Set.empty)
  Bool _ -> return (TBool, Set.empty)
  Var name -> do
    env <- gets tsEnv
    case Map.lookup name env of
      Nothing -> throwError $ UnboundVariable name
      Just t -> return (t, Set.empty) -- Constructors are now in tsEnv
  BinOp op e1 e2 -> do
    (t1, eff1) <- inferType e1
    (t2, eff2) <- inferType e2
    let currentEffects = eff1 `Set.union` eff2
    resType <- case op of
      Add -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
      Sub -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
      Mul -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
      Div -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
      Eq -> unify t1 t2 >> return TBool
      Lt -> unify TNumber t1 >> unify TNumber t2 >> return TBool
      Gt -> unify TNumber t1 >> unify TNumber t2 >> return TBool
    return (resType, currentEffects)
  If cond th el -> do
    (condType, condEff) <- inferType cond
    unify TBool condType
    (thenType, thenEff) <- inferType th
    (elseType, elseEff) <- inferType el
    unify thenType elseType
    return (thenType, condEff `Set.union` thenEff `Set.union` elseEff)
  Lambda name mType body -> do
    let actualParamType = fromMaybe TNumber mType
    (bodyType, bodyEffects) <- withBinding name actualParamType $ inferType body
    return (TFunction actualParamType bodyType bodyEffects, Set.empty)
  Apply func arg -> do
    (funcType, funcEffs) <- inferType func
    (argType, argEffs) <- inferType arg
    let accumulatedEffects = funcEffs `Set.union` argEffs
    case funcType of
      TFunction paramT retT funBodyEffs -> do
        unify paramT argType
        return (retT, accumulatedEffects `Set.union` funBodyEffs)
      _ -> throwError $ TypeMismatch (TFunction argType (error "Cannot construct expected type for error reporting easily") Set.empty) funcType
  Do name -> do
    case lookupEffectOp name of
      Just (argT, retT, effect) ->
        return (TFunction argT retT (Set.singleton effect), Set.empty)
      Nothing -> throwError $ UndefinedEffect name
  -- Match expression type checking will be handled in a subsequent task.
  -- For now, if it's encountered, it might fall through or cause an error if not handled by inferType.
  -- Let's add a placeholder for Match to avoid compilation errors if it's part of Expr.
  Match scrutinee clauses -> do
    -- Infer scrutinee type
    (scrutineeType, scrutineeEffects) <- inferType scrutinee
    adtEnv <- getAdtEnv

    -- Check if scrutinee is TAlgebraicData
    adtActualName <- case scrutineeType of
      TAlgebraicData name -> return name
      _ -> throwError $ TypeMismatch (TAlgebraicData "ADT" []) scrutineeType -- Placeholder for expected type

    -- Retrieve AdtInfo
    adtInfo <- case Map.lookup adtActualName adtEnv of
      Nothing -> throwError $ CustomErrorType $ "ADT info not found for type: " <> adtActualName
      Just info -> return info
    let adtConstructorsMap = adtConstructors adtInfo

    -- Check for empty clauses
    when (null clauses) $
      throwError $ CustomErrorType "Match expression cannot have empty clauses"

    -- Process clauses
    let processClause (firstBranchTypeOpt, accClauseEffects) (Clause pattern branchExpr) = do
          localBindings <- case pattern of
            PConstructor patConsName varNames -> do
              constructorSig <- case Map.lookup patConsName adtConstructorsMap of
                Nothing -> throwError $ CustomErrorType $ "Constructor '" <> patConsName <> "' not part of ADT '" <> adtActualName <> "'"
                Just sig -> return sig
              unless (length varNames == length (csArgTypes constructorSig)) $
                throwError $ CustomErrorType $ "Pattern arity mismatch for constructor '" <> patConsName <> "'. Expected " <> T.pack (show (length (csArgTypes constructorSig))) <> " args, got " <> T.pack (show (length varNames))
              return $ zip varNames (csArgTypes constructorSig)
            PVar varName -> return [(varName, scrutineeType)]
            PWildcard -> return []

          (currentBranchExprType, currentBranchExprEffects) <- withBindings localBindings $ inferType branchExpr
          
          newFirstBranchTypeOpt <- case firstBranchTypeOpt of
            Nothing -> return $ Just currentBranchExprType
            Just firstBranchType -> do
              unify firstBranchType currentBranchExprType
              return $ Just firstBranchType
          
          return (newFirstBranchTypeOpt, Set.union accClauseEffects currentBranchExprEffects)

    (finalBranchTypeOpt, totalClauseEffects) <- foldM processClause (Nothing, Set.empty) clauses
    
    finalMatchExprType <- case finalBranchTypeOpt of
        Nothing -> throwError $ CustomErrorType "Could not determine type of match expression (should be caught by empty clauses check)" -- Should not happen if clauses not empty
        Just t -> return t

    -- Exhaustiveness Check
    let (coveredConstructors, hasWildcardOrVar) = foldr 
          (\(Clause p _) (accSet, accBool) -> case p of
            PConstructor cn _ -> (Set.insert cn accSet, accBool)
            PVar _          -> (accSet, True)
            PWildcard       -> (accSet, True)
          ) (Set.empty, False) clauses

    unless hasWildcardOrVar $ do
      let allAdtConstructors = Map.keysSet adtConstructorsMap
      when (not (allAdtConstructors `Set.isSubsetOf` coveredConstructors)) $
        throwError $ CustomErrorType $ "Pattern matching is not exhaustive for ADT '" <> adtActualName <> "'. Missing: " <> T.pack (show (Set.difference allAdtConstructors coveredConstructors))

    return (finalMatchExprType, Set.union scrutineeEffects totalClauseEffects)

  _ -> throwError $ CustomErrorType $ "Type checking not implemented for this expression: " <> T.pack (show expr)


-- | Helper to build the functional type of a constructor
buildConstructorType :: [Type] -> Type -> Type
buildConstructorType argTypes resultType = foldr (\arg acc -> TFunction arg acc Set.empty) resultType argTypes

-- | Register ADTs and their constructors
registerAdtsAndConstructors :: [Definition] -> TypeCheck ()
registerAdtsAndConstructors definitions = do
  let dataDefs = [def | def@(DataDef _ _) <- definitions]

  -- Phase 1: Collect ADT names, check for duplicates, and pre-populate tsAdtEnv.
  currentAdtEnv <- getAdtEnv
  foldM_ (preRegisterAdtName currentAdtEnv) () dataDefs

  -- Phase 2: Process constructors for each ADT.
  mapM_ processAdtDefinition dataDefs

  where
    preRegisterAdtName :: Map Text AdtInfo -> () -> Definition -> TypeCheck ()
    preRegisterAdtName initialAdtEnv _ (DataDef adtName _) = do
      when (Map.member adtName initialAdtEnv) $ -- Check against env before this batch started
        throwError $ CustomErrorType $ "Duplicate ADT definition: " <> adtName
      
      -- Check if already pre-registered in this current batch (in case of duplicate DataDef in input list)
      currentBatchAdtEnv <- getAdtEnv
      when (Map.member adtName currentBatchAdtEnv && not (Map.member adtName initialAdtEnv)) $
         throwError $ CustomErrorType $ "Duplicate ADT definition in current batch: " <> adtName
      
      addAdtInfo adtName (AdtInfo adtName Map.empty) -- Placeholder for recursive resolution
    preRegisterAdtName _ _ _ = return () -- Should not be called with ValDef

    processAdtDefinition :: Definition -> TypeCheck ()
    processAdtDefinition (DataDef adtName consDefs) = do
      -- Build constructor signatures and add them to tsEnv
      finalConstructorMap <- foldM (buildAndRegisterConstructor adtName) Map.empty consDefs
      -- Update the AdtInfo with the fully processed constructors
      modify $ \st -> st {tsAdtEnv = Map.adjust (\info -> info {adtConstructors = finalConstructorMap}) adtName (tsAdtEnv st)}
    processAdtDefinition _ = return () -- Skip ValDefs

    buildAndRegisterConstructor :: Text -> Map Text ConstructorSignature -> ConstructorDef -> TypeCheck (Map Text ConstructorSignature)
    buildAndRegisterConstructor adtName accumulatedConstructors (ConstructorDef consNameText argTypes) = do
      -- Check for global name collision in tsEnv (vars, other constructors)
      env <- getEnv
      when (Map.member consNameText env) $
        throwError $ CustomErrorType $ "Constructor name '" <> consNameText <> "' conflicts with an existing definition or another constructor."

      -- Check for local name collision (constructors within the same ADT)
      when (Map.member consNameText accumulatedConstructors) $
        throwError $ CustomErrorType $ "Duplicate constructor name '" <> consNameText <> "' in ADT '" <> adtName <> "'"

      -- Resolve argument types (ADT name is visible in tsAdtEnv due to Phase 1)
      mapM_ (\argT -> resolveType argT (Just adtName)) argTypes

      let resultType = TAlgebraicData adtName
      let constructorSig = ConstructorSignature argTypes resultType
      let functionalType = buildConstructorType argTypes resultType

      -- Add constructor's functional type to global value environment (tsEnv)
      addBinding consNameText functionalType
      
      return $ Map.insert consNameText constructorSig accumulatedConstructors

-- | Type check a whole program
typeCheckProgram :: Program -> Either TypeError (Map.Map Text Type)
typeCheckProgram (Program definitions) = evalState (runExceptT go) initialState
  where
    go :: TypeCheck (Map.Map Text Type)
    go = do
      -- Pass 1: Register ADTs and their constructors
      registerAdtsAndConstructors definitions

      -- Pass 2: Populate environment with declared types of ValDefs for mutual recursion.
      -- And type check ValDef bodies.
      -- We need to separate collecting ValDef types and checking them,
      -- because a ValDef might use an ADT constructor whose type was just registered.
      
      -- Collect ValDef types first
      valDefTypes <- foldM collectValDefTypes Map.empty definitions
      
      -- Add ValDef types to the environment, which already contains constructor types
      currentEnv <- getEnv
      let combinedEnv = Map.union currentEnv valDefTypes -- valDefTypes will overwrite if names clash, but constructors should be unique
      modify $ \st -> st {tsEnv = combinedEnv}

      -- Check ValDef bodies
      checkedValDefsData <- foldM checkValDefBody [] definitions
      
      return $ Map.fromList checkedValDefsData

    collectValDefTypes :: Map Text Type -> Definition -> TypeCheck (Map Text Type)
    collectValDefTypes acc def = case def of
      ValDef name typ _ -> return $ Map.insert name typ acc
      _ -> return acc

    checkValDefBody :: [(Text, Type)] -> Definition -> TypeCheck [(Text, Type)]
    checkValDefBody acc def = case def of
      ValDef name declType exprBody -> do
        (inferredBodyType, inferredBodyEffects) <- inferType exprBody
        unify declType inferredBodyType
        unless (Set.null inferredBodyEffects) $
          throwError $ CustomErrorType $
            "In definition '" <> name <> "': effects are not allowed for top-level definitions but got " <> T.pack (show inferredBodyEffects)
        return $ (name, declType) : acc
      _ -> return acc
