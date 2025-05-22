{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker (
  typeCheck, -- For individual expressions
  typeCheckProgram, -- For whole programs
  TypeError (..), -- Re-export
)
where

import Control.Monad (foldM, foldM_, forM, unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify)
import Data.List (foldr, zip) -- Added zip
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe) -- Added fromJust
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax (
  BinOp (..),
  -- Includes Match
  -- Includes PConstructor, PVar, PWildcard
  Clause (..),
  -- Includes TAlgebraicData
  ConstructorDef (..),
  Definition (..),
  Effect (..),
  Effects,
  Expr (..),
  Pattern (..),
  Program (..),
  Type (..),
  TypeError (..),
 )

-- | Signature of a data constructor
data ConstructorSignature = ConstructorSignature
  { csArgTypes :: [Type] -- Argument types,
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
  , tsEffectEnv :: Map Text EffectInfo -- Environment for Effect definitions
  }

-- | Signature of an effect operator
data OperatorSignature = OperatorSignature
  { osArgType :: Type -- Argument type of the operator
  , osRetType :: Type -- Return type of the operator
  , osEffectName :: Text -- Name of the effect this operator belongs to
  }
  deriving (Show, Eq)

-- | Information about a declared Effect
data EffectInfo = EffectInfo
  { eiName :: Text -- Name of the Effect
  , eiOps :: Map Text OperatorSignature -- Operators (op_name -> signature)
  }
  deriving (Show, Eq)

-- | Initial state for type checking
initialState :: TypeState
initialState = TypeState{tsEnv = Map.empty, tsAdtEnv = Map.empty, tsEffectEnv = Map.empty}

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

-- | Get the current effect environment
getEffectEnv :: TypeCheck (Map Text EffectInfo)
getEffectEnv = gets tsEffectEnv

-- | Add an Effect definition to the effect environment
addEffectInfo :: Text -> EffectInfo -> TypeCheck ()
addEffectInfo name effectInfo = modify $ \st -> st{tsEffectEnv = Map.insert name effectInfo (tsEffectEnv st)}

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

{- | Resolve a type to ensure it refers to known ADTs or primitive types.
currentAdtName is for allowing recursive definitions.
-}
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
        throwError $
          CustomErrorType $
            "Undefined algebraic data type: " <> name

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
    adtEnv <- getAdtEnv

    -- Retrieve AdtInfo
    adtInfo <- case Map.lookup scrutinee adtEnv of
      Nothing -> throwError $ CustomErrorType $ "ADT info not found for type: " <> scrutinee
      Just info -> return info
    let adtConstructorsMap = adtConstructors adtInfo

    -- Check for empty clauses
    when (null clauses) $
      throwError $
        CustomErrorType "Match expression cannot have empty clauses"

    -- Process clauses
    let processClause (firstBranchTypeOpt, accClauseEffects) (Clause pattern branchExpr) = do
          -- Since scrutinee is the ADT type name, we can directly use it for type checking
          localBindings <- case pattern of
            PConstructor patConsName varNames -> do
              constructorSig <- case Map.lookup patConsName adtConstructorsMap of
                Nothing -> throwError $ CustomErrorType $ "Constructor '" <> patConsName <> "' not part of ADT '" <> scrutinee <> "'"
                Just sig -> return sig
              unless (length varNames == length (csArgTypes constructorSig)) $
                throwError $
                  CustomErrorType $
                    "Pattern arity mismatch for constructor '" <> patConsName <> "'. Expected " <> T.pack (show (length (csArgTypes constructorSig))) <> " args, got " <> T.pack (show (length varNames))
              return $ zip varNames (csArgTypes constructorSig)
            PVar varName -> return [(varName, TAlgebraicData scrutinee)]
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
    let (coveredConstructors, hasWildcardOrVar) =
          foldr
            ( \(Clause p _) (accSet, accBool) -> case p of
                PConstructor cn _ -> (Set.insert cn accSet, accBool)
                PVar _ -> (accSet, True)
                PWildcard -> (accSet, True)
            )
            (Set.empty, False)
            clauses

    unless hasWildcardOrVar $ do
      let allAdtConstructors = Map.keysSet adtConstructorsMap
      unless (allAdtConstructors `Set.isSubsetOf` coveredConstructors) $
        throwError $
          CustomErrorType $
            "Pattern matching is not exhaustive for ADT '" <> scrutinee <> "'. Missing: " <> T.pack (show (Set.difference allAdtConstructors coveredConstructors))

    return (TFunction (TAlgebraicData scrutinee) finalMatchExprType totalClauseEffects, Set.empty)
  Handle handledExpr effectsToHandle handlerClauses -> do
    (exprType, exprEffects) <- inferType handledExpr
    effectEnv <- getEffectEnv

    let effectNamesToHandle = Set.map (\(Effect name) -> name) effectsToHandle
    operatorSigsMap :: Map Text OperatorSignature <- foldM (collectOpSigs effectEnv) Map.empty effectNamesToHandle
    let clausesReturnEffects = exprEffects `Set.difference` effectsToHandle

    -- Separate clauses
    let (opClauses, returnClauses) = partitionHandlerClauses handlerClauses
    
    when (null returnClauses) $
      throwError $ CustomErrorType "Handle expression must have at least one return clause."

    -- Determine targetBodyType from return clauses
    (targetBodyType, returnClausesEffects) <- processReturnClauses exprType returnClauses clausesReturnEffects

    -- Process operator clauses
    opClausesCombinedEffects <- processOpClauses operatorSigsMap opClauses targetBodyType clausesReturnEffects

    -- Exhaustiveness Check
    let handledOperatorsInClauses = Set.fromList [opName | (HandlerClause opName _ _ _) <- opClauses]
    let allOperatorsInHandledEffects = Map.keysSet operatorSigsMap
    unless (allOperatorsInHandledEffects `Set.isSubsetOf` handledOperatorsInClauses) $
      throwError $ CustomErrorType $
        "Handle expression is not exhaustive. Missing handlers for operators: " <>
        T.pack (show (Set.toList (allOperatorsInHandledEffects `Set.difference` handledOperatorsInClauses)))

    let finalEffects = clausesReturnEffects `Set.union` returnClausesEffects `Set.union` opClausesCombinedEffects
    return (targetBodyType, finalEffects)

partitionHandlerClauses :: [HandlerClause] -> ([HandlerClause], [HandlerClause])
partitionHandlerClauses = Data.List.partition isOpClause
  where
    isOpClause HandlerClause{} = True
    isOpClause _ = False

processReturnClauses :: Type -> [HandlerClause] -> Effects -> TypeCheck (Type, Effects)
processReturnClauses _ [] _ = throwError $ CustomErrorType "Internal error: processReturnClauses called with empty list."
processReturnClauses exprType clauses clausesReturnEffects = do
    typedBodies <- forM clauses $ \(HandlerReturnClause retVarName bodyExpr) ->
        withBinding retVarName exprType $ inferType bodyExpr

    -- Unify all return clause body types
    let firstBodyType = fst (head typedBodies)
    mapM_ (\(bodyT, _) -> unify firstBodyType bodyT) (tail typedBodies)
    
    -- Check effects for all return clauses
    totalEffects <- foldM (\accEffects (bodyT, bodyEffs) -> do
        unless (bodyEffs `isSubEffects` clausesReturnEffects) $
            throwError $ EffectMismatch bodyEffs clausesReturnEffects
        return (accEffects `Set.union` bodyEffs)
        ) Set.empty typedBodies
        
    return (firstBodyType, totalEffects)

processOpClauses :: Map Text OperatorSignature -> [HandlerClause] -> Type -> Effects -> TypeCheck Effects
processOpClauses _ [] _ _ = return Set.empty -- No op clauses, no additional effects from them
processOpClauses operatorSigsMap clauses targetBodyType clausesReturnEffects = do
    totalEffects <- foldM (\accEffects (HandlerClause opName argVarName contVarName bodyExpr) -> do
        opSig <- case Map.lookup opName operatorSigsMap of
            Just sig -> return sig
            Nothing -> throwError $ CustomErrorType $ "Operator '" <> opName <> "' not defined for handled effects."

        let contType = TFunction (osRetType opSig) targetBodyType clausesReturnEffects
        (currentOpClauseBodyType, currentOpClauseBodyEffects) <-
            withBindings [(argVarName, osArgType opSig), (contVarName, contType)] $ inferType bodyExpr

        unify targetBodyType currentOpClauseBodyType
        unless (currentOpClauseBodyEffects `isSubEffects` clausesReturnEffects) $
            throwError $ EffectMismatch currentOpClauseBodyEffects clausesReturnEffects
        
        return (accEffects `Set.union` currentOpClauseBodyEffects)
        ) Set.empty clauses
    return totalEffects


collectOpSigs :: Map Text EffectInfo -> Map Text OperatorSignature -> Text -> TypeCheck (Map Text OperatorSignature)
collectOpSigs effectEnv accSigs effectName = do
    effectInfo <- case Map.lookup effectName effectEnv of
        Just ei -> return ei
        Nothing -> throwError $ CustomErrorType $ "Undefined effect referenced in handle: " <> effectName
    let currentEffectOps = eiOps effectInfo
    sequence_ [ throwError $ CustomErrorType $ "Duplicate operator name '" <> opN <> "' found across handled effects."
              | opN <- Map.keys currentEffectOps, Map.member opN accSigs ]
    return $ Map.union accSigs currentEffectOps

-- | Helper to build the functional type of a constructor
buildConstructorType :: [Type] -> Type -> Type
buildConstructorType argTypes resultType = foldr (\arg acc -> TFunction arg acc Set.empty) resultType argTypes

{- | 逆に Type から引数の型の列と結果の型を取り出す
| 結果の型が分解不可能になるまで繰り返す
-}
extractConstructorType :: Type -> ([Type], Type)
extractConstructorType typ = case typ of
  TFunction argT retT _ -> let (argTypes, resultType) = extractConstructorType retT in (argT : argTypes, resultType)
  _ -> ([], typ)

-- | Register ADTs and their constructors
registerAdtsAndConstructors :: [Definition] -> TypeCheck ()
registerAdtsAndConstructors definitions = do
  let dataDefs = [def | def@(DataDef _ _) <- definitions]

  -- Phase 1: Collect ADT names, check for duplicates, and pre-populate tsAdtEnv.
  currentAdtEnv <- getAdtEnv
  mapM_ (preRegisterAdtName currentAdtEnv) dataDefs

  -- Phase 2: Process constructors for each ADT.
  mapM_ processAdtDefinition dataDefs
 where
  preRegisterAdtName :: Map Text AdtInfo -> Definition -> TypeCheck ()
  preRegisterAdtName initialAdtEnv (DataDef adtName _) = do
    when (Map.member adtName initialAdtEnv) $ -- Check against env before this batch started -- Check against env before this batch started -- Check against env before this batch started -- Check against env before this batch started -- Check against env before this batch started -- Check against env before this batch started -- Check against env before this batch started -- Check against env before this batch started
    -- Check against env before this batch started
      throwError $
        CustomErrorType $
          "Duplicate ADT definition: " <> adtName

    -- Check if already pre-registered in this current batch (in case of duplicate DataDef in input list)
    currentBatchAdtEnv <- getAdtEnv
    when (Map.member adtName currentBatchAdtEnv && not (Map.member adtName initialAdtEnv)) $
      throwError $
        CustomErrorType $
          "Duplicate ADT definition in current batch: " <> adtName

    addAdtInfo adtName (AdtInfo{adtName = adtName, adtConstructors = Map.empty}) -- Placeholder for recursive resolution
  preRegisterAdtName _ _ = return () -- Should not be called with ValDef
  processAdtDefinition :: Definition -> TypeCheck ()
  processAdtDefinition (DataDef adtName consDefs) = do
    -- Build constructor signatures and add them to tsEnv
    finalConstructorMap <- foldM (buildAndRegisterConstructor adtName) Map.empty consDefs
    -- Update the AdtInfo with the fully processed constructors
    modify $ \st -> st{tsAdtEnv = Map.adjust (\info -> info{adtConstructors = finalConstructorMap}) adtName (tsAdtEnv st)}
  processAdtDefinition _ = return () -- Skip ValDefs
  buildAndRegisterConstructor :: Text -> Map Text ConstructorSignature -> ConstructorDef -> TypeCheck (Map Text ConstructorSignature)
  buildAndRegisterConstructor adtName accumulatedConstructors (ConstructorDef consNameText typ) = do
    -- Check for global name collision in tsEnv (vars, other constructors)
    env <- getEnv

    let (argTypes, resultType) = extractConstructorType typ

    resolveType resultType (Just adtName)

    when (Map.member consNameText env) $
      throwError $
        CustomErrorType $
          "Constructor name '" <> consNameText <> "' conflicts with an existing definition or another constructor."

    -- Check for local name collision (constructors within the same ADT)
    when (Map.member consNameText accumulatedConstructors) $
      throwError $
        CustomErrorType $
          "Duplicate constructor name '" <> consNameText <> "' in ADT '" <> adtName <> "'"

    -- Resolve argument types (ADT name is visible in tsAdtEnv due to Phase 1)
    mapM_ (\argT -> resolveType argT (Just adtName)) argTypes

    let constructorSig = ConstructorSignature{csArgTypes = argTypes, csResultType = resultType}
    let functionalType = buildConstructorType argTypes resultType

    -- Add constructor's functional type to global value environment (tsEnv)
    addBinding consNameText functionalType

    return $ Map.insert consNameText constructorSig accumulatedConstructors

-- | Register Effect definitions
registerEffects :: [Definition] -> TypeCheck ()
registerEffects definitions = do
  let effectDefs = [def | def@(EffectDef _ _) <- definitions]
  mapM_ processEffectDefinition effectDefs
 where
  processEffectDefinition :: Definition -> TypeCheck ()
  processEffectDefinition (EffectDef effectName opDefs) = do
    currentEffectEnv <- getEffectEnv
    when (Map.member effectName currentEffectEnv) $
      throwError $ CustomErrorType $ "Duplicate effect definition: " <> effectName

    -- Build operator signatures
    opSigMap <- foldM (buildAndRegisterOperator effectName) Map.empty opDefs

    -- Add EffectInfo to the environment
    addEffectInfo effectName (EffectInfo{eiName = effectName, eiOps = opSigMap})
  processEffectDefinition _ = return () -- Should not be called with other Definition types

  buildAndRegisterOperator :: Text -> Map Text OperatorSignature -> OperatorDef -> TypeCheck (Map Text OperatorSignature)
  buildAndRegisterOperator effectName accumulatedOpSigs (OperatorDef opName opType) = do
    -- Resolve opType and ensure it's a function
    resolveType opType Nothing -- Effects are usually not defined in op signatures themselves
    (argTypes, retType) <- case opType of
      TFunction argT' retT' effs -> do
        -- For now, assuming operator signatures themselves don't declare effects.
        -- If they could, effs would need to be handled (e.g., ensuring they are part of `effectName`)
        -- For simplicity, let's assume effs in OperatorDef types should be empty or match the parent effect.
        -- This example assumes a simple TArg -> TRet form.
        -- A more robust check might be needed if opType can be arbitrarily complex.
        -- For now, we extract the first argument and final return type.
        let (allArgTypes, finalRetType) = extractConstructorType opType
        unless (length allArgTypes == 1) $ -- Assuming single argument for simplicity based on typical op structure
          throwError $ CustomErrorType $ "Operator '" <> opName <> "' in effect '" <> effectName <> "' must have a single argument function type."
        return (head allArgTypes, finalRetType)
      _ -> throwError $ CustomErrorType $ "Operator type for '" <> opName <> "' in effect '" <> effectName <> "' must be a function type."


    when (Map.member opName accumulatedOpSigs) $
      throwError $ CustomErrorType $ "Duplicate operator name '" <> opName <> "' in effect '" <> effectName <> "'"

    let opSig = OperatorSignature{osArgType = argTypes, osRetType = retType, osEffectName = effectName}
    
    -- Note: Unlike ADT constructors, effect operators are not added to the global tsEnv 
    -- as directly callable functions. They are invoked via 'do' or handled by 'handle'.

    return $ Map.insert opName opSig accumulatedOpSigs

-- | Type check a whole program
typeCheckProgram :: Program -> Either TypeError (Map.Map Text Type)
typeCheckProgram (Program definitions) = evalState (runExceptT go) initialState
 where
  go :: TypeCheck (Map.Map Text Type)
  go = do
    -- Pass 1: Register ADTs and their constructors
    registerAdtsAndConstructors definitions

    -- Pass 2: Register Effects and their operator signatures
    registerEffects definitions

    -- Pass 3: Populate environment with declared types of ValDefs for mutual recursion.
    -- And type check ValDef bodies.
    valDefTypes <- foldM collectValDefTypes Map.empty definitions

    currentEnv <- getEnv
    let combinedEnv = Map.union currentEnv valDefTypes
    modify $ \st -> st{tsEnv = combinedEnv}

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
        throwError $
          CustomErrorType $
            "In definition '" <> name <> "': effects are not allowed for top-level definitions but got " <> T.pack (show inferredBodyEffects)
      return $ (name, declType) : acc
    _ -> return acc
