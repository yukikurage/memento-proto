{-# LANGUAGE OverloadedStrings #-}
-- May be needed for some existing logic, e.g. in Match or Handle
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Expressions (
  inferType,
  -- Potentially helper functions if they were complex enough and are directly called by typeCheckProgram
  -- For now, only inferType. Other helpers like partitionHandlerClauses, processReturnClauses etc. are local.
  collectOpSigs, -- Exporting as it's used by inferType for Handle, but complex enough
  partitionHandlerClauses, -- Exporting as it's used by inferType for Handle
  processReturnClauses, -- Exporting as it's used by inferType for Handle
  processOpClauses, -- Exporting as it's used by inferType for Handle
) where

import Control.Monad (foldM, unless, when)
import Control.Monad.Except (throwError)
import qualified Data.List as List (partition)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Types

-- | Helper: Partition HandlerClauses into operator clauses and return clauses
partitionHandlerClauses :: [HandlerClause] -> ([HandlerClause], [HandlerClause])
partitionHandlerClauses = List.partition isOpClause
 where
  isOpClause HandlerClause{} = True
  isOpClause _ = False

-- | Helper: Process return clauses for a Handle expression
processReturnClauses :: Type -> [HandlerClause] -> TypeCheck (Type, Effects)
processReturnClauses exprType [HandlerReturnClause retVarName bodyExpr] = withBinding retVarName exprType $ inferType bodyExpr
processReturnClauses _ _ = throwError $ CustomErrorType "Internal error: processReturnClauses called with invalid arguments. Expected exactly one return clause properly structured."

-- | Helper: Process operator clauses for a Handle expression
processOpClauses :: Map.Map Text OperatorSignature -> [HandlerClause] -> Type -> Effects -> TypeCheck ()
processOpClauses _ [] _ _ = return () -- No op clauses to process
processOpClauses operatorSigsMap clauses targetBodyType targetBodyEffects = do
  mapM_ processSingleOpClause clauses
 where
  processSingleOpClause (HandlerClause opName argVarName contVarName bodyExpr) = do
    opSig <- case Map.lookup opName operatorSigsMap of
      Just sig -> return sig
      Nothing -> throwError $ CustomErrorType $ "Operator '" <> opName <> "' not defined for handled effects (should be caught by exhaustiveness check)."

    let contType = TFunction (osRetType opSig) (targetBodyType, targetBodyEffects)
    (currentOpClauseBodyType, currentOpClauseBodyEffects) <-
      withBindings [(argVarName, osArgType opSig), (contVarName, contType)] $ inferType bodyExpr

    unify targetBodyType currentOpClauseBodyType
    unless (currentOpClauseBodyEffects `Set.isSubsetOf` targetBodyEffects) $
      throwError $
        EffectMismatch targetBodyEffects currentOpClauseBodyEffects -- Corrected order
  processSingleOpClause (HandlerReturnClause _ _) =
    throwError $ CustomErrorType "Internal error: processOpClauses received a HandlerReturnClause."

-- | Helper: Collect operator signatures for handled effects
collectOpSigs :: Map.Map Text EffectInfo -> Map.Map Text OperatorSignature -> Text -> TypeCheck (Map.Map Text OperatorSignature)
collectOpSigs effectEnv accSigs effectName = do
  effectInfo <- case Map.lookup effectName effectEnv of
    Just ei -> return ei
    Nothing -> throwError $ CustomErrorType $ "Undefined effect referenced in handle: " <> effectName
  let currentEffectOps = eiOps effectInfo
  mapM_
    ( \opN ->
        when (Map.member opN accSigs) $
          throwError $
            CustomErrorType $
              "Duplicate operator name '" <> opN <> "' found across handled effects."
    )
    (Map.keys currentEffectOps)
  return $ Map.union accSigs currentEffectOps

-- | Infer the type and effects of an expression.
inferType :: Expr -> TypeCheck (Type, Effects)
inferType expr = case expr of
  Number _ -> return (TNumber, Set.empty)
  Bool _ -> return (TBool, Set.empty)
  Var name -> do
    env <- getEnv
    case Map.lookup name env of
      Nothing -> throwError $ UnboundVariable name
      Just t -> return (t, Set.empty)
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
  Apply (Lambda name mType body) arg -> do
    (argType, argEffs) <- inferType arg
    -- Use argType for the lambda parameter type if no annotation
    let lambdaParamType = fromMaybe argType mType
    -- If type annotation exists, it must match the actual argument type
    unless (isNothing mType || Just argType == mType) $
      throwError $
        TypeMismatch (fromMaybe (TAlgebraicData "Any") mType) argType

    (bodyType, bodyEffects) <- withBinding name lambdaParamType $ inferType body
    return (bodyType, bodyEffects `Set.union` argEffs)
  Lambda name mType body -> do
    -- If no type annotation, it's a bit tricky. For now, assume it needs to be inferred or is an error.
    -- Original code used fromMaybe TNumber mType. This is a default, which might not be ideal.
    -- Let's stick to it for now to maintain original behavior.
    let actualParamType = fromMaybe TNumber mType -- Defaulting to TNumber if no annotation
    (bodyType, bodyEffects) <- withBinding name actualParamType $ inferType body
    return (TFunction actualParamType (bodyType, bodyEffects), Set.empty)
  HandleApply func arg -> do
    (funcType, funcEffs) <- inferType func
    (argType, argEffs) <- inferType arg
    let accumulatedEffects = funcEffs `Set.union` argEffs
    case funcType of
      THandler (paramT, consumedEffects) (retT, generatedEffects) -> do
        unify paramT argType
        return (retT, (accumulatedEffects `Set.difference` consumedEffects) `Set.union` generatedEffects)
      _ -> throwError $ TypeMismatch funcType (THandler (argType, Set.empty) (TAlgebraicData "_", Set.empty)) -- Placeholder for expected type
  Apply func arg -> do
    (funcType, funcEffs) <- inferType func
    (argType, argEffs) <- inferType arg
    let accumulatedEffects = funcEffs `Set.union` argEffs
    case funcType of
      TFunction paramT (retT, generatedEffects) -> do
        unify paramT argType
        return (retT, accumulatedEffects `Set.union` generatedEffects)
      _ -> throwError $ TypeMismatch funcType (TFunction argType (TAlgebraicData "_", Set.empty)) -- Placeholder for expected type
  Do name -> do
    opEnv <- getOperatorEnv
    case Map.lookup name opEnv of
      Just (OperatorSignature{osArgType = paramT, osRetType = retT, osEffectName = effectName}) -> do
        return (TFunction paramT (retT, Set.singleton (Effect effectName)), Set.empty)
      Nothing -> throwError $ UndefinedEffect name
  Match scrutineeType clauses -> do
    -- Scrutinee is Type as per parser and Syntax.hs
    adtEnv <- getAdtEnv
    scrutineeName <- case scrutineeType of -- Now scrutineeType is a Type
      TAlgebraicData name -> return name
      _ -> throwError $ CustomErrorType "Scrutinee in Match must be an algebraic data type name."

    adtInfo <- case Map.lookup scrutineeName adtEnv of
      Nothing -> throwError $ CustomErrorType $ "ADT info not found for type: " <> scrutineeName
      Just info -> return info
    let adtConstructorsMap = adtConstructors adtInfo

    when (null clauses) $
      throwError $
        CustomErrorType "Match expression cannot have empty clauses"

    (firstBranchTypeOpt, totalClauseEffects) <- foldM (processClause adtConstructorsMap scrutineeType) (Nothing, Set.empty) clauses

    finalMatchExprType <- case firstBranchTypeOpt of
      Nothing -> throwError $ CustomErrorType "Could not determine type of match expression"
      Just t -> return t

    -- Exhaustiveness Check for Match
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
            "Pattern matching is not exhaustive for ADT '" <> scrutineeName <> "'. Missing: " <> T.pack (show (Set.toList (allAdtConstructors `Set.difference` coveredConstructors)))

    -- The type of a Match expression is TFunction ScrutineeType BranchType.
    -- This seems to be the convention in the original code.
    return (TFunction scrutineeType (finalMatchExprType, totalClauseEffects), Set.empty)
  Handle handlerType handlerClauses -> do
    effectEnv <- getEffectEnv
    ((argType, argEffects), (retType, retEffects)) <- extractHandlerType handlerType
    let effectNamesToHandle = Set.map (\(Effect name) -> name) argEffects
    operatorSigsMap :: Map.Map Text OperatorSignature <- foldM (collectOpSigs effectEnv) Map.empty effectNamesToHandle

    let (opClauses, returnClauses) = partitionHandlerClauses handlerClauses
    when (null returnClauses) $
      throwError $
        CustomErrorType "Handle expression must have at least one return clause."
    when (length returnClauses > 1) $
      throwError $
        CustomErrorType "Handle expression cannot have more than one return clause."

    (targetBodyType, returnClausesEffects) <- processReturnClauses argType returnClauses

    -- Process operator clauses, ensuring their bodies unify with targetBodyType and effects are subsets of returnClausesEffects
    processOpClauses operatorSigsMap opClauses targetBodyType returnClausesEffects

    -- Exhaustiveness Check for Handle
    handledOperatorsInClauses <- foldM (checkOpClauseExhaustiveness operatorSigsMap) Set.empty opClauses
    let allOperatorsInHandledEffects = Map.keysSet operatorSigsMap
    unless (allOperatorsInHandledEffects `Set.isSubsetOf` handledOperatorsInClauses) $
      throwError $
        CustomErrorType $
          "Handle expression is not exhaustive. Missing handlers for operators: "
            <> T.pack (show (Set.toList (allOperatorsInHandledEffects `Set.difference` handledOperatorsInClauses)))

    return (THandler (argType, argEffects) (retType, retEffects), Set.empty)
  Tuple exprsList -> do
    -- Infer types and effects for each expression in the tuple
    typedExprs <- mapM inferType exprsList
    -- Separate the types and effects
    let (inferredTypes, inferredEffectsList) = unzip typedExprs
    -- The resulting type is a tuple of the inferred types
    let tupleType = TTuple inferredTypes
    -- The resulting effects are the union of all inferred effects
    let combinedEffects = Set.unions inferredEffectsList
    return (tupleType, combinedEffects)

-- Helper to get bindings from a pattern
getPatternBindings :: Type -> Pattern -> Map.Map Text ConstructorSignature -> TypeCheck [(Text, Type)]
getPatternBindings actualScrutineeType pattern adtCtorMap = case pattern of
  PNumber _ -> do
    unify actualScrutineeType TNumber
    return []
  PBool _ -> do
    unify actualScrutineeType TBool
    return []
  PTuple subPatterns -> case actualScrutineeType of
    TTuple elementTypes -> do
      when (length subPatterns /= length elementTypes) $
        throwError $ CustomErrorType $ "Tuple pattern arity mismatch. Expected " <> T.pack (show (length elementTypes)) <> " elements, got " <> T.pack (show (length subPatterns))
      -- Recursively get bindings for sub-patterns
      listOfBindings <- zipWithM (\p t -> getPatternBindings t p adtCtorMap) subPatterns elementTypes
      return $ concat listOfBindings
    _ -> throwError $ TypeMismatch actualScrutineeType (TTuple []) -- Expected a tuple type
  PVar varName -> return [(varName, actualScrutineeType)]
  PWildcard -> return []
  PConstructor patConsName varName -> do
    -- This pattern only makes sense if the scrutinee is an Algebraic Data Type
    case actualScrutineeType of
      TAlgebraicData adtName -> do
        constructorSig <- case Map.lookup patConsName adtCtorMap of
          Nothing -> throwError $ CustomErrorType $ "Constructor '" <> patConsName <> "' not found in ADT '" <> adtName <> "' or ADT definition not found for type '" <> T.pack (show actualScrutineeType) <> "'"
          Just sig -> return sig
        -- Ensure the constructor belongs to the scrutinee's ADT type.
        -- This is implicitly handled if adtCtorMap is correctly populated for the specific adtName.
        return [(varName, csArgType constructorSig)]
      _ -> throwError $ TypeMismatch actualScrutineeType (TAlgebraicData "SomeADT") -- Expected an ADT for PConstructor

-- Helper for Match clause processing
processClause :: Map.Map Text ConstructorSignature -> Type -> (Maybe Type, Effects) -> Clause -> TypeCheck (Maybe Type, Effects)
processClause adtConstructorsMap scrutineeType (firstBranchTypeOpt, accClauseEffects) (Clause pattern branchExpr) = do
  localBindings <- getPatternBindings scrutineeType pattern adtConstructorsMap

  (currentBranchExprType, currentBranchExprEffects) <- withBindings localBindings $ inferType branchExpr

  newFirstBranchTypeOpt <- case firstBranchTypeOpt of
    Nothing -> return $ Just currentBranchExprType
    Just firstBranchType -> do
      unify firstBranchType currentBranchExprType
      return $ Just firstBranchType

  return (newFirstBranchTypeOpt, accClauseEffects `Set.union` currentBranchExprEffects)

-- Helper for Handle exhaustiveness check
checkOpClauseExhaustiveness :: Map.Map Text OperatorSignature -> Set.Set Text -> HandlerClause -> TypeCheck (Set.Set Text)
checkOpClauseExhaustiveness operatorSigsMap acc (HandlerClause opName _ _ _) = do
  when (Set.member opName acc) $
    throwError $
      CustomErrorType $
        "Duplicate operator '" <> opName <> "' in handle clauses."
  unless (Map.member opName operatorSigsMap) $
    throwError $
      CustomErrorType $
        "Operator '" <> opName <> "' in handle clause is not part of the handled effects."
  return $ Set.insert opName acc
checkOpClauseExhaustiveness _ acc (HandlerReturnClause _ _) = return acc -- Return clauses don't count towards op exhaustiveness
