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

import Control.Monad (foldM, unless, when, zipWithM)
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
  Lambda pattern mArgTypeAnnotation body -> do
    (bindings, actualPatternType) <- checkLambdaPatternAndGetBindings pattern mArgTypeAnnotation
    (bodyType, bodyEffects) <- withBindings bindings $ inferType body
    return (TFunction actualPatternType (bodyType, bodyEffects), Set.empty)
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
    -- Scrutinee can be any type now: ADT, primitive (number, boolean), or tuple
    when (null clauses) $
      throwError $
        CustomErrorType "Match expression cannot have empty clauses"

    -- Process based on scrutinee type
    (adtConstructorsMap, processedClauses) <- case scrutineeType of
      -- For Algebraic Data Types
      TAlgebraicData name -> do
        adtEnv <- getAdtEnv
        adtInfo <- case Map.lookup name adtEnv of
          Nothing -> throwError $ CustomErrorType $ "ADT info not found for type: " <> name
          Just info -> return info
        return (adtConstructors adtInfo, clauses)

      -- For Primitive Types (Number, Boolean) and Tuples
      _ -> return (Map.empty, clauses)

    (firstBranchTypeOpt, totalClauseEffects) <- foldM (processClause adtConstructorsMap scrutineeType) (Nothing, Set.empty) processedClauses

    finalMatchExprType <- case firstBranchTypeOpt of
      Nothing -> throwError $ CustomErrorType "Could not determine type of match expression"
      Just t -> return t

    -- Exhaustiveness Check based on scrutinee type
    case scrutineeType of
      -- For algebraic data types
      TAlgebraicData name -> do
        let (coveredConstructors, hasWildcardOrVar) =
              foldr
                ( \(Clause p _) (accSet, accBool) -> case p of
                    PConstructor cn _ -> (Set.insert cn accSet, accBool)
                    PVar _ -> (accSet, True)
                    PWildcard -> (accSet, True)
                    _ -> (accSet, accBool) -- Other patterns don't contribute to ADT coverage
                )
                (Set.empty, False)
                clauses

        unless hasWildcardOrVar $ do
          let allAdtConstructors = Map.keysSet adtConstructorsMap
          unless (allAdtConstructors `Set.isSubsetOf` coveredConstructors) $
            throwError $
              CustomErrorType $
                "Pattern matching is not exhaustive for ADT '" <> name <> "'. Missing: " <> T.pack (show (Set.toList (allAdtConstructors `Set.difference` coveredConstructors)))

      -- For number
      TNumber -> do
        let (hasNumberPattern, hasWildcardOrVar) =
              foldr
                ( \(Clause p _) (accNum, accWild) -> case p of
                    PNumber _ -> (True, accWild)
                    PVar _ -> (accNum, True)
                    PWildcard -> (accNum, True)
                    _ -> (accNum, accWild)
                )
                (False, False)
                clauses

        unless (hasNumberPattern || hasWildcardOrVar) $
          throwError $
            CustomErrorType "Pattern matching is not exhaustive for Number type. Add a number pattern or wildcard."

      -- For boolean
      TBool -> do
        let patterns = map (\(Clause p _) -> p) clauses
        let hasTruePattern = any (\p -> case p of PBool True -> True; _ -> False) patterns
        let hasFalsePattern = any (\p -> case p of PBool False -> True; _ -> False) patterns
        let hasWildcardOrVar = any (\p -> case p of PVar _ -> True; PWildcard -> True; _ -> False) patterns

        unless (hasWildcardOrVar || (hasTruePattern && hasFalsePattern)) $
          throwError $
            CustomErrorType "Pattern matching is not exhaustive for Boolean type. Need to handle both True and False cases."

      -- For tuple
      TTuple types -> do
        let hasTuplePattern = any (\(Clause p _) -> case p of PTuple _ -> True; _ -> False) clauses
        let hasWildcardOrVar = any (\(Clause p _) -> case p of PVar _ -> True; PWildcard -> True; _ -> False) clauses

        unless (hasTuplePattern || hasWildcardOrVar) $
          throwError $
            CustomErrorType $
              "Pattern matching is not exhaustive for Tuple type with " <> T.pack (show (length types)) <> " elements."

      -- For other types, we don't enforce specific exhaustiveness rules
      _ -> return ()

    -- The type of a Match expression is TFunction ScrutineeType BranchType.
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
        throwError $
          CustomErrorType $
            "Tuple pattern arity mismatch. Expected " <> T.pack (show (length elementTypes)) <> " elements, got " <> T.pack (show (length subPatterns))
      -- Recursively get bindings for sub-patterns
      listOfBindings <- zipWithM (\p t -> getPatternBindings t p adtCtorMap) subPatterns elementTypes -- Pass adtCtorMap through
      return $ concat listOfBindings
    _ -> throwError $ TypeMismatch actualScrutineeType (TTuple []) -- Expected a tuple type
  PVar varName -> return [(varName, actualScrutineeType)]
  PWildcard -> return []
  PConstructor patConsName varName -> do
    case actualScrutineeType of
      TAlgebraicData adtName -> do -- actualScrutineeType is the expected ADT
        constructorSig <- case Map.lookup patConsName adtCtorMap of
          Nothing -> throwError $ CustomErrorType $ "Constructor '" <> patConsName <> "' not found in ADT '" <> adtName <> "'."
          Just sig -> return sig
        -- Sanity check: the constructor's result type must match the ADT type we are checking against
        unless (csResultType constructorSig == TAlgebraicData adtName) $
          throwError $ CustomErrorType $ "Internal error: Constructor '" <> patConsName <> "' from ADT '" <> adtName <> "' has an unexpected result type '" <> T.pack (show (csResultType constructorSig)) <> "'."
        return [(varName, csArgType constructorSig)]
      _ -> throwError $ TypeMismatch actualScrutineeType (TAlgebraicData "SomeADT") -- Expected an ADT for PConstructor when matching

-- New helper function for lambda pattern checking
checkLambdaPatternAndGetBindings :: Pattern -> Maybe Type -> TypeCheck ([(Text, Type)], Type)
checkLambdaPatternAndGetBindings pattern optAnnotation = case (pattern, optAnnotation) of
  -- Case 1: Type annotation is provided
  (p, Just annotatedType) -> do
    adtEnv <- getAdtEnv
    -- Determine the relevant ADT constructor map if the annotated type is an ADT
    let adtCtorMap = case annotatedType of
          TAlgebraicData adtName -> maybe Map.empty adtConstructors (Map.lookup adtName adtEnv)
          _ -> Map.empty -- Not an ADT or ADT name not found, so no specific ADT constructors apply
    -- Use getPatternBindings to check pattern against the annotated type and get bindings
    bindings <- getPatternBindings annotatedType p adtCtorMap
    return (bindings, annotatedType)

  -- Case 2: No type annotation (inference - simple cases for now)
  (PVar varName, Nothing) -> do
    -- Defaulting to TNumber as per previous behavior for unannotated lambdas (name -> ...)
    -- This is a simplification and might need refinement for true polymorphic lambdas.
    let inferredType = TNumber
    return ([(varName, inferredType)], inferredType)
  (PWildcard, Nothing) -> do
    let inferredType = TNumber -- Similar default for wildcard
    return ([], inferredType)
  (PNumber _, Nothing) -> return ([], TNumber)
  (PBool _, Nothing) -> return ([], TBool)
  (PTuple subPatterns, Nothing) -> do
    -- Recursively infer for sub-patterns
    results <- mapM (`checkLambdaPatternAndGetBindings` Nothing) subPatterns
    let bindings = concatMap fst results
    let types = map snd results
    return (bindings, TTuple types)
  (PConstructor ctorName varName, Nothing) -> do
    -- Infer from global constructor environment
    env <- getEnv -- This env contains variable types and constructor types
    case Map.lookup ctorName env of
      Just (TFunction argType (resultType@(TAlgebraicData _), _)) -> do
        -- Ensure resultType is indeed TAlgebraicData, which it should be for constructors
        return ([(varName, argType)], resultType)
      Just otherType -> throwError $ CustomErrorType $ "Expected constructor '" <> ctorName <> "' to have a function type, but got " <> T.pack (show otherType)
      Nothing -> throwError $ CustomErrorType $ "Constructor '" <> ctorName <> "' not found in environment for inference."

  -- Catch-all for complex patterns without annotation (e.g. PConstructor inside PTuple without annotation)
  -- This could be refined, but for now, require annotations for more complex unannotated patterns.
  (p, Nothing) -> throwError $ CannotInferType (Lambda p optAnnotation (Var "dummy")) -- Dummy expr

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
