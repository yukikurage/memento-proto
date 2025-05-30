{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- May be needed for some existing logic, e.g. in Match or Handle
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Expressions where

import Control.Monad (foldM, forM_, unless, when, zipWithM)
import Control.Monad.Except (throwError)
import qualified Data.List as List (partition)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (traceM)
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Handle (collectHandleConstraint)
import Language.Memento.TypeChecker.Match (collectMatchConstraint)
import Language.Memento.TypeChecker.Monad (ConstraintCollectorError (UnboundVariable), ConstraintCollectorM, addCSEffectConstraints, addCSTypeConstraints, addEffectPreference, addTypePreference, getEnv, withBinds)
import Language.Memento.TypeChecker.Patterns (collectPatternConstraintAndGetBindings)
import Language.Memento.TypeChecker.Solver (ConstraintOperator (COEqual, COGreaterThanOrEqual, COLessThanOrEqual), Preference (Maximize, Minimize), UnsolvedEffects (UEFull, UESet, UEUnion, UEVar), UnsolvedEffectsVariable (UnsolvedEffectsVariable), UnsolvedType (UTBool, UTFunction, UTHandler, UTNum, UTTuple, UTVar), UnsolvedTypeVariable (UnsolvedTypeVariable))

collectExpressionConstraint :: ExprWithMetadata -> ConstraintCollectorM (UnsolvedTypeVariable, UnsolvedEffectsVariable)
collectExpressionConstraint (ExprWithMetadata expr meta@(ExprMetadata _ uniqueId)) = do
  let
    typeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId -- 全体の型引数を作って名前を付ける
  addTypePreference typeVariable Minimize -- 最小化を目指す
  let
    effectVariable = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_effect"
  addEffectPreference effectVariable Minimize
  case expr of
    Number _ -> do
      -- 全体の型は数値リテラルより大きく無ければいけない
      addCSTypeConstraints (typeVariable, UTNum, COGreaterThanOrEqual)
      -- 全体のエフェクトは空より大きく無ければ行けない
      addCSEffectConstraints (effectVariable, UESet Set.empty, COGreaterThanOrEqual)
    Bool _ -> do
      -- 全体の型は真偽値リテラルより大きく無ければいけない
      addCSTypeConstraints (typeVariable, UTBool, COGreaterThanOrEqual)
      -- 全体のエフェクトは空より大きく無ければ行けない
      addCSEffectConstraints (effectVariable, UESet Set.empty, COGreaterThanOrEqual)
    Var (VariableWithMetadata var _) -> do
      -- 変数の型を取得する
      env <- getEnv
      case Map.lookup var env of
        Nothing -> throwError $ UnboundVariable var
        Just t -> do
          -- 変数の型との一致を Constraint に追加
          addCSTypeConstraints (typeVariable, UTVar t, COGreaterThanOrEqual)
          -- 変数のエフェクトは空より大きく無ければ行けない
          addCSEffectConstraints (effectVariable, UESet Set.empty, COGreaterThanOrEqual)
    BinOp op e1 e2 -> do
      (t1, eff1) <- collectExpressionConstraint e1
      (t2, eff2) <- collectExpressionConstraint e2
      case op of
        Add -> do
          addCSTypeConstraints (t1, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (t2, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (typeVariable, UTNum, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff1, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff2, COGreaterThanOrEqual)
        Sub -> do
          addCSTypeConstraints (t1, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (t2, UTNum, COGreaterThanOrEqual)
          addCSTypeConstraints (typeVariable, UTNum, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff1, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff2, COGreaterThanOrEqual)
        Mul -> do
          addCSTypeConstraints (t1, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (t2, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (typeVariable, UTNum, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff1, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff2, COGreaterThanOrEqual)
        Div -> do
          addCSTypeConstraints (t1, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (t2, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (typeVariable, UTNum, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff1, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff2, COGreaterThanOrEqual)
        Eq -> do
          addCSTypeConstraints (t1, UTNum, COEqual)
          addCSTypeConstraints (t2, UTNum, COEqual)
          addCSTypeConstraints (typeVariable, UTBool, COEqual)
          addCSEffectConstraints (effectVariable, UEVar eff1, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff2, COGreaterThanOrEqual)
        Lt -> do
          addCSTypeConstraints (t1, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (t2, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (typeVariable, UTBool, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff1, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff2, COGreaterThanOrEqual)
        Gt -> do
          addCSTypeConstraints (t1, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (t2, UTNum, COLessThanOrEqual)
          addCSTypeConstraints (typeVariable, UTBool, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff1, COGreaterThanOrEqual)
          addCSEffectConstraints (effectVariable, UEVar eff2, COGreaterThanOrEqual)
    If cond thenExpr elseExpr -> do
      (condType, condEff) <- collectExpressionConstraint cond
      (thenType, thenEff) <- collectExpressionConstraint thenExpr
      (elseType, elseEff) <- collectExpressionConstraint elseExpr
      addCSTypeConstraints (condType, UTBool, COEqual)
      addCSTypeConstraints (typeVariable, UTVar thenType, COGreaterThanOrEqual)
      addCSTypeConstraints (typeVariable, UTVar elseType, COGreaterThanOrEqual)
      addCSEffectConstraints (effectVariable, UEVar condEff, COGreaterThanOrEqual)
      addCSEffectConstraints (effectVariable, UEVar thenEff, COGreaterThanOrEqual)
      addCSEffectConstraints (effectVariable, UEVar elseEff, COGreaterThanOrEqual)
    Lambda pattern mAnn body -> do
      (typeVariableOfArg, bindingsOfPattern) <- collectPatternConstraintAndGetBindings pattern
      (typeVariableOfBody, typeVariableOfEffect) <- withBinds bindingsOfPattern $ collectExpressionConstraint body
      addCSTypeConstraints (typeVariable, UTFunction (UTVar typeVariableOfArg) (UTVar typeVariableOfBody, UEVar typeVariableOfEffect), COGreaterThanOrEqual)
    Apply func arg -> do
      (funcType, funcEff) <- collectExpressionConstraint func
      (argType, argEff) <- collectExpressionConstraint arg
      addCSTypeConstraints (funcType, UTFunction (UTVar argType) (UTVar typeVariable, UEVar effectVariable), COLessThanOrEqual)
      addCSEffectConstraints (effectVariable, UEVar funcEff, COGreaterThanOrEqual)
      addCSEffectConstraints (effectVariable, UEVar argEff, COGreaterThanOrEqual)
    HandleApply func arg -> do
      (funcType, funcEff) <- collectExpressionConstraint func
      (argType, argEff) <- collectExpressionConstraint arg
      let
        -- ハンドルすることのできる Effect を最大化
        handledEffectVariable = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_handled"
      addEffectPreference handledEffectVariable Maximize
      addCSTypeConstraints (funcType, UTHandler (UTVar argType, UEVar handledEffectVariable) (UTVar typeVariable, UEVar handledEffectVariable), COLessThanOrEqual)
      addCSEffectConstraints (effectVariable, UEVar funcEff, COGreaterThanOrEqual) -- ハンドル自体を作るのに必要な Effect
      -- argEff (引数が起こせる Effect) は、ハンドルすることができる Effect + 全体の Effect より小さくなければいけない
      addCSEffectConstraints
        ( argEff
        , UEUnion (UEVar handledEffectVariable) (UEVar effectVariable)
        , COLessThanOrEqual
        )
    Tuple exprsList -> do
      exprsConstraints <- mapM collectExpressionConstraint exprsList
      addCSTypeConstraints (typeVariable, UTTuple (map (UTVar . fst) exprsConstraints), COGreaterThanOrEqual)
      forM_ exprsConstraints $ \(_, e) -> do
        addCSEffectConstraints (effectVariable, UEVar e, COGreaterThanOrEqual)
    Match clauses -> do
      collectMatchConstraint collectExpressionConstraint clauses meta typeVariable
    Handle clauses -> do
      collectHandleConstraint collectExpressionConstraint clauses meta typeVariable

  return (typeVariable, effectVariable)

-- -- | Infer the type and effects of an expression, optionally checking against an expected type.
-- inferType :: Expr -> Maybe Type -> TypeCheck (Type, Effects)
-- inferType expr mExpectedType = do
--   (inferredType, effects) <- case expr of
--     Number n -> return (TNumber, Set.empty)
--     Bool b -> return (TBool, Set.empty)
--     Var name -> do
--       env <- getEnv
--       case Map.lookup name env of
--         Nothing -> throwError $ UnboundVariable name
--         Just t -> return (t, Set.empty)
--     BinOp op e1 e2 -> do
--       -- Expected types for sub-expressions based on operator
--       let (mExpectedT1, mExpectedT2) = case op of
--             Add -> (Just TNumber, Just TNumber)
--             Sub -> (Just TNumber, Just TNumber)
--             Mul -> (Just TNumber, Just TNumber)
--             Div -> (Just TNumber, Just TNumber)
--             Lt -> (Just TNumber, Just TNumber)
--             Gt -> (Just TNumber, Just TNumber)
--             Eq -> (Nothing, Nothing) -- Types of e1 and e2 must unify, but can be anything initially
--       (t1, eff1) <- inferType e1 mExpectedT1
--       (t2, eff2) <- inferType e2 mExpectedT2

--       let currentEffects = eff1 `Set.union` eff2
--       resType <- case op of
--         Add -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
--         Sub -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
--         Mul -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
--         Div -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
--         Eq -> unify t1 t2 >> return TBool
--         Lt -> unify TNumber t1 >> unify TNumber t2 >> return TBool
--         Gt -> unify TNumber t1 >> unify TNumber t2 >> return TBool
--       return (resType, currentEffects)
--     If cond thenExpr elseExpr -> do
--       (condType, condEff) <- inferType cond (Just TBool)
--       -- If mExpectedType for If is Just expected, thenExpr and elseExpr should conform to it
--       (thenType, thenEff) <- inferType thenExpr mExpectedType
--       (elseType, elseEff) <- inferType elseExpr (Just thenType) -- elseExpr must match thenExpr's type
--       unify thenType elseType -- Double check, though inferType for elseExpr should ensure it
--       return (thenType, condEff `Set.union` thenEff `Set.union` elseEff)
--     Apply (Lambda pattern mAnn lambdaBody) argExpr -> do
--       -- Infer argType first, this gives an expectation for the lambda's pattern
--       (argType, argEffs) <- inferType argExpr Nothing
--       (bindings, patternActualType) <- checkPatternAndGetBindings pattern (Just argType)
--       unify patternActualType argType
--       checkPatternCoverage argType [pattern]
--       -- If Apply itself has an mExpectedType, body should conform to it.
--       (lambdaBodyType, lambdaBodyEffects) <- withBindings bindings $ inferType lambdaBody mExpectedType
--       return (lambdaBodyType, argEffs `Set.union` lambdaBodyEffects)
--     Lambda pattern mAnn body -> do
--       let (mExpectedParamType, mExpectedBodyType) = case mExpectedType of
--             Just (TFunction ept (ebt, _)) -> (Just ept, Just ebt)
--             _ -> (Nothing, Nothing)

--       let patternContextType = case mAnn of -- Annotation takes precedence
--             Just annT -> Just annT
--             Nothing -> mExpectedParamType

--       (bindings, actualPatternType) <- checkPatternAndGetBindings pattern patternContextType

--       -- Ensure annotation, if present, matches actual pattern type
--       case mAnn of
--         Just annT -> unify annT actualPatternType
--         Nothing -> return ()
--       -- Ensure expected param type, if present, matches actual pattern type
--       case mExpectedParamType of
--         Just ept -> unify ept actualPatternType
--         Nothing -> return ()

--       checkPatternCoverage actualPatternType [pattern]
--       (bodyType, bodyEffects) <- withBindings bindings $ inferType body mExpectedBodyType
--       return (TFunction actualPatternType (bodyType, bodyEffects), Set.empty)
--     HandleApply func arg -> do
--       -- Infer argType first
--       (argType, argEffs) <- inferType arg Nothing
--       -- Try to infer func as a THandler expecting argType
--       -- If mExpectedType is Just retT, func should be THandler argType (retT, _)
--       let mExpectedFuncType = case mExpectedType of
--             Just expectedRetT -> Just (THandler (argType, Set.empty) (expectedRetT, Set.empty)) -- Effects are wildcards for now
--             Nothing -> Nothing
--       (funcType, funcEffs) <- inferType func mExpectedFuncType

--       let accumulatedEffects = funcEffs `Set.union` argEffs
--       case funcType of
--         THandler (paramT, consumedEffects) (retT, generatedEffects) -> do
--           unify paramT argType
--           return (retT, (accumulatedEffects `Set.difference` consumedEffects) `Set.union` generatedEffects)
--         _ -> throwError $ TypeMismatch funcType (THandler (argType, Set.empty) (TAlgebraicData "infer", Set.empty))
--     Apply func argExpr -> do
--       -- If the whole Apply expression has an expected type, the function's return type is constrained
--       let mExpectedFuncRetType = mExpectedType

--       -- We need to infer the function type first, possibly without a specific expected structure initially,
--       -- or derive a partial expectation for the function if mExpectedFuncRetType is available.
--       (funcType, funcEffs) <- inferType func Nothing -- Could refine this later
--       case funcType of
--         TFunction expectedArgType (actualRetType, funcBodyEffects) -> do
--           (argType, argEffs) <- inferType argExpr (Just expectedArgType) -- Arg must match function's param type
--           -- The actual return type of the function is unified with the overall expected type
--           case mExpectedFuncRetType of
--             Just ert -> unify actualRetType ert
--             Nothing -> return ()
--           return (actualRetType, funcEffs `Set.union` argEffs `Set.union` funcBodyEffects)
--         _ -> do
--           -- If funcType is not TFunction, try to infer arg then throw error
--           traceM $ "funcType: " <> show funcType
--           (argType, _) <- inferType argExpr Nothing
--           throwError $ TypeMismatch funcType (TFunction argType (fromMaybe (TAlgebraicData "infer") mExpectedFuncRetType, Set.empty))
--     Do name -> do
--       opEnv <- getOperatorEnv
--       case Map.lookup name opEnv of
--         Just (OperatorSignature{osArgType = paramT, osRetType = retT, osEffectName = effectName}) ->
--           return (TFunction paramT (retT, Set.singleton (Effect effectName)), Set.empty)
--         Nothing -> throwError $ UndefinedEffectOperator name
--     Match scrutineeType clauses -> do
--       -- The scrutineeType is given directly by the parser.
--       -- The mExpectedType for the Match expression is the expected type for its branches.
--       (matchBodyType, matchBodyEffs) <- inferMatchType inferType scrutineeType clauses mExpectedType
--       return (matchBodyType, matchBodyEffs)
--     Handle handlerType clauses ->
--       -- mExpectedType for Handle expression is the expected return type of the handler.
--       inferHandleType inferType handlerType clauses mExpectedType
--     Tuple exprsList -> do
--       let mExpectedElementTypes = case mExpectedType of
--             Just (TTuple ts) | length ts == length exprsList -> map Just ts
--             _ -> replicate (length exprsList) Nothing

--       results <- zipWithM (\e met -> inferType e met) exprsList mExpectedElementTypes
--       let inferredTypes = map fst results
--       let inferredEffects = Set.unions (map snd results)
--       return (TTuple inferredTypes, inferredEffects)

--   -- Final unification with the overall expected type, if provided
--   case mExpectedType of
--     Just expectedT -> unifyOnlyType expectedT inferredType >> return (expectedT, effects) -- Return expectedT after successful unification
--     Nothing -> return (inferredType, effects)
