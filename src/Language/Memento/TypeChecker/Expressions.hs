{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- May be needed for some existing logic, e.g. in Match or Handle
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Expressions where

{-

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

-}