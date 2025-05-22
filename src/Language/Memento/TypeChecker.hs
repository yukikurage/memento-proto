{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker (
  typeCheck,
  typeCheckProgram,
  TypeError (..),
) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax

type TypeEnv = Map.Map Text Type
type TypeCheck a = ExceptT TypeError (State TypeEnv) a

-- | プログラム全体の型チェック
typeCheckProgram :: Expr -> Either TypeError Type
typeCheckProgram = typeCheck

-- | 式の型チェック
typeCheck :: Expr -> Either TypeError Type
typeCheck expr = evalState (runExceptT (inferType expr)) Map.empty

-- | 型の一致を確認
unify :: Type -> Type -> TypeCheck ()
unify expected actual =
  when (expected /= actual) $
    throwError $
      TypeMismatch expected actual

-- | 型推論
inferType :: Expr -> TypeCheck Type
inferType expr = case expr of
  Number _ -> return TNumber
  Bool _ -> return TBool
  Var name -> do
    env <- get
    case Map.lookup name env of
      Nothing -> throwError $ UnboundVariable name
      Just t -> return t
  BinOp op e1 e2 -> do
    t1 <- inferType e1
    t2 <- inferType e2
    case op of
      Add -> do
        unify TNumber t1
        unify TNumber t2
        return TNumber
      Sub -> do
        unify TNumber t1
        unify TNumber t2
        return TNumber
      Mul -> do
        unify TNumber t1
        unify TNumber t2
        return TNumber
      Div -> do
        unify TNumber t1
        unify TNumber t2
        return TNumber
      Eq -> do
        unify t1 t2
        return TBool
      Lt -> do
        unify TNumber t1
        unify TNumber t2
        return TBool
  If cond then_ else_ -> do
    condType <- inferType cond
    unify TBool condType
    thenType <- inferType then_
    elseType <- inferType else_
    unify thenType elseType
    return thenType
  Lambda name mType body -> do
    case mType of
      Just paramType -> do
        oldEnv <- get
        modify (Map.insert name paramType)
        bodyType <- inferType body
        put oldEnv -- スコープから外れたら元の環境に戻す
        return $ TFunction paramType bodyType
      Nothing -> do
        -- 引数の型が省略された場合、コンテキストから推論
        -- ここでは単純化のため、数値型を仮定
        oldEnv <- get
        modify (Map.insert name TNumber)
        bodyType <- inferType body
        put oldEnv -- スコープから外れたら元の環境に戻す
        return $ TFunction TNumber bodyType
  -- パイプライン演算子の特殊処理
  Apply (Lambda name mType body) arg -> do
    -- 引数の型を推論
    argType <- inferType arg
    -- 型注釈があればそれを使う、なければ引数の型を使う
    let paramType = case mType of
          Just t -> t
          Nothing -> argType
    -- 引数の型と型注釈が一致するか確認
    unify paramType argType
    -- 環境に変数を追加して本体を評価
    oldEnv <- get
    modify (Map.insert name paramType)
    bodyType <- inferType body
    put oldEnv
    return bodyType
  Apply func arg -> do
    funcType <- inferType func
    argType <- inferType arg
    case funcType of
      TFunction paramType returnType -> do
        unify paramType argType
        return returnType
      _ -> throwError $ TypeMismatch (TFunction argType TNumber) funcType