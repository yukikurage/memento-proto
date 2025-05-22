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
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax

-- | 型チェックの状態（型環境とエフェクトの集合）
data TypeState = TypeState
  { tsEnv :: Map.Map Text Type -- 型環境
  , tsEffects :: Effects -- エフェクトセット
  }

type TypeCheck a = ExceptT TypeError (State TypeState) a

-- エフェクト操作のマッピング (名前, 引数の型, 戻り値の型, エフェクト)
effectOps :: [(Text, (Type, Type, Effect))]
effectOps = [("throw", (TNumber, TNumber, Throw))]

-- エフェクト操作を名前で検索
lookupEffectOp :: Text -> Maybe (Type, Type, Effect)
lookupEffectOp name = lookup name effectOps

-- | 初期状態
initialState :: TypeState
initialState =
  TypeState
    { tsEnv = Map.empty
    , tsEffects = Set.empty
    }

-- | エフェクトを追加
addEffect :: Effect -> TypeCheck ()
addEffect eff = modify $ \st -> st{tsEffects = Set.insert eff (tsEffects st)}

-- | 現在のエフェクトセットを取得
getEffects :: TypeCheck Effects
getEffects = gets tsEffects

-- | 型環境の取得
getEnv :: TypeCheck (Map.Map Text Type)
getEnv = gets tsEnv

-- | 型環境にバインディングを追加
addBinding :: Text -> Type -> TypeCheck ()
addBinding name typ = modify $ \st -> st{tsEnv = Map.insert name typ (tsEnv st)}

-- | 一時的に環境を変更して計算を実行し、その結果を返す
withBinding :: Text -> Type -> TypeCheck a -> TypeCheck a
withBinding name typ action = do
  oldEnv <- getEnv
  addBinding name typ
  result <- action
  modify $ \st -> st{tsEnv = oldEnv}
  return result

-- | プログラム全体の型チェック
typeCheckProgram :: Expr -> Either TypeError (Type, Effects)
typeCheckProgram expr =
  let (result, state) = runState (runExceptT (inferType expr)) initialState
   in case result of
        Left err -> Left err
        Right typ -> Right (typ, tsEffects state)

-- | 式の型チェック
typeCheck :: Expr -> Either TypeError (Type, Effects)
typeCheck = typeCheckProgram

-- | 型の一致を確認
unify :: Type -> Type -> TypeCheck ()
unify (TFunction argT1 retT1 eff1) (TFunction argT2 retT2 eff2) = do
  unify argT1 argT2
  unify retT1 retT2
-- エフェクトは単一化の対象ではないため、チェックしない
unify expected actual =
  when (expected /= actual) $
    throwError $
      TypeMismatch expected actual

-- | 型推論 - 型を推論し、エフェクトはStateに累積される
inferType :: Expr -> TypeCheck Type
inferType expr = case expr of
  Number _ -> return TNumber
  Bool _ -> return TBool
  Var name -> do
    -- (var) x : t, Γ ⊦ x : t & ∅
    env <- getEnv
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
      Gt -> do
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
    -- (abs) (Γ, x : s ⊦ e : t & a) => (Γ ⊦ (Lambda x e) : function(s, t, a))
    oldEffs <- getEffects
    case mType of
      Just paramType -> do
        -- ラムダ式の本体を型チェック（新しい変数をスコープに追加）
        bodyType <- withBinding name paramType $ inferType body

        -- ラムダ本体で累積されたエフェクトを取得
        effects <- getEffects

        modify $ \st -> st{tsEffects = oldEffs}

        -- 関数型を返す
        return $ TFunction paramType bodyType effects
      Nothing -> do
        -- 引数の型が省略された場合、コンテキストから推論
        -- ここでは単純化のため、数値型を仮定
        bodyType <- withBinding name TNumber $ inferType body

        -- ラムダ本体で累積されたエフェクトを取得
        effects <- getEffects

        modify $ \st -> st{tsEffects = oldEffs}

        return $ TFunction TNumber bodyType effects

  -- パイプライン演算子の特殊処理
  Apply (Lambda name mType body) arg -> do
    -- (apply) (Γ ⊦ x : s & a), (Δ ⊦ f : function(s, t, b) & c) => (Γ, Δ ⊦ Apply f x : t & (a ∪ b ∪ c))
    argType <- inferType arg

    -- 型注釈があればそれを使う、なければ引数の型を使う
    let paramType = case mType of
          Just t -> t
          Nothing -> argType

    -- 引数の型と型注釈が一致するか確認
    unify paramType argType

    -- 環境に変数を追加して本体を評価
    bodyType <- withBinding name paramType $ inferType body

    return bodyType
  Apply func arg -> do
    -- (apply) (Γ ⊦ x : s & a), (Δ ⊦ f : function(s, t, b) & c) => (Γ, Δ ⊦ Apply f x : t & (a ∪ b ∪ c))
    funcType <- inferType func
    argType <- inferType arg

    case funcType of
      TFunction paramType returnType bodyEffects -> do
        unify paramType argType

        -- 関数本体で定義されたエフェクトを現在のステートに追加
        mapM_ addEffect (Set.toList bodyEffects)

        return returnType
      _ -> throwError $ TypeMismatch (TFunction argType TNumber Set.empty) funcType
  Do name -> do
    -- デフォルトの型とエフェクト
    case lookupEffectOp name of
      Just (argType, retType, effect) -> do
        -- 関数型を返す
        return $ TFunction argType retType (Set.singleton effect)
      Nothing -> throwError $ UndefinedEffect name
