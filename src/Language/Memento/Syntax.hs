{-# LANGUAGE DeriveGeneric #-}

module Language.Memento.Syntax where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

-- | エフェクトの定義
data Effect
  = ZeroDiv -- ゼロ除算エフェクト
  deriving (Show, Eq, Ord, Generic)

-- エフェクトのセット型
type Effects = Set Effect

-- | 型の定義
data Type
  = TNumber -- 数値型
  | TBool -- 真偽値型
  | TFunction Type Type Effects -- 関数型 (引数の型 -> 戻り値の型 & エフェクト)
  deriving (Show, Eq, Generic)

-- | 式の型
data Expr
  = Var Text -- 変数
  | Number Double -- 数値リテラル
  | Bool Bool -- 真偽値リテラル
  | BinOp BinOp Expr Expr -- 二項演算
  | If Expr Expr Expr -- if式 (condition) then expr else expr
  | Lambda Text (Maybe Type) Expr -- ラムダ式 (変数名, 型注釈(省略可), 本体)
  | Apply Expr Expr -- 関数適用
  | Do Text -- do name 構文
  deriving (Show, Eq, Generic)

-- | 二項演算子の型
data BinOp
  = Add -- 加算
  | Sub -- 減算
  | Mul -- 乗算
  | Div -- 除算
  | Eq -- 等価比較
  | Lt -- 小なり
  | Gt -- 大なり
  deriving (Show, Eq, Generic)

-- | 型エラー
data TypeError
  = TypeMismatch Type Type -- 期待する型と実際の型が異なる
  | UnboundVariable Text -- 未定義の変数
  | CannotInferType Expr -- 型を推論できない
  deriving (Show, Eq)