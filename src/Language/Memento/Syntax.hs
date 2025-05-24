{-# LANGUAGE DeriveGeneric #-}

module Language.Memento.Syntax (
  Effect (..),
  Effects,
  Type (..),
  Expr (..),
  BinOp (..),
  Definition (..), -- Added Definition
  Program (..), -- Added Program
  TypeError (..),
  Pattern (..), -- Added Pattern
  Clause (..), -- Added Clause
  ConstructorDef (..), -- Added ConstructorDef
  OperatorDef (..), -- Added OperatorDef
  HandlerClause (..), -- Added HandlerClause
)
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

-- | エフェクトの定義
data Effect = Effect Text deriving (Show, Eq, Ord, Generic) -- Modified to take Text

-- | コンストラクタ定義
data ConstructorDef = ConstructorDef Text Type deriving (Show, Eq, Generic)

-- | エフェクトオペレータのシグネチャ定義
data OperatorDef = OperatorDef Text Type deriving (Show, Eq, Generic)

-- | ハンドラ節の定義
data HandlerClause
  = HandlerClause Text Text Text Expr -- オペレータ名, 引数変数, 継続変数, 式
  | HandlerReturnClause Text Expr -- 変数名, 式
  deriving (Show, Eq, Generic)

-- エフェクトのセット型
type Effects = Set Effect

-- | 型の定義
data Type
  = TNumber -- 数値型
  | TBool -- 真偽値型
  | THandler (Type, Effects) (Type, Effects) -- ハンドラ型 (引数の型 & 消費するエフェクト -> 戻り値の型 & 生成するエフェクト)
  | TFunction Type (Type, Effects) -- 関数型 (引数の型  -> 戻り値の型 & 生成するエフェクト)
  | TAlgebraicData Text -- 代数的データ型
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
  | HandleApply Expr Expr -- ハンドル適用 (ハンドラ, 引数)
  | Do Text -- do name 構文
  | Match Type [Clause] -- match式 (マッチする型, clauses)
  | Handle Type [HandlerClause] -- ハンドル式 (期待されるハンドラの型 (Function), ハンドラ節)
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

-- | パターンの定義
data Pattern
  = PConstructor Text Text -- コンストラクタパターン (コンストラクタ名, 変数名)
  | PVar Text -- 変数パターン (変数名)
  | PWildcard -- ワイルドカードパターン (_)
  | PNumber Double -- 数値パターン
  | PBool Bool -- 真偽値パターン
  | PTuple [Pattern] -- タプルパターン
  deriving (Show, Eq, Generic)

-- | match式の節の定義
data Clause = Clause Pattern Expr -- パターン, 式
  deriving (Show, Eq, Generic)

-- | 値定義の型
data Definition
  = ValDef Text Type Expr -- 変数名, 型, 式
  | DataDef Text [ConstructorDef] -- データ型名, コンストラクタ定義のリスト
  | EffectDef Text [OperatorDef] -- エフェクト名, オペレータ定義のリスト
  deriving (Show, Eq, Generic)

-- | プログラムの型 (トップレベル定義のリスト)
newtype Program = Program {getDefinitions :: [Definition]}
  deriving (Show, Eq, Generic)

-- | 型エラー
data TypeError
  = TypeMismatch Type Type -- 期待する型と実際の型が異なる
  | UnboundVariable Text -- 未定義の変数
  | CannotInferType Expr -- 型を推論できない
  | UndefinedEffect Text -- 未定義のエフェクト
  | EffectMismatch Effects Effects -- エフェクトが一致しない (実際のエフェクト, 期待されるエフェクト)
  | CustomErrorType Text -- カスタムメッセージ
  deriving (Show, Eq)