{-# LANGUAGE DeriveGeneric #-}

module Language.Memento.Syntax (
  Effect (Effect),
  Effects,
  Type (..),
  Expr (..),
  BinOp (..),
  Definition (..), -- Added Definition
  Program (..), -- Added Program
  Pattern (..), -- Added Pattern
  Clause (..), -- Added Clause
  ConstructorDef (..), -- Added ConstructorDef
  OperatorDef (..), -- Added OperatorDef
  HandlerClause (..), -- Added HandlerClause
  TypeWithMetadata (..),
  TypeMetadata (..),
  TypeVariable (..),
  TypeVariableWithMetadata (..),
  TypeVariableMetadata (..),
  TypeArgumentWithMetadata (..),
  TypeArgumentMetadata (..),
  Variable (..),
  VariableWithMetadata (..),
  VariableMetadata (..),
  ArgumentWithMetadata (..),
  ArgumentMetadata (..),
  ConstructorDefWithMetadata (..),
  ConstructorDefMetadata (..),
  OperatorDefWithMetadata (..),
  OperatorDefMetadata (..),
  HandlerClauseWithMetadata (..),
  HandlerClauseMetadata (..),
  ExprWithMetadata (..),
  ExprMetadata (..),
  PatternWithMetadata (..),
  PatternMetadata (..),
  ClauseWithMetadata (..),
  ClauseMetadata (..),
  EffectsWithMetadata (..),
  EffectsMetadata (..),
  EffectWithMetadata (..),
  EffectMetadata (..),
  DefinitionWithMetadata (..),
  DefinitionMetadata (..),
)
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Megaparsec (PosState, SourcePos)

newtype TypeVariable = TypeVariable Text deriving (Show, Eq, Ord, Generic)
data TypeArgumentMetadata = TypeArgumentMetadata
  { typeArgStartPosition :: SourcePos
  , typeArgUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data TypeArgumentWithMetadata = TypeArgumentWithMetadata TypeVariable TypeArgumentMetadata deriving (Show, Eq, Ord, Generic)

data TypeVariableMetadata = TypeVariableMetadata
  { typeVarStartPosition :: SourcePos
  , typeVarUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data TypeVariableWithMetadata = TypeVariableWithMetadata TypeVariable TypeVariableMetadata deriving (Show, Eq, Ord, Generic)

-- | エフェクトの定義
newtype Effect
  = Effect TypeVariableWithMetadata
  deriving (Show, Eq, Ord, Generic)

data EffectMetadata = EffectMetadata
  { effectStartPosition :: SourcePos
  , effectUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data EffectWithMetadata = EffectWithMetadata Effect EffectMetadata deriving (Show, Eq, Ord, Generic)

-- エフェクトのセット型
type Effects = Set EffectWithMetadata

data EffectsWithMetadata = EffectsWithMetadata Effects EffectsMetadata deriving (Show, Eq, Ord, Generic)

data EffectsMetadata = EffectsMetadata
  { effectsStartPosition :: SourcePos
  , effectsUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

-- | 型の定義
data Type
  = TNumber -- 数値型
  | TBool -- 真偽値型
  | THandler (TypeWithMetadata, EffectsWithMetadata) (TypeWithMetadata, EffectsWithMetadata) -- ハンドラ型 (引数の型 & 消費するエフェクト -> 戻り値の型 & 生成するエフェクト)
  | TFunction TypeWithMetadata (TypeWithMetadata, EffectsWithMetadata) -- 関数型 (引数の型  -> 戻り値の型 & 生成するエフェクト)
  | TAlgebraicData TypeVariableWithMetadata -- 代数的データ型
  | TTuple [TypeWithMetadata] -- タプル型
  deriving (Show, Eq, Ord, Generic)

data TypeMetadata = TypeMetadata
  { typeStartPosition :: SourcePos
  , typeUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data TypeWithMetadata = TypeWithMetadata Type TypeMetadata deriving (Show, Eq, Ord, Generic)

newtype Variable = Variable Text deriving (Show, Eq, Ord, Generic)
data ArgumentMetadata = ArgumentMetadata
  { argStartPosition :: SourcePos
  , argUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data ArgumentWithMetadata = ArgumentWithMetadata Variable ArgumentMetadata deriving (Show, Eq, Ord, Generic)

data VariableMetadata = VariableMetadata
  { varStartPosition :: SourcePos
  , varUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data VariableWithMetadata = VariableWithMetadata Variable VariableMetadata deriving (Show, Eq, Ord, Generic)

-- | コンストラクタ定義
data ConstructorDef = ConstructorDef ArgumentWithMetadata TypeWithMetadata deriving (Show, Eq, Ord, Generic)

data ConstructorDefWithMetadata = ConstructorDefWithMetadata ConstructorDef ConstructorDefMetadata deriving (Show, Eq, Ord, Generic)

data ConstructorDefMetadata = ConstructorDefMetadata
  { constructorDefStartPosition :: SourcePos
  , constructorDefUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

-- | エフェクトオペレータのシグネチャ定義
data OperatorDef = OperatorDef ArgumentWithMetadata TypeWithMetadata deriving (Show, Eq, Ord, Generic)

data OperatorDefWithMetadata = OperatorDefWithMetadata OperatorDef OperatorDefMetadata deriving (Show, Eq, Ord, Generic)

data OperatorDefMetadata = OperatorDefMetadata
  { operatorDefStartPosition :: SourcePos
  , operatorDefUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

-- | ハンドラ節の定義
data HandlerClause
  = HandlerClause TypeVariableWithMetadata ArgumentWithMetadata ArgumentWithMetadata ExprWithMetadata -- オペレータ名, 引数変数, 継続変数, 式
  | HandlerReturnClause ArgumentWithMetadata ExprWithMetadata -- 変数名, 式
  deriving (Show, Eq, Ord, Generic)

data HandlerClauseWithMetadata = HandlerClauseWithMetadata HandlerClause HandlerClauseMetadata deriving (Show, Eq, Ord, Generic)

data HandlerClauseMetadata = HandlerClauseMetadata
  { handlerClauseStartPosition :: SourcePos
  , handlerClauseUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

-- | 式の型
data Expr
  = Var VariableWithMetadata -- 変数
  | Number Double -- 数値リテラル
  | Bool Bool -- 真偽値リテラル
  | BinOp BinOp ExprWithMetadata ExprWithMetadata -- 二項演算
  | If ExprWithMetadata ExprWithMetadata ExprWithMetadata -- if式 (condition) then expr else expr
  | Lambda PatternWithMetadata (Maybe TypeWithMetadata) ExprWithMetadata -- ラムダ式 (変数名, 型注釈(省略可), 本体)
  | Apply ExprWithMetadata ExprWithMetadata -- 関数適用
  | HandleApply ExprWithMetadata ExprWithMetadata -- ハンドル適用 (ハンドラ, 引数)
  | Match [ClauseWithMetadata] -- match式 (clauses)
  | Handle [HandlerClauseWithMetadata] -- ハンドル式 (ハンドラ節)
  | Tuple [ExprWithMetadata] -- タプルリテラル
  deriving (Show, Eq, Ord, Generic)

data ExprMetadata = ExprMetadata
  { exprStartPosition :: SourcePos
  , exprUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data ExprWithMetadata = ExprWithMetadata Expr ExprMetadata deriving (Show, Eq, Ord, Generic)

-- | 二項演算子の型
data BinOp
  = Add -- 加算
  | Sub -- 減算
  | Mul -- 乗算
  | Div -- 除算
  | Eq -- 等価比較
  | Lt -- 小なり
  | Gt -- 大なり
  deriving (Show, Eq, Ord, Generic)

-- | パターンの定義
data Pattern
  = PConstructor VariableWithMetadata PatternWithMetadata -- コンストラクタパターン (コンストラクタ名, パターン)
  | PVar ArgumentWithMetadata -- 変数パターン (変数名)
  | PWildcard -- ワイルドカードパターン (_)
  | PNumber Double -- 数値パターン
  | PBool Bool -- 真偽値パターン
  | PTuple [PatternWithMetadata] -- タプルパターン
  deriving (Show, Eq, Ord, Generic)

data PatternMetadata = PatternMetadata
  { patternStartPosition :: SourcePos
  , patternUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

data PatternWithMetadata = PatternWithMetadata Pattern PatternMetadata deriving (Show, Eq, Ord, Generic)

-- | match式の節の定義
data Clause = Clause PatternWithMetadata ExprWithMetadata -- パターン, 式
  deriving (Show, Eq, Ord, Generic)

data ClauseWithMetadata = ClauseWithMetadata Clause ClauseMetadata deriving (Show, Eq, Ord, Generic)

data ClauseMetadata = ClauseMetadata
  { clauseStartPosition :: SourcePos
  , clauseUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

-- | 値定義の型
data Definition
  = ValDef ArgumentWithMetadata TypeWithMetadata ExprWithMetadata -- 変数名, 型, 式
  | DataDef TypeArgumentWithMetadata [ConstructorDefWithMetadata] -- データ型名, コンストラクタ定義のリスト
  | EffectDef TypeArgumentWithMetadata [OperatorDefWithMetadata] -- エフェクト名, オペレータ定義のリスト
  deriving (Show, Eq, Ord, Generic)

data DefinitionWithMetadata = DefinitionWithMetadata Definition DefinitionMetadata deriving (Show, Eq, Ord, Generic)

data DefinitionMetadata = DefinitionMetadata
  { definitionStartPosition :: SourcePos
  , definitionUniqueId :: Int -- ソース内で唯一の ID　パースするたび同一
  }
  deriving (Show, Eq, Ord, Generic)

-- | プログラムの型 (トップレベル定義のリスト)
newtype Program = Program {getDefinitions :: [DefinitionWithMetadata]}
  deriving (Show, Eq, Ord, Generic)
