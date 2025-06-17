{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Definition where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Base (List)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KDefinition, KExpr, KType, KVariable)

data Definition f a where
  ValDef ::
    f KVariable ->           -- Variable name
    List (f KVariable) ->    -- Type parameters (e.g., [T, U])
    f KType ->               -- Type annotation
    f KExpr ->               -- Expression
    Definition f KDefinition
  -- 今回の言語では data は単一のコンストラクタしか持たない
  -- Union を使って複数コンストラクタをまとめる
  -- data Just<T> : (value : T) => Just<T>
  -- data Nothing<T> : () => Nothing<T>
  -- type Maybe<T> = Just<T> | Nothing<T>
  -- justVal : Maybe<number> := Just(10)
  -- nothingVal : Maybe<number> := Nothing()
  DataDef ::
    f KVariable ->           -- Data type & constructor name
    List (f KVariable) ->    -- Type parameters (e.g., [T, U])
    f KType ->               -- Data constructor type
    Definition f KDefinition
  TypeDef ::
    f KVariable ->           -- Type alias name
    List (f KVariable) ->    -- Type parameters (e.g., [T, U])
    f KType ->               -- Type definition
    Definition f KDefinition

deriving instance (Show (f KVariable), Show (f KType), Show (f KExpr)) => Show (Definition f a)
deriving instance (Eq (f KVariable), Eq (f KType), Eq (f KExpr)) => Eq (Definition f a)
deriving instance (Ord (f KVariable), Ord (f KType), Ord (f KExpr)) => Ord (Definition f a)

instance HFunctor Definition where
  hmap f (ValDef v params t e) = ValDef (f v) (map f params) (f t) (f e)
  hmap f (DataDef v params t) = DataDef (f v) (map f params) (f t)
  hmap f (TypeDef v params t) = TypeDef (f v) (map f params) (f t)