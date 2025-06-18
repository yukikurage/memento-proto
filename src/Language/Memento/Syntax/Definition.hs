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
  -- New data definition syntax separates constructor and type names:
  -- data Cons : (x : number) => Typ              -- Basic
  -- data Cons<T> : (x : T) => Typ                -- Constructor polymorphism
  -- data Cons : (x : number) => Typ<string>      -- Type polymorphism
  -- data ConsSome<T> : (x : T) => Some<T>        -- Combined
  DataDef ::
    f KVariable ->           -- Constructor name
    List (f KVariable) ->    -- Constructor type parameters (e.g., [T, U])
    List (f KType) ->        -- Constructor arguments type
    f KVariable ->           -- Type Constructor name
    List (f KType) ->        -- Type parameters for the type constructor
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
  hmap f (DataDef c ctorParams ctorArgs returnType typeParams) =
    DataDef (f c) (map f ctorParams) (map f ctorArgs) (f returnType) (map f typeParams)
  hmap f (TypeDef v params t) = TypeDef (f v) (map f params) (f t)
