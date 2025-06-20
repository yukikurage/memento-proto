{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Syntax.Definition where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Base (List)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KDefinition, KExpr, KType, KVariable)

-- Individual constructor definition for multi-constructor data types
data ConstructorDef f = ConstructorDef
  { cdName :: f KVariable           -- Constructor name (e.g., Some)
  , cdTypeParams :: List (f KVariable)  -- Constructor generics (e.g., [T])
  , cdType :: f KType               -- Constructor type (e.g., (x : T) => Maybe<T>)
  }

deriving instance (Show (f KVariable), Show (f KType)) => Show (ConstructorDef f)
deriving instance (Eq (f KVariable), Eq (f KType)) => Eq (ConstructorDef f)
deriving instance (Ord (f KVariable), Ord (f KType)) => Ord (ConstructorDef f)

data Definition f a where
  ValDef ::
    f KVariable ->           -- Variable name
    List (f KVariable) ->    -- Type parameters (e.g., [T, U])
    f KType ->               -- Type annotation
    f KExpr ->               -- Expression
    Definition f KDefinition
  -- Multi-constructor data definition syntax:
  -- data Maybe [Some<T> : (x : T) => Maybe<T>, None<T> : () => Maybe<T>];
  DataDef ::
    f KVariable ->             -- Data type name (e.g., Maybe)
    List (ConstructorDef f) -> -- List of constructors
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
  hmap f (DataDef dataName constructors) =
    DataDef (f dataName) (map (\(ConstructorDef name params typ) ->
      ConstructorDef (f name) (map f params) (f typ)) constructors)
  hmap f (TypeDef v params t) = TypeDef (f v) (map f params) (f t)
