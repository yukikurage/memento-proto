{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.MType where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Base (List)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KLiteral, KType, KVariable)

-- | Type is a type-level function that maps a kind to a type.
data MType f a where
  TVar :: f KVariable -> MType f KType
  TNumber :: MType f KType
  TInt :: MType f KType
  TBool :: MType f KType
  TString :: MType f KType
  TFunction ::
    List (f KVariable, f KType) -> f KType -> MType f KType
  TUnknown :: MType f KType
  TNever :: MType f KType
  TLiteral :: f KLiteral -> MType f KType
  TUnion :: List (f KType) -> MType f KType
  TIntersection :: List (f KType) -> MType f KType
  -- Polymorphism support
  TApplication :: f KVariable -> List (f KType) -> MType f KType  -- Type<Arg1, Arg2, ...>

deriving instance (Show (f KLiteral), Show (f KType), Show (f KVariable)) => Show (MType f a)
deriving instance (Eq (f KLiteral), Eq (f KType), Eq (f KVariable)) => Eq (MType f a)
deriving instance (Ord (f KLiteral), Ord (f KType), Ord (f KVariable)) => Ord (MType f a)

instance HFunctor MType where
  hmap f (TVar v) = TVar (f v)
  hmap f TNumber = TNumber
  hmap f TInt = TInt
  hmap f TBool = TBool
  hmap f TString = TString
  hmap f (TFunction ts e) = TFunction (map (bimap f f) ts) (f e)
  hmap f TUnknown = TUnknown
  hmap f TNever = TNever
  hmap f (TLiteral l) = TLiteral (f l)
  hmap f (TUnion ts) = TUnion (map f ts)
  hmap f (TIntersection ts) = TIntersection (map f ts)
  hmap f (TApplication base args) = TApplication (f base) (map f args)