{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Pattern where

import Data.Kind (Type)
import GHC.Base (List)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KLiteral, KPattern, KType, KVariable)

-- | Pattern represents a pattern in pattern matching
data Pattern (f :: Type -> Type) (a :: Type) where
  -- | Variable pattern matches any value and binds it to a name
  PVar :: f KVariable -> Pattern f KPattern
  -- | Wildcard pattern matches any value without binding
  PWildcard :: Pattern f KPattern
  -- | Literal patterns match exact values
  PLiteral :: f KLiteral -> Pattern f KPattern
  -- | Constructor pattern matches a specific constructor and its arguments
  PCons :: f KVariable -> List (f KPattern) -> Pattern f KPattern

deriving instance (Show (f KVariable), Show (f KLiteral), Show (f KPattern)) => Show (Pattern f a)
deriving instance (Eq (f KVariable), Eq (f KLiteral), Eq (f KPattern)) => Eq (Pattern f a)
deriving instance (Ord (f KVariable), Ord (f KLiteral), Ord (f KPattern)) => Ord (Pattern f a)

instance HFunctor Pattern where
  hmap f (PVar v) = PVar (f v)
  hmap f PWildcard = PWildcard
  hmap f (PLiteral l) = PLiteral (f l)
  hmap f (PCons v ps) = PCons (f v) (map f ps)