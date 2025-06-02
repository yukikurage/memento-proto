{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.MType where

import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Base (List)
import Language.Memento.Syntax.Tag (KLiteral, KType, KVariable)

-- | Type is a type-level function that maps a kind to a type.
data MType f a where
  TVar :: f KVariable -> MType f KType
  TNumber :: MType f KType
  TInt :: MType f KType
  TBool :: MType f KType
  TString :: MType f KType
  TFunction ::
    List (f KType) -> f KType -> MType f KType
  TData :: f KVariable -> MType f KType
  TUnknown :: MType f KType
  TNever :: MType f KType
  TLiteral :: f KLiteral -> MType f KType
  TUnion :: List (f KType) -> MType f KType
  TIntersection :: List (f KType) -> MType f KType

deriving instance (Show (f KLiteral), Show (f KType), Show (f KVariable)) => Show (MType f a)
deriving instance (Eq (f KLiteral), Eq (f KType), Eq (f KVariable)) => Eq (MType f a)
deriving instance (Ord (f KLiteral), Ord (f KType), Ord (f KVariable)) => Ord (MType f a)