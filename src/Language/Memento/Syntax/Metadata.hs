{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Metadata where

import Data.Kind (Type)
import Text.Megaparsec (SourcePos)

type UniqueId = Int

-- | Type is a type-level function that maps a kind to a type.
data Metadata (f :: Type -> Type) a where
  Metadata :: SourcePos -> SourcePos -> Metadata f a

deriving instance Show (Metadata f a)
deriving instance Eq (Metadata f a)
deriving instance Ord (Metadata f a)