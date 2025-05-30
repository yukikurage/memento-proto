{-# LANGUAGE GADTs #-}

module Language.Memento.Syntax.Type where

import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Syntax.Kind (KToken, KType)

-- | Type is a type-level function that maps a kind to a type.
data Type f a where
  TNumber :: Type f KType
  TBool :: Type f KType
  TString :: Type f KType
  TFunction ::
    f KType -> f KType -> Type f KType
  TData ::
    f (KToken Text) -> Type f KType
  TUnknown :: Type f KType
  TNever :: Type f KType
  TNumberLiteral :: f (KToken Double) -> Type f KType
  TBoolLiteral :: f (KToken Bool) -> Type f KType
  TStringLiteral :: f (KToken Text) -> Type f KType
  TUnion :: f KType -> f KType -> Type f KType
  TIntersection :: f KType -> f KType -> Type f KType