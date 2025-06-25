{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Variable where

import Data.Kind (Type)
import Data.Text (Text)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KVariable, KTypeVariable)

data Variable (f :: Type -> Type) a where
  Var :: Text -> Variable f KVariable

data TypeVariable (f :: Type -> Type) a where
  TypeVar :: Text -> TypeVariable f KTypeVariable

deriving instance Show (Variable f a)
deriving instance Eq (Variable f a)
deriving instance Ord (Variable f a)

deriving instance Show (TypeVariable f a)
deriving instance Eq (TypeVariable f a)
deriving instance Ord (TypeVariable f a)

instance HFunctor Variable where
  hmap f (Var v) = Var v

instance HFunctor TypeVariable where
  hmap f (TypeVar v) = TypeVar v