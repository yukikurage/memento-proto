{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Variable where

import Data.Kind (Type)
import Data.Text (Text)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KVariable)

data Variable (f :: Type -> Type) a where
  Var :: Text -> Variable f KVariable

deriving instance Show (Variable f a)
deriving instance Eq (Variable f a)
deriving instance Ord (Variable f a)

instance HFunctor Variable where
  hmap f (Var v) = Var v