{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Program where

import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Base (List)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KDefinition, KProgram)

data Program f a where
  Program :: List (f KDefinition) -> Program f KProgram

deriving instance (Show (f KDefinition)) => Show (Program f a)
deriving instance (Eq (f KDefinition)) => Eq (Program f a)
deriving instance (Ord (f KDefinition)) => Ord (Program f a)

instance HFunctor Program where
  hmap f (Program ds) = Program (map f ds)