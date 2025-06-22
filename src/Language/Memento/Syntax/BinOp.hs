{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.BinOp where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KBinOp)

data BinOp (f :: Type -> Type) a where
  Add :: BinOp f KBinOp
  Sub :: BinOp f KBinOp
  Mul :: BinOp f KBinOp
  Div :: BinOp f KBinOp
  Eq :: BinOp f KBinOp
  Lt :: BinOp f KBinOp
  Gt :: BinOp f KBinOp

deriving instance Show (BinOp f a)
deriving instance Eq (BinOp f a)
deriving instance Ord (BinOp f a)
instance HFunctor BinOp where
  hmap f Add = Add
  hmap f Sub = Sub
  hmap f Mul = Mul
  hmap f Div = Div
  hmap f Eq = Eq
  hmap f Lt = Lt
  hmap f Gt = Gt