{-# LANGUAGE GADTs #-}

module Language.Memento.Syntax.BinOp where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Syntax.Kind (KBinOp, KDefinition, KExpr, KToken, KType)

data BinOp f a where
  Add :: BinOp f KBinOp
  Sub :: BinOp f KBinOp
  Mul :: BinOp f KBinOp
  Div :: BinOp f KBinOp
  Eq :: BinOp f KBinOp
  Lt :: BinOp f KBinOp
  Gt :: BinOp f KBinOp