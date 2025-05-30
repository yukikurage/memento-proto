{-# LANGUAGE GADTs #-}

module Language.Memento.Syntax.Expr where

import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Syntax.Kind (KBinOp, KExpr, KPattern, KToken, KType)

data Expr f a where
  EVar :: f (KToken Text) -> Expr f KExpr
  ENumber :: f (KToken Double) -> Expr f KExpr
  EBool :: f (KToken Bool) -> Expr f KExpr
  EString :: f (KToken Text) -> Expr f KExpr
  ELambda :: f KPattern -> Maybe (f KType) -> f KExpr -> Expr f KExpr
  EApply :: f KExpr -> f KExpr -> Expr f KExpr
  EMatch :: [f KExpr] -> Expr f KExpr -- Match はλ式の集合
  EIf :: f KExpr -> f KExpr -> f KExpr -> Expr f KExpr
  EBinOp :: f KBinOp -> f KExpr -> f KExpr -> Expr f KExpr