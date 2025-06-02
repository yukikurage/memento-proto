{-# LANGUAGE TypeOperators #-}

module Language.Memento.Syntax where

import Language.Memento.Data.HCoproduct (HVoid, (:+:))
import Language.Memento.Data.HFix (HFix)
import Language.Memento.Data.HProduct ((:*:))
import Language.Memento.Syntax.BinOp (BinOp (..))
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Expr (Expr (..), Let (..))
import Language.Memento.Syntax.Literal (Literal)
import Language.Memento.Syntax.MType (MType (..))
import Language.Memento.Syntax.Metadata (Metadata (..))
import Language.Memento.Syntax.Pattern (Pattern (..))
import Language.Memento.Syntax.Program (Program (..))
import Language.Memento.Syntax.Variable (Variable (..))

-- Higher-order Fixed Point を使ってプログラム全体を構築する

type Syntax =
  HVoid
    :+: Literal
    :+: MType
    :+: Expr
    :+: BinOp
    :+: Definition
    :+: Program
    :+: Variable
    :+: Pattern
    :+: Let

type AST = HFix (Metadata :*: Syntax)