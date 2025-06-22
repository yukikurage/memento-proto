{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.BinOps where

import Data.Text (Text)

import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Syntax (AST, unBinOp)
import Language.Memento.Syntax.BinOp (BinOp (..))
import Language.Memento.Syntax.Tag (KBinOp)

genBinOp :: AST KBinOp -> Text
genBinOp astB = case unHFix astB of
  meta :*: stx -> case unBinOp stx of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Eq -> "==="
    Lt -> "<"
    Gt -> ">"