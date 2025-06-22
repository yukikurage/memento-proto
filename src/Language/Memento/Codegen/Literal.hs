{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Literal where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Syntax (AST, unLiteral)
import Language.Memento.Syntax.Literal (Literal (..))
import Language.Memento.Syntax.Tag (KLiteral)

-- | Generate a JavaScript representation of a literal
genLiteral :: AST KLiteral -> Text
genLiteral astL = case unHFix astL of
  meta :*: stx -> case unLiteral stx of
    NumberLiteral n -> T.pack $ show n
    BoolLiteral b -> T.pack $ if b then "true" else "false"
    StringLiteral s -> T.pack $ show s -- 上手くエスケープが働くか調べる
    IntLiteral i -> T.pack $ show i