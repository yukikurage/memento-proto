{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Language.Memento.Syntax where

import Data.Maybe (fromJust)
import Language.Memento.Data.HCoproduct (HVoid, Injective (hProject), (:+:))
import Language.Memento.Data.HFix (HFix (HFix, unHFix))
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Data.HOp
import Language.Memento.Data.HProduct (HUnit (HUnit), type (:*:) (..))
import Language.Memento.Syntax.BinOp (BinOp (..))
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Expr (Expr (..), Let (..))
import Language.Memento.Syntax.Literal (Literal)
import Language.Memento.Syntax.MType (MType (..))
import Language.Memento.Syntax.Metadata (Metadata (..))
import Language.Memento.Syntax.Pattern (Pattern (..))
import Language.Memento.Syntax.Program (Program (..))
import Language.Memento.Syntax.Tag (KBinOp, KDefinition, KExpr, KLet, KLiteral, KPattern, KProgram, KType, KVariable)
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

unLiteral :: Syntax AST KLiteral -> Literal AST KLiteral
unLiteral stx = fromJust $ hProject stx -- KLiteral は Literal のみに含まれる

unMType :: Syntax AST KType -> MType AST KType
unMType stx = fromJust $ hProject stx -- KType は MType のみに含まれる

unExpr :: Syntax AST KExpr -> Expr AST KExpr
unExpr stx = fromJust $ hProject stx -- KExpr は Expr のみに含まれる

unBinOp :: Syntax AST KBinOp -> BinOp AST KBinOp
unBinOp stx = fromJust $ hProject stx -- KBinOp は BinOp のみに含まれる

unDefinition :: Syntax AST KDefinition -> Definition AST KDefinition
unDefinition stx = fromJust $ hProject stx -- KDefinition は Definition のみに含まれる

unProgram :: Syntax AST KProgram -> Program AST KProgram
unProgram stx = fromJust $ hProject stx -- KProgram は Program のみに含まれる

unVariable :: Syntax AST KVariable -> Variable AST KVariable
unVariable stx = fromJust $ hProject stx -- KVariable は Variable のみに含まれる

unPattern :: Syntax AST KPattern -> Pattern AST KPattern
unPattern stx = fromJust $ hProject stx -- KPattern は Pattern のみに含まれる

unLet :: Syntax AST KLet -> Let AST KLet
unLet stx = fromJust $ hProject stx -- KLet は Let のみに含まれる

type AST' = HFix Syntax

noMetadata :: AST a -> AST' a
noMetadata ast = HFix $ hmap noMetadata $ unMetadata $ unHFix ast
 where
  unMetadata :: forall f a. (Metadata :*: Syntax) f a -> Syntax f a
  unMetadata (meta :*: stx) = stx

-- Extract syntax from AST, handling the metadata
extractSyntax :: AST a -> Syntax AST a
extractSyntax ast =
  case unHFix ast of
    (metadata :*: syntax) -> syntax