{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Pattern where

import Data.List (foldl', zipWith)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (List)

import Language.Memento.Codegen.Core (extractData, matchData)
import Language.Memento.Codegen.Literal (genLiteral)
import Language.Memento.Data.HCoproduct (Injective, hProject)
import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Syntax
import Language.Memento.Syntax.Literal (Literal (..))
import Language.Memento.Syntax.Pattern (Pattern (..))
import Language.Memento.Syntax.Tag (KLiteral, KPattern, KVariable)
import Language.Memento.Syntax.Variable (Variable (..))

-- (condition, bindings)
type PatternResult = (List Text, List (AST KVariable, Text))

genPatternVar :: AST KVariable -> Text -> PatternResult
genPatternVar astV arg = ([], [(astV, arg)])

genPatternWildcard :: Text -> PatternResult
genPatternWildcard arg = ([], [])

genPatternLiteral :: AST KLiteral -> Text -> PatternResult
genPatternLiteral astL arg = ([arg <> "===" <> genLiteral astL], [])

genPatternCons :: AST KVariable -> List (AST KPattern) -> Text -> PatternResult
genPatternCons consAST args scrutinee =
  let
    pred = matchData consAST scrutinee
    extracted = extractData scrutinee (length args) -- scrutinee[1] ... scrutinee[N]
    argResults = zipWith genPattern args extracted
    argConditions = concatMap fst argResults
    argBindings = concatMap snd argResults
   in
    (pred : argConditions, argBindings)

genPattern :: AST KPattern -> Text -> PatternResult
genPattern astP arg = case unHFix astP of
  meta :*: pat -> case unPattern pat of
    PVar v -> genPatternVar v arg
    PWildcard -> genPatternWildcard arg
    PLiteral l -> genPatternLiteral l arg
    PCons v ps -> genPatternCons v ps arg
