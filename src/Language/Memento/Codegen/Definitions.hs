{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Definitions where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (List)
import Language.Memento.Codegen.Core (genVariable, getSymName)
import Language.Memento.Codegen.Expressions (genExpr)
import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Syntax (AST, unMType)
import Language.Memento.Syntax.MType (MType (TFunction, TVar))
import Language.Memento.Syntax.Tag (KExpr, KType, KVariable)

type DefinitionResult = (List (Text, Text)) -- (argName, argValue) -> const argName = argValue

{-
Data definition:

data T : (A1, A2, ... An) -> T

↓

const _SYM_T = Symbol();
const T = (...args) => [_SYM_T, ...args];

is T? scrutinee[0] === _SYM_T
extract data
x = scrutinee[1]
y = scrutinee[2]
...
z = scrutinee[N]

-}

genDataDefinition :: AST KVariable -> AST KType -> DefinitionResult
genDataDefinition astV astT = case unHFix astT of
  meta :*: stx -> case unMType stx of
    TFunction ts ret -> case unHFix ret of
      meta :*: stx -> case unMType stx of
        TVar v ->
              let symName = getSymName astV
                  symDef = (symName, "Symbol()")
                  classDef = (genVariable astV, "(...args) => [" <> symName <> ", ...args]")
               in [symDef, classDef]
        _ -> error ("genData: Expected TVar, got" <> (show (unMType stx)))
    _ -> error "genData: Expected TFunction"

extractData :: Text -> Int -> List Text
extractData scrutinee numArgs =
  map (\idx -> scrutinee <> "[" <> T.pack (show idx) <> "]") [1 .. numArgs]

genValDefinition :: AST KVariable -> AST KType -> AST KExpr -> DefinitionResult
genValDefinition astV _ astE = [(genVariable astV, genExpr astE)]