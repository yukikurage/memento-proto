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
import Language.Memento.Syntax.MType (MType (TFunction, TVar, TApplication))
import Language.Memento.Syntax.Tag (KExpr, KType, KVariable, KTypeVariable)
import Language.Memento.Syntax.Definition (ConstructorDef (..))

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

-- Single data definition has been removed

extractData :: Text -> Int -> List Text
extractData scrutinee numArgs =
  map (\idx -> scrutinee <> "[" <> T.pack (show idx) <> "]") [1 .. numArgs]

genValDefinition :: AST KVariable -> AST KType -> AST KExpr -> DefinitionResult
genValDefinition astV _ astE = [(genVariable astV, genExpr astE)]

{-
New separated constructor/type syntax:

data SomeNum : (value : number) => Value

Generates:
const _SYM_SomeNum = Symbol();
const SomeNum = (...args) => [_SYM_SomeNum, ...args];
-}

genDataDefinitionSeparated :: AST KVariable -> AST KType -> DefinitionResult
genDataDefinitionSeparated constructorVar returnType = 
  let constructorName = genVariable constructorVar
      symName = "_SYM_" <> constructorName
      symDef = (symName, "Symbol()")
      constructorDef = (constructorName, "(...args) => [" <> symName <> ", ...args]")
   in [symDef, constructorDef]


-- Generate JavaScript for multi-constructor data definition
-- data Maybe [Some<T> : (x : T) => Maybe<T>, None<T> : () => Maybe<T>];
genMultiDataDefinition :: List (ConstructorDef (AST)) -> DefinitionResult
genMultiDataDefinition constructors = 
  let 
    -- Each constructor gets its own symbol
    constructorBindings = concatMap genSingleConstructor constructors
  in
    constructorBindings

-- Generate JavaScript for a single constructor
genSingleConstructor :: ConstructorDef (AST) -> DefinitionResult
genSingleConstructor (ConstructorDef { cdName = constructorAst, cdType = constructorTypeAst }) = 
  let constructorName = genVariable constructorAst
      symName = "_SYM_" <> constructorName
      symDef = (symName, "Symbol()")
      constructorDef = (constructorName, "(...args) => [" <> symName <> ", ...args]")
  in [symDef, constructorDef]

-- Remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)