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
import Language.Memento.Syntax.Tag (KExpr, KType, KVariable)
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

{-
New separated constructor/type syntax:

data SomeNum : (value : number) => Value

Generates:
const _SYM_Value = Symbol();
const SomeNum = (...args) => [_SYM_Value, ...args];
-}

genDataDefinitionSeparated :: AST KVariable -> AST KType -> DefinitionResult
genDataDefinitionSeparated constructorVar returnType = 
  let returnTypeName = extractTypeName returnType
      symName = "_SYM_" <> returnTypeName
      symDef = (symName, "Symbol()")
      constructorName = genVariable constructorVar
      constructorDef = (constructorName, "(...args) => [" <> symName <> ", ...args]")
   in [symDef, constructorDef]

-- Extract the type name from a return type AST
-- For function types like (value : number) => Type, extract the return type (Type)
extractTypeName :: AST KType -> Text
extractTypeName astT = case unHFix astT of
  _meta :*: stx -> case unMType stx of
    TVar v -> genVariable v
    TApplication base _args -> genVariable base
    TFunction _params returnType -> extractTypeName returnType
    _ -> error ("extractTypeName: Unsupported return type: " <> show (unMType stx))

-- Generate JavaScript for multi-constructor data definition
-- data Maybe [Some<T> : (x : T) => Maybe<T>, None<T> : () => Maybe<T>];
genMultiDataDefinition :: List (ConstructorDef (AST)) -> DefinitionResult
genMultiDataDefinition constructors = 
  let 
    -- Group constructors by their return type to create shared symbols
    constructorBindings = concatMap genSingleConstructor constructors
    
    -- Extract unique return type names and create symbols for them
    returnTypeNames = map (\constructor -> extractTypeName (cdType constructor)) constructors
    uniqueReturnTypes = removeDuplicates returnTypeNames
    symbolBindings = [(symName, "Symbol()") | typeName <- uniqueReturnTypes, let symName = "_SYM_" <> typeName]
  in
    symbolBindings ++ constructorBindings

-- Generate JavaScript for a single constructor
genSingleConstructor :: ConstructorDef (AST) -> DefinitionResult
genSingleConstructor (ConstructorDef { cdName = constructorAst, cdType = constructorTypeAst }) = 
  let returnTypeName = extractTypeName constructorTypeAst
      symName = "_SYM_" <> returnTypeName
      constructorName = genVariable constructorAst
      constructorDef = (constructorName, "(...args) => [" <> symName <> ", ...args]")
  in [constructorDef]

-- Remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)