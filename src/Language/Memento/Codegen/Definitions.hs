{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Definitions (
  generateDefinition,
  generateConstructorWrapperFunctions,
  getConstructorArity_cg,
  isDataDef,
  isValDef,
  generateConstructorsForAdt,
  generateSingleConstructorWrapper,
) where

import qualified Data.Text as T
import Language.Memento.Codegen.Expressions (generateExpr) -- For ValDef body
import Language.Memento.Syntax (ConstructorDef (..), Definition (..), Type (..))

-- Helper to check if a Definition is ValDef
isValDef :: Definition -> Bool
isValDef (ValDef _ _ _) = True
isValDef _ = False

-- Helper to check if a Definition is DataDef
isDataDef :: Definition -> Bool
isDataDef (DataDef _ _) = True
isDataDef _ = False

-- Helper to get constructor arity for codegen
getConstructorArity_cg :: Type -> Int
getConstructorArity_cg typ =
  case typ of
    TFunction _ (retType, _) -> 1 + getConstructorArity_cg retType
    _ -> 0

generateSingleConstructorWrapper :: ConstructorDef -> T.Text
generateSingleConstructorWrapper (ConstructorDef consName consTypeSyntax) =
  let arity = getConstructorArity_cg consTypeSyntax
      argNames = map (\i -> T.pack ("arg" ++ show i)) [1 .. arity]
      jsArgs = T.intercalate ", " argNames
      -- Construct the payload part of the array: "arg1, arg2, ..."
      -- If no args, this is empty. If args, it starts with a comma.
      jsPayload = if null argNames then T.empty else T.cons ',' (T.intercalate ", " argNames)
   in T.concat
        [ "const "
        , consName
        , " = "
        , T.concat (map (\argName -> T.pack "(" `T.append` argName `T.append` T.pack ") => ") argNames)
        , "ret([\"" -- Opening escaped quote for constructor name string literal in JS
        , consName
        , "\""
        , jsPayload -- Will be empty or like ", arg1, arg2"
        , "]);"
        ]

generateConstructorsForAdt :: Definition -> [T.Text]
generateConstructorsForAdt (DataDef _ consDefs) =
  map generateSingleConstructorWrapper consDefs
generateConstructorsForAdt _ = [] -- Should not happen if filtered by isDataDef

generateConstructorWrapperFunctions :: [Definition] -> T.Text
generateConstructorWrapperFunctions defs =
  let dataDefs = filter isDataDef defs
   in T.intercalate "\n" (concatMap generateConstructorsForAdt dataDefs)

generateDefinition :: Definition -> T.Text
generateDefinition (ValDef name _ expr) =
  T.concat ["const ", name, " = (", generateExpr expr, ")[1];"]
generateDefinition _ = T.empty -- DataDef and EffectDef don't generate top-level JS in this function
