{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Definitions (
  generateDefinition,
  generateConstructorWrapperFunctions,
  getConstructorArity_cg,
  isDataDef,
  isValDef,
  isEffectDef,
  generateConstructorsForAdt,
  generateSingleConstructorWrapper,
  generateSingleOperationWrapper,
) where

import qualified Data.Text as T
import Language.Memento.Codegen.Expressions (generateExpr) -- For ValDef body
import Language.Memento.Syntax (ArgumentWithMetadata (ArgumentWithMetadata), ConstructorDef (..), ConstructorDefWithMetadata (ConstructorDefWithMetadata), Definition (..), DefinitionWithMetadata (DefinitionWithMetadata), OperatorDef (OperatorDef), OperatorDefWithMetadata (OperatorDefWithMetadata), Type (..), TypeWithMetadata (TypeWithMetadata), Variable (Variable))

-- Helper to check if a Definition is ValDef
isValDef :: DefinitionWithMetadata -> Bool
isValDef (DefinitionWithMetadata (ValDef _ _ _) _) = True
isValDef _ = False

isEffectDef :: DefinitionWithMetadata -> Bool
isEffectDef (DefinitionWithMetadata (EffectDef{}) _) = True
isEffectDef _ = False

-- Helper to check if a Definition is DataDef
isDataDef :: DefinitionWithMetadata -> Bool
isDataDef (DefinitionWithMetadata (DataDef _ _) _) = True
isDataDef _ = False

-- Helper to get constructor arity for codegen
getConstructorArity_cg :: TypeWithMetadata -> Int
getConstructorArity_cg typ =
  case typ of
    TypeWithMetadata (TFunction _ (retType, _)) _ -> 1 + getConstructorArity_cg retType
    _ -> 0

generateSingleConstructorWrapper :: ConstructorDefWithMetadata -> T.Text
generateSingleConstructorWrapper
  ( ConstructorDefWithMetadata
      ( ConstructorDef
          (ArgumentWithMetadata (Variable consName) _)
          consTypeSyntax
        )
      _
    ) =
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

generateSingleOperationWrapper :: OperatorDefWithMetadata -> T.Text
generateSingleOperationWrapper
  ( OperatorDefWithMetadata
      (OperatorDef (ArgumentWithMetadata (Variable opName) _) opTypeSyntax)
      _
    ) =
    T.concat
      [ "const "
      , opName
      , " = (_op_arg_) => "
      , "([\"op" -- Opening escaped quote for constructor name string literal in JS
      , opName
      , "\""
      , ", _op_arg_, (x) => ret(x)"
      , "]);"
      ]

generateConstructorsForAdt :: DefinitionWithMetadata -> [T.Text]
generateConstructorsForAdt (DefinitionWithMetadata (DataDef _ consDefs) _) =
  map generateSingleConstructorWrapper consDefs
generateConstructorsForAdt _ = [] -- Should not happen if filtered by isDataDef

generateConstructorWrapperFunctions :: [DefinitionWithMetadata] -> T.Text
generateConstructorWrapperFunctions defs =
  let dataDefs = filter isDataDef defs
   in T.intercalate "\n" (concatMap generateConstructorsForAdt dataDefs)

generateOperatorsForEffect :: DefinitionWithMetadata -> [T.Text]
generateOperatorsForEffect (DefinitionWithMetadata (EffectDef effName opDefs) _) =
  map generateSingleOperationWrapper opDefs
generateOperatorsForEffect _ = []

generateOperationWrapperFunctions :: [DefinitionWithMetadata] -> T.Text
generateOperationWrapperFunctions defs =
  let effectDefs = filter isEffectDef defs
   in T.intercalate "\n" (concatMap generateOperatorsForEffect effectDefs)

generateDefinition :: DefinitionWithMetadata -> T.Text
generateDefinition (DefinitionWithMetadata (ValDef (ArgumentWithMetadata (Variable name) _) _ expr) _) =
  T.concat ["const ", name, " = (", generateExpr expr, ")[1];"]
generateDefinition _ = T.empty -- DataDef and EffectDef don't generate top-level JS in this function
