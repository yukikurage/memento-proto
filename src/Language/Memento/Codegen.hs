{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Codegen.Base (baseDefinitions)
import Language.Memento.Codegen.Definitions
import Language.Memento.Codegen.Expressions (generateExpr) -- generateExpr is still needed for generateDefinition
import Language.Memento.Syntax (ArgumentWithMetadata (ArgumentWithMetadata), Definition (..), DefinitionWithMetadata (DefinitionWithMetadata), Program (..), Variable (Variable)) -- Removed ConstructorDef, Type as they are now handled in Definitions.hs

-- | JavaScriptコードの生成
generateJS :: Program -> Text
generateJS (Program definitions) =
  T.concat
    [ "'use strict';\n\n"
    , baseDefinitions
    , "\n\n// Generated ADT Constructor functions\n"
    , generateConstructorWrapperFunctions definitions -- from Definitions.hs
    , "\n\n// Generated Value definitions\n"
    , T.intercalate "\n\n" (map generateDefinition (filter isValDef definitions)) -- from Definitions.hs
    , finalExecutionBlock definitions -- Pass definitions to finalExecutionBlock
    ]

-- finalExecutionBlock remains here as it's part of the top-level JS generation logic
finalExecutionBlock :: [DefinitionWithMetadata] -> Text
finalExecutionBlock definitions =
  let valDefs = filter isValDef definitions -- isValDef from Definitions.hs
   in if null valDefs
        then "\n\n// No ValDefs found to execute."
        else
          let lastDefName = (\(DefinitionWithMetadata (ValDef (ArgumentWithMetadata (Variable name) _) _ _) _) -> name) (last valDefs)
              mainDefExists = any (\(DefinitionWithMetadata (ValDef (ArgumentWithMetadata (Variable name) _) _ _) _) -> name == "main") valDefs
              nameToExecute = if mainDefExists then "main" else lastDefName
           in T.concat ["\n\nconsole.log(JSON.stringify(globalHandler(", nameToExecute, "())));"]