{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Programs (
  -- * Top level helpers
  generateProgram,

  -- * Utility helpers re-exported so that other code-gen modules can use them
  generateConstructorWrapperFunctions,
  generateDefinition,
  isValDef,
) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (List)

import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))

import Language.Memento.Codegen.Core (genConstDefRaw)
import Language.Memento.Codegen.Definitions (genDataDefinition, genValDefinition)

import Language.Memento.Syntax (
  AST,
  unDefinition,
  unProgram,
 )
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Program (Program (..))
import Language.Memento.Syntax.Tag (KDefinition, KProgram)

-------------------------------------------------------------------------------

-- | Predicate that checks whether a definition is a value definition.

-------------------------------------------------------------------------------

isValDef :: AST KDefinition -> Bool
isValDef astD =
  case unHFix astD of
    _meta :*: stx ->
      case unDefinition stx of
        ValDef _ _ _ -> True
        _ -> False

-------------------------------------------------------------------------------

{- | Convert a single definition (either @data@ or @val@) into JavaScript code.

  * For data definitions we emit a symbol constant and the constructor
    wrapper function.
  * For value definitions we emit a single constant binding.
-}

-------------------------------------------------------------------------------

generateDefinition :: AST KDefinition -> Text
generateDefinition astD =
  case unHFix astD of
    _meta :*: stx ->
      case unDefinition stx of
        -- Value definition
        ValDef var typ expr ->
          let bindings = genValDefinition var typ expr -- [(name, expr)]
           in T.unlines $ map (uncurry genConstDefRaw) bindings
        -- Data definition (emit its constructor wrappers)
        DataDef var typ ->
          let bindings = genDataDefinition var typ -- [(name, expr)]
           in T.unlines $ map (uncurry genConstDefRaw) bindings
        TypeDef var typ -> ""

-------------------------------------------------------------------------------

{- | Generate /only/ the constructor wrapper functions for the given list of
  definitions. This is useful because data and value definitions are often
  emitted in separate sections in the final output.
-}

-------------------------------------------------------------------------------

generateConstructorWrapperFunctions :: List (AST KDefinition) -> Text
generateConstructorWrapperFunctions defs =
  let
    bindings =
      concatMap
        ( \astD ->
            case unHFix astD of
              _meta :*: stx ->
                case unDefinition stx of
                  DataDef var typ -> genDataDefinition var typ
                  _ -> []
        )
        defs
   in
    T.unlines $ map (uncurry genConstDefRaw) bindings

-------------------------------------------------------------------------------

{- | High level helper that converts an entire program AST into JavaScript.
  The layout roughly follows:

@
  // Constructor wrappers
  ...

  // Value definitions
  ...
@
-}

-------------------------------------------------------------------------------

generateProgram :: AST KProgram -> Text
generateProgram astP =
  case unHFix astP of
    _meta :*: stx ->
      case unProgram stx of
        Program defs ->
          let
            constructorPart = generateConstructorWrapperFunctions defs
            valPart =
              let valDefs = filter isValDef defs
               in T.intercalate "\n\n" $ map generateDefinition valDefs
           in
            T.unlines
              [ "// Generated ADT Constructor functions"
              , constructorPart
              , ""
              , "// Generated value definitions"
              , valPart
              ]
