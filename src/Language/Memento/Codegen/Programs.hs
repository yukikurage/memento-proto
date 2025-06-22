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
import Language.Memento.Codegen.Definitions (genDataDefinitionSeparated, genMultiDataDefinition, genValDefinition)

import Language.Memento.Syntax (
  AST,
  unDefinition,
  unProgram,
  unMType,
 )
import Language.Memento.Syntax.Definition (Definition (..), ConstructorDef (..))
import Language.Memento.Syntax.Program (Program (..))
import Language.Memento.Syntax.Tag (KDefinition, KProgram, KType)
import Language.Memento.Syntax.MType (MType (TFunction))
import Language.Memento.Data.HFix (HFix (..))

-------------------------------------------------------------------------------

-- | Predicate that checks whether a definition is a value definition.

-------------------------------------------------------------------------------

isValDef :: AST KDefinition -> Bool
isValDef astD =
  case unHFix astD of
    _meta :*: stx ->
      case unDefinition stx of
        ValDef _ _ _ _ -> True
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
        ValDef var _params typ expr ->  -- Ignore type parameters for now
          let bindings = genValDefinition var typ expr -- [(name, expr)]
           in T.unlines $ map (uncurry genConstDefRaw) bindings
        -- Multi-constructor data definition
        DataDef dataName constructors ->
          let bindings = genMultiDataDefinition constructors
           in T.unlines $ map (uncurry genConstDefRaw) bindings
        TypeDef var _params typ -> ""  -- Ignore type parameters for now

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
                  DataDef dataName constructors ->
                    genMultiDataDefinition constructors
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

