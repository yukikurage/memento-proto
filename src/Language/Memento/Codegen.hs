{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen (
  -- * Public API
  generateJS,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Memento.Codegen.Base (baseDefinitions)
import Language.Memento.Codegen.Core (genVariable)
import Language.Memento.Codegen.Programs (generateProgram)
import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Syntax (AST, unDefinition, unProgram)
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Program (Program (..))
import Language.Memento.Syntax.Tag (KDefinition, KProgram, KVariable)

-- Additional utility imports (must appear before declarations)

import Control.Applicative ((<|>))

{- | Entry point: convert a whole program AST to JavaScript source code.

The generated code layout:

  1. 'use strict' header
  2. shared runtime helpers (from @baseDefinitions@)
  3. code generated from the user's program (@generateProgram@)
  4. optional execution block (evaluate @main@ if it exists)

The resulting 'Text' can be written directly to a file.
-}

-------------------------------------------------------------------------------

generateJS :: AST KProgram -> Text
generateJS astP =
  let
    programCode = generateProgram astP
    execBlock = generateExecutionBlock astP
   in
    T.concat
      [ "'use strict';\n\n"
      , baseDefinitions
      , "\n\n"
      , programCode
      , execBlock
      ]

-------------------------------------------------------------------------------
-- Helpers --------------------------------------------------------------------
-------------------------------------------------------------------------------

{- | Generate a small block of JS that executes either:
  * the user-defined function/value named @main@ (if present), or
  * nothing at all (empty string) when no such definition exists.
  When @main@ is a function we call it with no arguments;
  otherwise we just log it as is.
-}

-------------------------------------------------------------------------------

generateExecutionBlock :: AST KProgram -> Text
generateExecutionBlock astP =
  case unHFix astP of
    _meta :*: stx ->
      case unProgram stx of
        Program defs ->
          let mMain = findMain defs
           in case mMain of
                Nothing -> "" -- do nothing
                Just varName ->
                  T.concat
                    ["\n\nconsole.log(JSON.stringify(", varName, "()));"]

-- | Search for a value definition named @main@ and return its JS identifier.
findMain :: [AST KDefinition] -> Maybe Text
findMain defs =
  let
    go astD = case unHFix astD of
      _meta :*: stx -> case unDefinition stx of
        ValDef v _ _ _ ->  -- Ignore type parameters
          let name = genVariable v in if name == "main" then Just name else Nothing
        _ -> Nothing
   in
    foldr (\d acc -> acc <|> go d) Nothing defs