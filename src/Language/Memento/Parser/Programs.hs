{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.Parser.Programs where

import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import qualified Language.Memento.Parser.Core as Core
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Program (Program (Program))
import Language.Memento.Syntax.Tag (KDefinition, KProgram)
import Text.Megaparsec (MonadParsec, eof, many)

-- | Parse a program, which consists of a series of definitions and other declarations.
parseProgram ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Program h
  , PClass.DefinitionParser f m
  , PClass.FixParser h f m
  ) =>
  m (f KProgram)
parseProgram = PClass.parseFix @h $ do
  -- Skip any leading whitespace
  Core.sc

  -- Parse all definitions, collecting only the valid ones
  defs <- many PClass.parseDefinition

  -- Make sure we've consumed the entire input
  eof

  -- Return the program with its definitions
  return $ hInject $ Program defs