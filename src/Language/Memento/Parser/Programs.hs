{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.Parser.Programs where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Literal (Literal (BoolLiteral, IntLiteral, NumberLiteral, StringLiteral))
import Language.Memento.Syntax.Program (Program (Program))
import Language.Memento.Syntax.Tag (KDefinition, KLiteral, KProgram, KVariable)
import Language.Memento.Syntax.Variable (Variable (Var))
import Text.Megaparsec (MonadParsec (try), choice, many, manyTill)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer as L

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
  defs <- many PClass.parseDefinition
  return $ hInject $ Program defs