{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.Parser.Literal where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Literal (Literal (BoolLiteral, IntLiteral, NumberLiteral, StringLiteral))
import Language.Memento.Syntax.Tag (KDefinition, KLiteral)
import Text.Megaparsec (MonadParsec (try), choice, manyTill)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer as L

parseNumberLiteral ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.FixParser h f m
  , PClass.CoreParser m
  , Injective Literal h
  ) =>
  m (f KLiteral)
parseNumberLiteral = PClass.parseFix @h $ do
  num <- PClass.parseLexeme L.float
  return $ hInject $ NumberLiteral num

parseBoolLiteral ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.FixParser h f m
  , PClass.CoreParser m
  , Injective Literal h
  ) =>
  m (f KLiteral)
parseBoolLiteral = PClass.parseFix @h $ do
  bool <- PClass.parseLexeme $ True <$ PClass.parseSymbol "true" <|> False <$ PClass.parseSymbol "false"
  return $ hInject $ BoolLiteral bool

parseIntLiteral ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.FixParser h f m
  , PClass.CoreParser m
  , Injective Literal h
  ) =>
  m (f KLiteral)
parseIntLiteral = PClass.parseFix @h $ do
  int <- PClass.parseLexeme L.decimal
  return $ hInject $ IntLiteral int

parseStringLiteral ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.FixParser h f m
  , PClass.CoreParser m
  , Injective Literal h
  ) =>
  m (f KLiteral)
parseStringLiteral = PClass.parseFix @h $ do
  str <- PClass.parseLexeme (char '"' >> manyTill L.charLiteral (char '"'))
  return $ hInject $ StringLiteral $ T.pack str

parseLiteral ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.FixParser h f m
  , PClass.CoreParser m
  , Injective Literal h
  ) =>
  m (f KLiteral)
parseLiteral = choice [parseNumberLiteral @h, parseBoolLiteral @h, parseIntLiteral @h, parseStringLiteral @h]