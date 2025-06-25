{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.Parser.Variables where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Literal (Literal (BoolLiteral, IntLiteral, NumberLiteral, StringLiteral))
import Language.Memento.Syntax.Tag (KDefinition, KLiteral, KVariable, KTypeVariable)
import Language.Memento.Syntax.Variable (Variable (Var), TypeVariable (TypeVar))
import Text.Megaparsec (MonadParsec (try), choice, manyTill)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer as L

parseVariable ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Variable h
  , PClass.FixParser h f m
  ) =>
  m (f KVariable)
parseVariable = PClass.parseFix @h $ do
  var <- PClass.parseIdentifier
  return $ hInject $ Var var

parseTypeVariable ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective TypeVariable h
  , PClass.FixParser h f m
  ) =>
  m (f KTypeVariable)
parseTypeVariable = PClass.parseFix @h $ do
  var <- PClass.parseIdentifier
  return $ hInject $ TypeVar var