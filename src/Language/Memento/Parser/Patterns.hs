{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser.Patterns where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.Pattern (Pattern (PCons, PLiteral, PVar, PWildcard))
import Language.Memento.Syntax.Tag (KPattern)
import Text.Megaparsec (MonadParsec (try), choice, sepBy)

-- | Parse a pattern
parsePattern ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Pattern h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KPattern)
parsePattern =
  choice
    [ parseVarPattern @h
    , parseWildcardPattern @h
    , parseLiteralPattern @h
    , parseConsPattern @h
    ]

-- | Parse a variable pattern
parseVarPattern ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Pattern h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  ) =>
  m (f KPattern)
parseVarPattern = PClass.parseFix @h $ do
  var <- PClass.parseVariable
  return $ hInject $ PVar var

-- | Parse a wildcard pattern
parseWildcardPattern ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Pattern h
  , PClass.FixParser h f m
  ) =>
  m (f KPattern)
parseWildcardPattern = PClass.parseFix @h $ do
  PClass.parseSymbol "_"
  return $ hInject PWildcard

-- | Parse a literal pattern
parseLiteralPattern ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Pattern h
  , PClass.FixParser h f m
  , PClass.LiteralParser f m
  ) =>
  m (f KPattern)
parseLiteralPattern = PClass.parseFix @h $ do
  lit <- PClass.parseLiteral
  return $ hInject $ PLiteral lit

-- | Parse a constructor pattern
parseConsPattern ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Pattern h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KPattern)
parseConsPattern = PClass.parseFix @h $ do
  constructor <- PClass.parseVariable
  args <- PClass.parseParens $ sepBy (parsePattern @h) (PClass.parseSymbol ",")
  return $ hInject $ PCons constructor args
