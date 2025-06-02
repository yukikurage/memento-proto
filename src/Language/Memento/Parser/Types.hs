{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser.Types where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.MType (MType (TBool, TData, TFunction, TInt, TIntersection, TLiteral, TNever, TNumber, TString, TUnion, TUnknown, TVar))
import Language.Memento.Syntax.Tag (KType)
import Text.Megaparsec (MonadParsec (try), choice, sepBy)

-- | Parse a type
parseMType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseMType =
  choice
    [ parseVarType @h
    , parsePrimitiveType @h
    , parseFunctionType @h
    , parseDataType @h
    , parseLiteralType @h
    , parseUnionType @h
    , parseIntersectionType @h
    , parseUnknownType @h
    , parseNeverType @h
    ]

-- | Parse a variable type
parseVarType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  ) =>
  m (f KType)
parseVarType = PClass.parseFix @h $ do
  var <- PClass.parseVariable
  return $ hInject $ TVar var

-- | Parse a primitive type
parsePrimitiveType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  ) =>
  m (f KType)
parsePrimitiveType =
  PClass.parseFix @h $
    choice
      [ PClass.parseReservedWord "number" >> return (hInject TNumber)
      , PClass.parseReservedWord "int" >> return (hInject TInt)
      , PClass.parseReservedWord "bool" >> return (hInject TBool)
      , PClass.parseReservedWord "string" >> return (hInject TString)
      ]

-- | Parse a function type
parseFunctionType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseFunctionType = PClass.parseFix @h $ do
  args <- PClass.parseParens $ sepBy (parseMType @h) (PClass.parseSymbol ",")
  PClass.parseSymbol "=>"
  ret <- parseMType @h
  return $ hInject $ TFunction args ret

-- | Parse a data type
parseDataType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  ) =>
  m (f KType)
parseDataType = PClass.parseFix @h $ do
  name <- PClass.parseVariable
  return $ hInject $ TData name

-- | Parse a literal type
parseLiteralType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseLiteralType = PClass.parseFix @h $ do
  lit <- PClass.parseLiteral
  return $ hInject $ TLiteral lit

-- | Parse a union type
parseUnionType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseUnionType = PClass.parseFix @h $ do
  types <- PClass.parseBrackets $ sepBy (parseMType @h) (PClass.parseSymbol "|")
  return $ hInject $ TUnion types

-- | Parse an intersection type
parseIntersectionType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseIntersectionType = PClass.parseFix @h $ do
  types <- PClass.parseBrackets $ sepBy (parseMType @h) (PClass.parseSymbol "&")
  return $ hInject $ TIntersection types

-- | Parse an unknown type
parseUnknownType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  ) =>
  m (f KType)
parseUnknownType = PClass.parseFix @h $ do
  PClass.parseReservedWord "unknown"
  return $ hInject TUnknown

-- | Parse a never type
parseNeverType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  ) =>
  m (f KType)
parseNeverType = PClass.parseFix @h $ do
  PClass.parseReservedWord "never"
  return $ hInject TNever