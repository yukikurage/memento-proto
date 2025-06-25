{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser.Types where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.MType (MType (TApplication, TBool, TFunction, TInt, TIntersection, TLiteral, TNever, TNumber, TString, TUnion, TUnknown, TVar))
import Language.Memento.Syntax.Variable (Variable(Var))
import Language.Memento.Syntax.Tag (KType)
import Text.Megaparsec (MonadParsec (try), choice, getSourcePos, many, sepBy, (<?>))

{- | Parse a type with precedence levels:

1. typeAtom: primitives, variables, parenthesized expressions
2. typeIntersection: typeAtom (&) typeAtom ...
3. typeUnion: typeIntersection (|) typeIntersection ...
4. typeFunction: (args) => typeUnion

This creates proper operator precedence where & binds tighter than |
-}
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
parseMType = parseTypeExpr @h

-- | Top-level type expression parser
parseTypeExpr ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseTypeExpr = try (parseTypeFunction @h) <|> parseTypeUnion @h

-- | Parse a function type: (arg : type, ...) => returnType
parseTypeFunction ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseTypeFunction =
  ( PClass.parseFix @h $ do
      _ <- getSourcePos -- fetch position for possible future use
      -- Parse argument types inside parentheses - with explicit zero-argument handling
      args <- PClass.parseParens $ 
        (sepBy ((,) <$> PClass.parseVariable <* PClass.parseSymbol ":" <*> parseTypeExpr @h) (PClass.parseSymbol ","))
      PClass.parseSymbol "=>"
      ret <- parseTypeExpr @h
      return $ hInject $ TFunction args ret
  )
    <?> "function type"

-- | Parse a union type: type | type | ...
parseTypeUnion ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseTypeUnion =
  ( do
      _ <- getSourcePos
      -- Parse first type
      first <- parseTypeIntersection @h
      -- Try to parse a union
      rest <- many $ do
        _ <- PClass.parseSymbol "|"
        parseTypeIntersection @h

      -- If we found union parts, construct a union type
      case rest of
        [] -> pure first -- Just the first type
        _ -> PClass.parseFix @h $ return $ hInject $ TUnion (first : rest)
  )
    <?> "union type"

-- | Parse an intersection type: type & type & ...
parseTypeIntersection ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseTypeIntersection =
  ( do
      _ <- getSourcePos
      -- Parse first type
      first <- parseTypeAtom @h
      -- Try to parse an intersection
      rest <- many $ do
        _ <- PClass.parseSymbol "&"
        parseTypeAtom @h

      -- If we found intersection parts, construct an intersection type
      case rest of
        [] -> pure first -- Just the first type
        _ -> PClass.parseFix @h $ return $ hInject $ TIntersection (first : rest)
  )
    <?> "intersection type"

-- | Parse an atomic type (lowest level of precedence)
parseTypeAtom ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseTypeAtom =
  choice
    [ PClass.parseParens (parseTypeExpr @h) <?> "parenthesized type"
    , try (parseTypeApplication @h) <?> "type application"
    , parseVarType @h <?> "type variable"
    , parsePrimitiveType @h <?> "primitive type"
    , parseLiteralType @h <?> "literal type"
    , parseUnknownType @h <?> "unknown type"
    , parseNeverType @h <?> "never type"
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
parseVarType =
  ( PClass.parseFix @h $ do
      _ <- getSourcePos
      var <- PClass.parseTypeVariable
      return $ hInject $ TVar var
  )
    <?> "type variable"

-- | Parse a type application like List<T> or Map<K, V>
parseTypeApplication ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  , PClass.LiteralParser f m
  ) =>
  m (f KType)
parseTypeApplication =
  ( PClass.parseFix @h $ do
      _ <- getSourcePos
      -- Parse the base type (as a type variable for now)
      baseVar <- PClass.parseTypeVariable
      
      -- Parse the type arguments <T1, T2, ...> using proper angle bracket parser
      args <- PClass.parseAngleBrackets $ sepBy (parseTypeExpr @h) (PClass.parseSymbol ",")
      
      return $ hInject $ TApplication baseVar args
  )
    <?> "type application"

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
  ( do
      _ <- getSourcePos
      PClass.parseFix @h $
        choice
          [ PClass.parseReservedWord "number" >> return (hInject TNumber)
          , PClass.parseReservedWord "int" >> return (hInject TInt)
          , PClass.parseReservedWord "bool" >> return (hInject TBool)
          , PClass.parseReservedWord "string" >> return (hInject TString)
          ]
  )
    <?> "primitive type"

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
parseLiteralType =
  ( PClass.parseFix @h $ do
      _ <- getSourcePos
      lit <- PClass.parseLiteral
      return $ hInject $ TLiteral lit
  )
    <?> "literal type"

-- | Parse an unknown type
parseUnknownType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  ) =>
  m (f KType)
parseUnknownType =
  ( PClass.parseFix @h $ do
      PClass.parseReservedWord "unknown"
      return $ hInject TUnknown
  )
    <?> "unknown type"

-- | Parse a never type
parseNeverType ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective MType h
  , PClass.FixParser h f m
  ) =>
  m (f KType)
parseNeverType =
  ( PClass.parseFix @h $ do
      _ <- getSourcePos
      PClass.parseReservedWord "never"
      return $ hInject TNever
  )
    <?> "never type"