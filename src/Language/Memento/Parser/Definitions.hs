{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.Parser.Definitions where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax (extractSyntax, unMType)
import Language.Memento.Syntax.Definition (ConstructorDef (..), Definition (..))
import qualified Language.Memento.Syntax.MType as SMType
import Language.Memento.Syntax.Tag (KDefinition, KType, KVariable)
import Text.Megaparsec (MonadParsec, choice, sepBy, try, (<?>))
import Text.Megaparsec.Char (string)

-- | Parse optional type parameter list like <T, U, V>
parseTypeParameters ::
  forall f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.VariableParser f m
  ) =>
  m [f KVariable]
parseTypeParameters = do
  try (PClass.parseAngleBrackets $ sepBy PClass.parseVariable (PClass.parseSymbol ","))
    <|> return [] -- Empty list if no type parameters

parseTypeAssignments ::
  forall f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.VariableParser f m
  , PClass.MTypeParser f m
  ) =>
  m [f KType]
parseTypeAssignments = do
  try (PClass.parseAngleBrackets $ sepBy PClass.parseMType (PClass.parseSymbol ","))
    <|> return [] -- Empty list if no assignments

-- Parse individual constructor: ConstructorName<T, U> : (args...) => ReturnType
parseConstructorDef ::
  forall f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  ) =>
  m (ConstructorDef f)
parseConstructorDef = do
  constructorName <- PClass.parseVariable
  ctorTypeParams <- parseTypeParameters
  PClass.parseSymbol ":"
  constructorType <- PClass.parseMType
  return $ ConstructorDef constructorName ctorTypeParams constructorType

-- Parse new multi-constructor data definition
-- data Maybe [Some<T> : (x : T) => Maybe<T>, None<T> : () => Maybe<T>];
parseDataDefinition ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , Injective Definition h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseDataDefinition = PClass.parseFix @h $ do
  dataName <- PClass.parseVariable
  constructors <- PClass.parseBrackets $ sepBy parseConstructorDef (PClass.parseSymbol ",")
  _ <- PClass.parseSymbol ";"
  return $ hInject $ DataDef dataName constructors

parseValDefinition ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , PClass.ExpressionParser f m
  , Injective Definition h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseValDefinition = PClass.parseFix @h $ do
  name <- PClass.parseVariable
  typeParams <- parseTypeParameters
  PClass.parseSymbol ":"
  typ <- PClass.parseMType
  PClass.parseSymbol ":="
  body <- PClass.parseExpr
  _ <- PClass.parseSymbol ";"
  return $ hInject $ ValDef name typeParams typ body

parseTypeDef ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , Injective Definition h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseTypeDef = PClass.parseFix @h $ do
  name <- PClass.parseVariable
  typeParams <- parseTypeParameters
  PClass.parseSymbol ":="
  typ <- PClass.parseMType
  _ <- PClass.parseSymbol ";"
  return $ hInject $ TypeDef name typeParams typ

parseDefinition ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , PClass.ExpressionParser f m
  , Injective Definition h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseDefinition =
  choice
    [ PClass.parseReservedWord "val" *> parseValDefinition @h <?> "val definition"
    , PClass.parseReservedWord "data" *> parseDataDefinition @h <?> "data definition"
    , PClass.parseReservedWord "type" *> parseTypeDef @h <?> "type definition"
    ]

-- {- | エフェクトオペレータ定義のパーサー
-- 例: NumBool : number -> bool
-- -}
-- operatorDefParser :: Parser OperatorDefWithMetadata
-- operatorDefParser = lexeme $ do
--   name <- upperIdentifierArgument -- 大文字で始まる識初
--   symbol ":"
--   typ <- typeExpr -- from Types
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   return $ OperatorDefWithMetadata (OperatorDef name typ) (OperatorDefMetadata sp uniqueId)

-- {- | データ定義のパーサー
-- 例: data MyList [ Nil, Cons : number -> MyList ];
-- -}
-- dataDefinitionParser :: Parser DefinitionWithMetadata
-- dataDefinitionParser = lexeme $ do
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   rword "data"
--   name <- upperIdentifierTypeArgument -- 大文字で始まる識別子
--   constructors <- brackets (sepEndBy constructorDefinitionParser (symbol ","))
--   symbol ";"
--   return $ DefinitionWithMetadata (DataDef name constructors) (DefinitionMetadata sp uniqueId)

-- {- | エフェクト定義のパーサー
-- 例: effect Trans [ NumBool : number -> bool, BoolNum : bool -> number ];
-- -}
-- effectDefinitionParser :: Parser DefinitionWithMetadata
-- effectDefinitionParser = lexeme $ do
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   rword "effect"
--   name <- upperIdentifierTypeArgument -- 大文字で始まる識別子
--   operators <- brackets (sepEndBy operatorDefParser (symbol ","))
--   symbol ";"
--   return $ DefinitionWithMetadata (EffectDef name operators) (DefinitionMetadata sp uniqueId)

-- valDefinitionParser :: Parser ExprWithMetadata -> Parser DefinitionWithMetadata
-- valDefinitionParser exprParser = lexeme $ do
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   rword "val"
--   name <- lowerIdentifierArgument -- 小文字で始まる識別子
--   symbol ":"
--   typ <- typeAnnotation -- from Types
--   symbol ":="
--   body <- exprParser -- from Expressions
--   symbol ";" -- Ensure semicolon termination
--   return $ DefinitionWithMetadata (ValDef name typ body) (DefinitionMetadata sp uniqueId)

-- -- | 値定義のパーサー
-- definitionParser :: Parser ExprWithMetadata -> Parser DefinitionWithMetadata
-- definitionParser exprParser =
--   lexeme $
--     choice
--       [ valDefinitionParser exprParser
--       , dataDefinitionParser
--       , effectDefinitionParser
--       ]
