{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.Parser.Definitions where

import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.MType (MType (TFunction, TVar))
import Language.Memento.Syntax.Tag (KDefinition)
import Text.Megaparsec (MonadParsec, choice)

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
  name <- PClass.parseVariable
  PClass.parseSymbol ":"
  typ <- PClass.parseMType
  PClass.parseSymbol ";"
  return $ hInject $ DataDef name typ

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
  PClass.parseSymbol ":"
  typ <- PClass.parseMType
  PClass.parseSymbol "="
  body <- PClass.parseExpr
  PClass.parseSymbol ";"
  return $ hInject $ ValDef name typ body

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
    [ PClass.parseReservedWord "val" *> parseValDefinition @h
    , PClass.parseReservedWord "data" *> parseDataDefinition @h
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
