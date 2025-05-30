{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Definitions (
  constructorDefinitionParser,
  operatorDefParser,
  dataDefinitionParser,
  effectDefinitionParser,
  valDefinitionParser,
  definitionParser,
) where

import Language.Memento.Parser.Core (
  Parser,
  brackets,
  lexeme,
  lowerIdentifier,
  lowerIdentifierArgument,
  newUniqueId,
  parens,
  rword,
  sc,
  symbol,
  upperIdentifier,
  upperIdentifierArgument,
  upperIdentifierTypeArgument,
  upperIdentifierVariable,
 )
import Language.Memento.Parser.Types (typeAnnotation, typeExpr)
import Language.Memento.Syntax (ConstructorDef (..), ConstructorDefMetadata (ConstructorDefMetadata), ConstructorDefWithMetadata (ConstructorDefWithMetadata), Definition (..), DefinitionMetadata (DefinitionMetadata), DefinitionWithMetadata (DefinitionWithMetadata), ExprWithMetadata, OperatorDef (..), OperatorDefMetadata (OperatorDefMetadata), OperatorDefWithMetadata (OperatorDefWithMetadata))
import Text.Megaparsec
import Text.Megaparsec.Char

{- | コンストラクタ定義のパーサー
例: Cons : number -> bool
-}
constructorDefinitionParser :: Parser ConstructorDefWithMetadata
constructorDefinitionParser = lexeme $ do
  name <- upperIdentifierArgument -- 大文字で始まる識別子
  symbol ":"
  typ <- typeExpr -- from Types
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ ConstructorDefWithMetadata (ConstructorDef name typ) (ConstructorDefMetadata sp uniqueId)

{- | エフェクトオペレータ定義のパーサー
例: NumBool : number -> bool
-}
operatorDefParser :: Parser OperatorDefWithMetadata
operatorDefParser = lexeme $ do
  name <- upperIdentifierArgument -- 大文字で始まる識初
  symbol ":"
  typ <- typeExpr -- from Types
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ OperatorDefWithMetadata (OperatorDef name typ) (OperatorDefMetadata sp uniqueId)

{- | データ定義のパーサー
例: data MyList [ Nil, Cons : number -> MyList ];
-}
dataDefinitionParser :: Parser DefinitionWithMetadata
dataDefinitionParser = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "data"
  name <- upperIdentifierTypeArgument -- 大文字で始まる識別子
  constructors <- brackets (sepEndBy constructorDefinitionParser (symbol ","))
  symbol ";"
  return $ DefinitionWithMetadata (DataDef name constructors) (DefinitionMetadata sp uniqueId)

{- | エフェクト定義のパーサー
例: effect Trans [ NumBool : number -> bool, BoolNum : bool -> number ];
-}
effectDefinitionParser :: Parser DefinitionWithMetadata
effectDefinitionParser = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "effect"
  name <- upperIdentifierTypeArgument -- 大文字で始まる識別子
  operators <- brackets (sepEndBy operatorDefParser (symbol ","))
  symbol ";"
  return $ DefinitionWithMetadata (EffectDef name operators) (DefinitionMetadata sp uniqueId)

valDefinitionParser :: Parser ExprWithMetadata -> Parser DefinitionWithMetadata
valDefinitionParser exprParser = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "val"
  name <- lowerIdentifierArgument -- 小文字で始まる識別子
  symbol ":"
  typ <- typeAnnotation -- from Types
  symbol ":="
  body <- exprParser -- from Expressions
  symbol ";" -- Ensure semicolon termination
  return $ DefinitionWithMetadata (ValDef name typ body) (DefinitionMetadata sp uniqueId)

-- | 値定義のパーサー
definitionParser :: Parser ExprWithMetadata -> Parser DefinitionWithMetadata
definitionParser exprParser =
  lexeme $
    choice
      [ valDefinitionParser exprParser
      , dataDefinitionParser
      , effectDefinitionParser
      ]
