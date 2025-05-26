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
  lowerIdentifier, -- 新しい識別子に統一
  parens,
  rword,
  sc,
  symbol,
  upperIdentifier,
 )
import Language.Memento.Parser.Expressions (expr)
import Language.Memento.Parser.Types (typeAnnotation, typeExpr)
import Language.Memento.Syntax (ConstructorDef (..), Definition (..), OperatorDef (..))
import Text.Megaparsec
import Text.Megaparsec.Char

{- | コンストラクタ定義のパーサー
例: Cons : number -> bool
-}
constructorDefinitionParser :: Parser ConstructorDef
constructorDefinitionParser = lexeme $ do
  name <- upperIdentifier -- 大文字で始まる識別子
  symbol ":"
  typ <- typeExpr -- from Types
  return $ ConstructorDef name typ

{- | エフェクトオペレータ定義のパーサー
例: NumBool : number -> bool
-}
operatorDefParser :: Parser OperatorDef
operatorDefParser = lexeme $ do
  name <- upperIdentifier -- 大文字で始まる識別子
  symbol ":"
  typ <- typeExpr -- from Types
  return $ OperatorDef name typ

{- | データ定義のパーサー
例: data MyList [ Nil, Cons : number -> MyList ];
-}
dataDefinitionParser :: Parser Definition
dataDefinitionParser = lexeme $ do
  rword "data"
  name <- upperIdentifier -- 大文字で始まる識別子
  constructors <- brackets (sepEndBy constructorDefinitionParser (symbol ","))
  symbol ";"
  return $ DataDef name constructors

{- | エフェクト定義のパーサー
例: effect Trans [ NumBool : number -> bool, BoolNum : bool -> number ];
-}
effectDefinitionParser :: Parser Definition
effectDefinitionParser = lexeme $ do
  rword "effect"
  name <- upperIdentifier -- 大文字で始まる識別子
  operators <- brackets (sepEndBy operatorDefParser (symbol ","))
  symbol ";"
  return $ EffectDef name operators

valDefinitionParser :: Parser Definition
valDefinitionParser = do
  rword "val"
  name <- lowerIdentifier -- 小文字で始まる識別子
  symbol ":"
  typ <- typeAnnotation -- from Types
  symbol ":="
  body <- expr -- from Expressions
  symbol ";" -- Ensure semicolon termination
  return $ ValDef name typ body

-- | 値定義のパーサー
definitionParser :: Parser Definition
definitionParser =
  lexeme $
    choice
      [ valDefinitionParser
      , dataDefinitionParser
      , effectDefinitionParser
      ]
