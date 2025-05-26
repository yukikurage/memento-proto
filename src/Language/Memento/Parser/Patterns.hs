{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Patterns (
  patternParser,
  clauseParser,
) where

import Language.Memento.Parser.Core (
  Parser,
  brackets,
  lexeme,
  lowerIdentifier,
  number,
  parens,
  rword,
  symbol,
  upperIdentifier,
 )
import Language.Memento.Syntax (Clause (..), Expr, Pattern (..))
import Text.Megaparsec
import Text.Megaparsec.Char

-- | パターンパーサー
patternParser :: Parser Pattern
patternParser =
  lexeme $
    choice
      [ PNumber <$> number -- 数値パターン
      , PBool True <$ symbol "true" -- trueパターン
      , PBool False <$ symbol "false" -- falseパターン
      , -- タプルパターン: [p1, p2, ...]
        PTuple <$> brackets (patternParser `sepBy` symbol ",")
      , -- コンストラクタパターン: ConstructorName varName
        try $ do
          constructorName <- upperIdentifier -- 大文字で始まる識別子
          varName <- lowerIdentifier -- 小文字で始まる識別子
          return $ PConstructor constructorName varName
      , -- ワイルドカードパターン: _
        PWildcard <$ symbol "_"
      , -- 変数パターン: varName (must come after wildcard and constructor to avoid ambiguity)
        PVar <$> lowerIdentifier -- 小文字で始まる識別子
      , parens patternParser
      ]

{- | match節のパーサー
例: (Cons x xs) -> x
-}
clauseParser :: Parser Expr -> Parser Clause
clauseParser exprParser = lexeme $ do
  rword "let"
  pat <- patternParser -- Pattern enclosed in parentheses
  symbol "->"
  ex <- exprParser -- Use the passed parser
  return $ Clause pat ex
