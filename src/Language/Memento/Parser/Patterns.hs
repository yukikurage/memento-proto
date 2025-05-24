{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Patterns (
  patternParser,
  clauseParser,
) where

import Language.Memento.Parser.Core (
  Parser,
  lexeme,
  lowerIdentifier, -- 新しい識別子に統一
  parens,
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
      [ -- コンストラクタパターン: (ConstructorName varName) - Assuming one variable for now as per PConstructor structure
        try $ do
          constructorName <- upperIdentifier -- 大文字で始まる識別子
          varName <- lowerIdentifier -- 小文字で始まる識別子
          return $ PConstructor constructorName varName
      , -- ワイルドカードパターン: _
        PWildcard <$ symbol "_"
      , -- 変数パターン: varName (must come after wildcard and constructor to avoid ambiguity)
        PVar <$> lowerIdentifier -- 小文字で始まる識別子
      ]

{- | match節のパーサー
例: (Cons x xs) -> x
-}
clauseParser :: Parser Expr -> Parser Clause
clauseParser exprParser = lexeme $ do
  pat <- parens patternParser -- Pattern enclosed in parentheses
  symbol "->"
  ex <- exprParser -- Use the passed parser
  return $ Clause pat ex
