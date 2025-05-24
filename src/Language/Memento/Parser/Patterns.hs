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
      [ try (PNumber <$> number) -- 数値パターン
      , try (PBool True <$ symbol "true") -- trueパターン
      , try (PBool False <$ symbol "false") -- falseパターン
      , -- タプルパターン: [p1, p2, ...]
        try $ PTuple <$> brackets (patternParser `sepBy` symbol ",")
      , -- コンストラクタパターン: ConstructorName varName
        -- Note: Original PConstructor was `identifier >> identifier`.
        -- This implies a constructor name followed by a single variable name.
        -- For example, `Just x` or `Cons y`. If it's a nullary constructor
        -- like `Nothing`, it might be intended to be `PConstructor "Nothing" "someImplicitOrIgnoredVar"`.
        -- Or, more likely, for nullary constructors, it should be `PConstructor "Nothing" ""`,
        -- or the PConstructor type/parsing should be more flexible.
        -- For now, sticking to the existing structure for PConstructor.
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
