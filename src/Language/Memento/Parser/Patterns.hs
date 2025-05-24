{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Patterns (
  patternParser,
  clauseParser
) where

import Language.Memento.Syntax (Pattern (..), Clause (..))
import Language.Memento.Parser.Core
import Language.Memento.Parser.Expressions (expr)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | パターンパーサー
patternParser :: Parser Pattern
patternParser =
  lexeme $
    choice
      [ -- コンストラクタパターン: (ConstructorName var1 var2 ...)
        try $ do
          constructorName <- identifier
          varNames <- many identifier -- Zero or more variable names
          return $ PConstructor constructorName varNames
      , -- ワイルドカードパターン: _
        PWildcard <$ symbol "_"
      , -- 変数パターン: varName (must come after wildcard and constructor to avoid ambiguity)
        PVar <$> identifier
      ]

{- | match節のパーサー
例: (Cons x xs) -> x
-}
clauseParser :: Parser Clause
clauseParser = lexeme $ do
  pat <- parens patternParser -- Pattern enclosed in parentheses
  symbol "->"
  ex <- expr -- Depends on expr from Language.Memento.Parser.Expressions
  return $ Clause pat ex
