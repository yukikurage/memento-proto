{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Patterns (
  patternParser,
  clauseParser,
) where

import Language.Memento.Parser.Core
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
      , -- タプルパターン: (p1, p2, ...)
        try $ PTuple <$> parens (patternParser `sepBy` symbol ",")
      , -- コンストラクタパターン: ConstructorName varName
        -- Note: Original PConstructor was `identifier >> identifier`.
        -- This implies a constructor name followed by a single variable name.
        -- For example, `Just x` or `Cons y`. If it's a nullary constructor
        -- like `Nothing`, it might be intended to be `PConstructor "Nothing" "someImplicitOrIgnoredVar"`.
        -- Or, more likely, for nullary constructors, it should be `PConstructor "Nothing" ""`,
        -- or the PConstructor type/parsing should be more flexible.
        -- For now, sticking to the existing structure for PConstructor.
        try $ do
          constructorName <- identifier
          varName <- identifier
          return $ PConstructor constructorName varName
      , -- ワイルドカードパターン: _
        PWildcard <$ symbol "_"
      , -- 変数パターン: varName (must come after others to avoid ambiguity)
        PVar <$> identifier
      ]

{- | match節のパーサー
例: (Cons x xs) -> x
-}
clauseParser :: Parser Expr -> Parser Clause
clauseParser exprParser = lexeme $ do
  -- The original clauseParser was `pat <- parens patternParser`.
  -- This implies patterns in clauses *must* be enclosed in parentheses.
  -- e.g., `(x) -> expr` or `((A, B)) -> expr`.
  -- If simple patterns like `x -> expr` or `_ -> expr` are desired without mandatory outer parens,
  -- this should be `pat <- patternParser`.
  -- However, the example `(Cons x xs) -> x` fits the `parens patternParser` model.
  -- For tuple patterns like `(a,b)`, `parens (PTuple <$> parens (patternParser `sepBy` symbol ","))`
  -- would mean `((p1, p2)) -> expr`.
  -- If `(p1, p2) -> expr` is desired, then `patternParser` (which includes PTuple)
  -- should be directly used.
  -- The prompt for PTuple is `(x,y,z)`, suggesting the parens are part of the tuple pattern itself.
  -- So `patternParser` should correctly parse `(x,y,z)` as one PTuple.
  -- Thus, `pat <- patternParser` seems more appropriate for the clause.
  -- I will keep `parens patternParser` as per original code for now,
  -- as the task is to update `patternParser` not `clauseParser`.
  pat <- parens patternParser -- Pattern enclosed in parentheses
  symbol "->"
  ex <- exprParser -- Use the passed parser
  return $ Clause pat ex
