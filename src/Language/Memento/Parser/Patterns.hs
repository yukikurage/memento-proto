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
  newUniqueId,
  number,
  parens,
  rword,
  symbol,
  upperIdentifier,
  upperIdentifierArgument,
  upperIdentifierVariable,
 )
import Language.Memento.Syntax (ArgumentMetadata (ArgumentMetadata), ArgumentWithMetadata (ArgumentWithMetadata), Clause (..), ClauseMetadata (ClauseMetadata), ClauseWithMetadata (ClauseWithMetadata), Expr, ExprWithMetadata, Pattern (..), PatternMetadata (PatternMetadata), PatternWithMetadata (PatternWithMetadata), Variable (Variable))
import Text.Megaparsec
import Text.Megaparsec.Char

-- | パターンパーサー
patternParser :: Parser PatternWithMetadata
patternParser =
  lexeme $
    choice
      [ do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          nm <- number
          return $ PatternWithMetadata (PNumber nm) (PatternMetadata sp uniqueId) -- 数値パターン
      , do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          symbol "true"
          return $ PatternWithMetadata (PBool True) (PatternMetadata sp uniqueId) -- trueパターン
      , do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          symbol "false"
          return $ PatternWithMetadata (PBool False) (PatternMetadata sp uniqueId)
      , -- タプルパターン: [p1, p2, ...]
        do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          patterns <- brackets (patternParser `sepBy` symbol ",")
          return $ PatternWithMetadata (PTuple patterns) (PatternMetadata sp uniqueId)
      , -- コンストラクタパターン: ConstructorName varName
        try $ do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          constructorName <- upperIdentifierVariable -- 大文字で始まる識別子
          -- Changed to parse a nested pattern instead of just a variable name
          pat <- patternParser -- 小文字で始まる識別子または他のパターン
          return $ PatternWithMetadata (PConstructor constructorName pat) (PatternMetadata sp uniqueId)
      , -- ワイルドカードパターン: _
        do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          symbol "_"
          return $ PatternWithMetadata PWildcard (PatternMetadata sp uniqueId)
      , -- 変数パターン: varName (must come after wildcard and constructor to avoid ambiguity)
        do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          varName <- lowerIdentifier
          return $ PatternWithMetadata (PVar (ArgumentWithMetadata (Variable varName) (ArgumentMetadata sp uniqueId))) (PatternMetadata sp uniqueId)
      , do
          parens patternParser
      ]

{- | match節のパーサー
例: (Cons x xs) -> x
-}
clauseParser :: Parser ExprWithMetadata -> Parser ClauseWithMetadata
clauseParser exprParser = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "let"
  pat <- patternParser -- Pattern enclosed in parentheses
  symbol "->"
  ex <- exprParser -- Use the passed parser
  return $ ClauseWithMetadata (Clause pat ex) (ClauseMetadata sp uniqueId)
