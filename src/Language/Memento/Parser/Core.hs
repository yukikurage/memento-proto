{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Core (
  Parser,
  sc,
  lexeme,
  symbol,
  parens,
  brackets,
  number,
  rword,
  reservedWords,
  identifier,
  lowerIdentifier,
  upperIdentifier,
) where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | スペース消費パーサー
sc :: Parser ()
sc =
  L.space
    space1 -- スペース、タブを消費
    (L.skipLineComment "//") -- 行コメント
    (L.skipBlockComment "/*" "*/") -- ブロックコメント

-- | レキサー
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | シンボル
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | 括弧で囲まれた式
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 角括弧で囲まれた式
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | 数値（整数または小数）
number :: Parser Double
number =
  lexeme $
    choice
      [ try L.float -- 小数を先にトライ
      , fromIntegral <$> L.decimal -- 整数
      ]

-- | 予約語
rword :: Text -> Parser ()
rword w = try $ lexeme (string w *> notFollowedBy (alphaNumChar <|> char '_'))

-- | 予約語のリスト
reservedWords :: [Text]
reservedWords = ["if", "then", "else", "true", "false", "number", "bool", "do", "val", "with", "Throw", "ZeroDiv", "data", "branch", "effect", "handle"]

-- | 識別子
identifier :: Parser Text
identifier = try $ lexeme $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | lowerIdentifier (先頭が小文字の識別子)
lowerIdentifier :: Parser Text
lowerIdentifier = try $ lexeme $ do
  fc <- lowerChar
  rest <- many (alphaNumChar <|> char '_')
  let name = T.pack (fc : rest)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | upperIdentifier (先頭が大文字の識別子)
upperIdentifier :: Parser Text
upperIdentifier = try $ lexeme $ do
  fc <- upperChar
  rest <- many (alphaNumChar <|> char '_')
  let name = T.pack (fc : rest)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name
