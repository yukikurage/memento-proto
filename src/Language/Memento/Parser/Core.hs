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
  lowerCamelCaseIdentifier,
  pascalCaseIdentifier,
  snakeCaseIdentifier
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
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- | 予約語のリスト
reservedWords :: [Text]
reservedWords = ["if", "then", "else", "true", "false", "number", "bool", "do", "val", "with", "Throw", "ZeroDiv", "data", "branch", "effect", "handle"]

-- | 識別子
identifier :: Parser Text
identifier = (lexeme . try) $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | lowerCamelCaseIdentifier
lowerCamelCaseIdentifier :: Parser Text
lowerCamelCaseIdentifier = (lexeme . try) $ do
  fc <- lowerChar
  rest <- many alphaNumChar
  let name = T.pack (fc : rest)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | pascalCaseIdentifier
pascalCaseIdentifier :: Parser Text
pascalCaseIdentifier = (lexeme . try) $ do
  fc <- upperChar
  rest <- many alphaNumChar
  let name = T.pack (fc : rest)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | snakeCaseIdentifier
snakeCaseIdentifier :: Parser Text
snakeCaseIdentifier = (lexeme . try) $ do
  nameStr <- (:) <$> lowerChar <*> many (lowerChar <|> digitChar <|> char '_')
  let name = T.pack nameStr
  if "__" `T.isInfixOf` name || T.isSuffixOf "_" name
    then fail "identifier cannot contain consecutive underscores or end with an underscore"
    else if name `elem` reservedWords
      then fail $ "keyword " <> show name <> " cannot be an identifier"
      else return name
