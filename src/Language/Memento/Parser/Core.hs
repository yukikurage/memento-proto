{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Core where

import Control.Monad.Combinators.Expr
import qualified Control.Monad.State as State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Language.Memento.Parser.Class as PClass
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text (State.State Int)

-- | スペース消費パーサー
sc :: (MonadParsec s Text m) => m ()
sc =
  L.space
    space1 -- スペース、タブ、改行を消費
    (L.skipLineComment "//") -- 行コメント
    (L.skipBlockComment "/*" "*/") -- ブロックコメント

-- | レキサー
parseLexeme :: (MonadParsec s Text m) => m a -> m a
parseLexeme = L.lexeme sc

parseSymbol :: (MonadParsec s Text m) => Text -> m Text
parseSymbol n =
  try $ L.symbol sc n <* notFollowedBy (symbolChar <|> punctuationChar)

-- | 括弧で囲まれた式
parseParens :: (MonadParsec s Text m) => m a -> m a
parseParens = between (parseSymbol "(") (parseSymbol ")")

-- | 角括弧で囲まれた式
parseBrackets :: (MonadParsec s Text m) => m a -> m a
parseBrackets = between (parseSymbol "[") (parseSymbol "]")

-- | 中括弧で囲まれた式
parseBraces :: (MonadParsec s Text m) => m a -> m a
parseBraces = between (parseSymbol "{") (parseSymbol "}")

-- | 予約語のリスト
reservedWords :: [Text]
reservedWords = ["if", "then", "else", "true", "false", "number", "bool", "val", "with", "data", "branch", "effect", "handle", "let"]

-- | 予約語
parseReservedWord :: (MonadParsec s Text m) => Text -> m ()
parseReservedWord w = try $ parseLexeme (parseSymbol w *> notFollowedBy (alphaNumChar <|> char '_'))

-- | 識別子
parseIdentifier :: (MonadParsec s Text m, MonadFail m) => m Text
parseIdentifier = try $ parseLexeme $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name
