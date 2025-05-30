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
  lowerIdentifierVariable,
  lowerIdentifierArgument,
  lowerIdentifierTypeVariable,
  lowerIdentifierTypeArgument,
  upperIdentifier,
  upperIdentifierVariable,
  upperIdentifierArgument,
  upperIdentifierTypeVariable,
  upperIdentifierTypeArgument,
  newUniqueId,
) where

import Control.Monad.Combinators.Expr
import qualified Control.Monad.State as State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Memento.Syntax (ArgumentMetadata (ArgumentMetadata), ArgumentWithMetadata (ArgumentWithMetadata), TypeArgumentMetadata (TypeArgumentMetadata), TypeArgumentWithMetadata (TypeArgumentWithMetadata), TypeVariable (TypeVariable), TypeVariableMetadata (TypeVariableMetadata), TypeVariableWithMetadata (TypeVariableWithMetadata), Variable (Variable), VariableMetadata (VariableMetadata), VariableWithMetadata (VariableWithMetadata))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text (State.State Int)

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

lowerIdentifierVariable :: Parser VariableWithMetadata
lowerIdentifierVariable = do
  name <- lowerIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ VariableWithMetadata (Variable name) (VariableMetadata sp uniqueId)

lowerIdentifierArgument :: Parser ArgumentWithMetadata
lowerIdentifierArgument = do
  name <- lowerIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ ArgumentWithMetadata (Variable name) (ArgumentMetadata sp uniqueId)

lowerIdentifierTypeVariable :: Parser TypeVariableWithMetadata
lowerIdentifierTypeVariable = do
  name <- lowerIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ TypeVariableWithMetadata (TypeVariable name) (TypeVariableMetadata sp uniqueId)

lowerIdentifierTypeArgument :: Parser TypeArgumentWithMetadata
lowerIdentifierTypeArgument = do
  name <- lowerIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ TypeArgumentWithMetadata (TypeVariable name) (TypeArgumentMetadata sp uniqueId)

-- | upperIdentifier (先頭が大文字の識別子)
upperIdentifier :: Parser Text
upperIdentifier = try $ lexeme $ do
  fc <- upperChar
  rest <- many (alphaNumChar <|> char '_')
  let name = T.pack (fc : rest)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

upperIdentifierVariable :: Parser VariableWithMetadata
upperIdentifierVariable = do
  name <- upperIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ VariableWithMetadata (Variable name) (VariableMetadata sp uniqueId)

upperIdentifierArgument :: Parser ArgumentWithMetadata
upperIdentifierArgument = do
  name <- upperIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ ArgumentWithMetadata (Variable name) (ArgumentMetadata sp uniqueId)

upperIdentifierTypeVariable :: Parser TypeVariableWithMetadata
upperIdentifierTypeVariable = do
  name <- upperIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ TypeVariableWithMetadata (TypeVariable name) (TypeVariableMetadata sp uniqueId)

upperIdentifierTypeArgument :: Parser TypeArgumentWithMetadata
upperIdentifierTypeArgument = do
  name <- upperIdentifier
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ TypeArgumentWithMetadata (TypeVariable name) (TypeArgumentMetadata sp uniqueId)

newUniqueId :: Parser Int
newUniqueId = do
  id <- State.get
  State.put (id + 1)
  return id
