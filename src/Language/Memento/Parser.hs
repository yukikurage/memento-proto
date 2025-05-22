{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Memento.Syntax
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
reservedWords = ["if", "then", "else", "true", "false", "number", "bool", "do"]

-- | 識別子
identifier :: Parser Text
identifier = (lexeme . try) $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | 型の項
typeTerm :: Parser Type
typeTerm =
  choice
    [ try $ parens typeExpr
    , TNumber <$ rword "number"
    , TBool <$ rword "bool"
    ]

-- | 型の演算子
typeOp :: Text -> Parser Text
typeOp n = (lexeme . try) (string n <* notFollowedBy (symbolChar <|> punctuationChar))

-- | 型の演算子の優先順位テーブル
typeOperatorTable :: [[Operator Parser Type]]
typeOperatorTable =
  [ [InfixR ((\t1 t2 -> TFunction t1 t2 Set.empty) <$ typeOp "->")]
  ]

-- | 型レベルの式
typeExpr :: Parser Type
typeExpr = makeExprParser typeTerm typeOperatorTable

-- | 型
typeAnnotation :: Parser Type
typeAnnotation = typeExpr

-- | 式
expr :: Parser Expr
expr = makeExprParser term operatorTable

-- | 項
term :: Parser Expr
term =
  choice
    [ try $ parens expr
    , try lambdaExpr
    , try doExpr
    , ifExpr
    , Var <$> identifier
    , Number <$> number
    , Bool <$> (True <$ rword "true" <|> False <$ rword "false")
    ]

op :: Text -> Parser Text
op n = (lexeme . try) (string n <* notFollowedBy (symbolChar <|> punctuationChar))

-- | 演算子の優先順位テーブル
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ InfixL (BinOp Mul <$ op "*")
    , InfixL (BinOp Div <$ op "/")
    ]
  ,
    [ InfixL (BinOp Add <$ op "+")
    , InfixL (BinOp Sub <$ op "-")
    ]
  ,
    [ InfixN (BinOp Eq <$ op "==")
    , InfixN (BinOp Lt <$ op "<")
    , InfixN (BinOp Gt <$ op ">")
    ]
  , [InfixR (Apply <$ op "<|")]
  , [InfixL (flip Apply <$ op "|>")]
  ]

-- | if式
ifExpr :: Parser Expr
ifExpr = (lexeme . try) $ do
  rword "if"
  cond <- term
  rword "then"
  thenExpr <- term
  rword "else"
  If cond thenExpr <$> term

-- | ラムダ式
lambdaExpr :: Parser Expr
lambdaExpr = do
  name <- identifier
  mType <- optional $ do
    void $ symbol ":"
    typeTerm
  void $ symbol "->"
  body <- expr
  return $ Lambda name mType body

-- | do構文
doExpr :: Parser Expr
doExpr = (lexeme . try) $ do
  rword "do"
  name <- identifier
  return $ Do name

-- | プログラム
program :: Parser Expr
program = between sc eof expr

-- | プログラムのパース
parseProgram :: Text -> Either (ParseErrorBundle Text Void) Expr
parseProgram = parse program ""
