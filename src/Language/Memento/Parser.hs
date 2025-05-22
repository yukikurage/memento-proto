{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
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
reservedWords = ["if", "then", "else", "true", "false", "number", "bool"]

-- | 識別子
identifier :: Parser Text
identifier = (lexeme . try) $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | 型
typeAnnotation :: Parser Type
typeAnnotation =
  choice
    [ TNumber <$ rword "number"
    , TBool <$ rword "bool"
    , -- func(type, type)
      do
        rword "func"
        void $ symbol "("
        t1 <- typeAnnotation
        void $ symbol ","
        t2 <- typeAnnotation
        void $ symbol ")"
        return $ TFunction t1 t2
    ]

-- | 式
expr :: Parser Expr
expr = makeExprParser term operatorTable

-- | 項
term :: Parser Expr
term =
  choice
    [ Number <$> number
    , Bool <$> (True <$ rword "true" <|> False <$ rword "false")
    , try ifExpr
    , try $ parens expr
    , try lambdaExpr
    , Var <$> identifier
    ]

-- | 演算子の優先順位テーブル
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ InfixL (BinOp Mul <$ symbol "*")
    , InfixL (BinOp Div <$ symbol "/")
    ]
  ,
    [ InfixL (BinOp Add <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-")
    ]
  ,
    [ InfixN (BinOp Eq <$ symbol "==")
    , InfixN (BinOp Lt <$ symbol "<")
    ]
  ,
    [ InfixL (flip Apply <$ symbol "|>")
    ]
  , [InfixL (Apply <$ symbol "<|")]
  ]

-- | if式
ifExpr :: Parser Expr
ifExpr = (lexeme . try) $ do
  rword "if"
  cond <- expr
  rword "then"
  thenExpr <- expr
  rword "else"
  If cond thenExpr <$> expr

-- | ラムダ式
lambdaExpr :: Parser Expr
lambdaExpr = do
  name <- identifier
  mType <- optional $ do
    void $ symbol ":"
    typeAnnotation
  void $ symbol ";"
  Lambda name mType <$> expr

-- | プログラム
program :: Parser Expr
program = between sc eof expr

-- | プログラムのパース
parseProgram :: Text -> Either (ParseErrorBundle Text Void) Expr
parseProgram = parse program ""