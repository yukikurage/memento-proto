{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Memento.Syntax (BinOp (..), Definition (..), Effect (..), Effects, Expr (..), Program (..), Type (..))
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
reservedWords = ["if", "then", "else", "true", "false", "number", "bool", "do", "val", "with", "Throw", "ZeroDiv"]

-- | 識別子
identifier :: Parser Text
identifier = (lexeme . try) $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | エフェクトのパーサー
effectParser :: Parser Effect
effectParser =
  (Throw <$ rword "Throw")
    <|> (ZeroDiv <$ rword "ZeroDiv")

effectsParser :: Parser Effects
effectsParser =
  symbol "<"
    *> (Set.fromList <$> sepBy effectParser (symbol ","))
    <* symbol ">"
    <|> pure Set.empty -- Allows for `with` followed by nothing, implying <>

-- | 型の項 (非関数型、括弧で囲まれた型)
typeTerm' :: Parser Type
typeTerm' = choice
  [ try $ parens typeExpr -- Recursive call to the new typeExpr
  , TNumber <$ rword "number"
  , TBool <$ rword "bool"
  ]

-- | 関数型の右辺のパーサー (-> Type [with Effects])
functionTypeSuffixParser :: Type -> Parser Type
functionTypeSuffixParser argType = do
  symbol "->"
  retType <- typeExpr -- Changed from typeExpr' to typeExpr for mutual recursion
  effects <- option Set.empty (rword "with" *> effectsParser)
  return $ TFunction argType retType effects

-- | 型のパーサー (関数型を含む)
typeExpr :: Parser Type
typeExpr = try (do
    argType <- typeTerm'
    functionTypeSuffixParser argType
  ) <|> typeTerm'

-- | 型注釈のパーサー
typeAnnotation :: Parser Type
typeAnnotation = typeExpr -- Uses the new typeExpr

-- | 型とエフェクトのペアをパース (val宣言用)
typeWithEffectsParser :: Parser (Type, Effects)
typeWithEffectsParser = do
  t <- typeExpr
  effs <- option Set.empty (rword "with" *> effectsParser)
  return (t, effs)

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

-- | do構文
doExpr :: Parser Expr
doExpr = (lexeme . try) $ do
  rword "do"
  name <- identifier
  return $ Do name

-- | 値定義のパーサー
definitionParser :: Parser Definition
definitionParser = do
  rword "val"
  name <- identifier
  symbol ":"
  (typ, effs) <- typeWithEffectsParser
  symbol "="
  body <- expr
  symbol ";" -- Ensure semicolon termination
  return $ ValDef name typ effs body

-- | プログラムのパーサー (トップレベル定義のリスト)
program :: Parser Program
program = Program <$> between sc eof (many definitionParser)

-- | プログラムのパース
parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse program ""
