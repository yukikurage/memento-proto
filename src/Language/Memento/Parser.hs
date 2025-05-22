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
reservedWords = ["if", "then", "else", "true", "false", "number", "bool", "do", "val", "with", "Throw", "ZeroDiv", "data", "branch"] -- Added "data", "branch"

-- | 識別子
identifier :: Parser Text
identifier = (lexeme . try) $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

{- | コンストラクタ定義のパーサー
例: Cons : number -> bool
-}
constructorDefinitionParser :: Parser ConstructorDef
constructorDefinitionParser = lexeme $ do
  name <- identifier
  symbol ":"

  typ <- typeExpr

  return $ ConstructorDef name typ

{- | データ定義のパーサー
例: data MyList [ Nil, Cons : number -> MyList ];
-}
dataDefinitionParser :: Parser Definition
dataDefinitionParser = lexeme $ do
  rword "data"
  name <- identifier
  constructors <- brackets (sepBy constructorDefinitionParser (symbol ","))
  symbol ";"
  return $ DataDef name constructors

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
例: ( (Cons x xs) ) -> x
-}
clauseParser :: Parser Clause
clauseParser = lexeme $ do
  pat <- parens patternParser -- Pattern enclosed in parentheses
  symbol "->"
  ex <- expr
  return $ Clause pat ex

{- | match式のパーサー
例: branch myValue [ ( (Some x) ) -> x, ( (None) ) -> 0 ]
-}
matchExprParser :: Parser Expr
matchExprParser = lexeme $ do
  rword "branch"
  scrutinee <- identifier
  clauses <- brackets (sepBy (try clauseParser) (symbol ",")) -- List of clauses
  return $ Match scrutinee clauses

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
typeTerm' =
  choice
    [ try $ parens typeExpr -- Recursive call to the new typeExpr
    , TNumber <$ rword "number"
    , TBool <$ rword "bool"
    , TAlgebraicData <$> identifier -- For ADT names
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
typeExpr =
  try
    ( do
        argType <- typeTerm'
        functionTypeSuffixParser argType
    )
    <|> typeTerm'

-- | 型注釈のパーサー
typeAnnotation :: Parser Type
typeAnnotation = typeExpr -- Uses the new typeExpr

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
    , try matchExprParser -- Added matchExprParser
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
    typeTerm'
  void $ symbol "->"
  body <- expr
  return $ Lambda name mType body

-- | do構文
doExpr :: Parser Expr
doExpr = (lexeme . try) $ do
  rword "do"
  name <- identifier
  return $ Do name

-- | 値定義のパーサー
definitionParser :: Parser Definition
definitionParser =
  lexeme $
    choice
      [ try valDefinitionParser
      , try dataDefinitionParser -- Added dataDefinitionParser
      ]

valDefinitionParser :: Parser Definition
valDefinitionParser = do
  rword "val"
  name <- identifier
  symbol ":"
  typ <- typeAnnotation
  symbol ":="
  body <- expr
  symbol ";" -- Ensure semicolon termination
  return $ ValDef name typ body

-- | プログラムのパーサー (トップレベル定義のリスト)
program :: Parser Program
program = Program <$> between sc eof (many definitionParser)

-- | プログラムのパース
parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse program ""
