{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Expressions (
  expr,
  term,
  ifExpr,
  lambdaExpr,
  matchExprParser,
  handlerExprParser,
  operatorTable, -- Exported for use in Patterns if ever needed, though likely not.
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Language.Memento.Parser.Core (
  Parser,
  brackets,
  identifier,
  lexeme,
  lowerIdentifier,
  lowerIdentifierArgument,
  lowerIdentifierVariable,
  newUniqueId,
  number,
  parens,
  rword,
  symbol,
  upperIdentifier,
  upperIdentifierArgument,
  upperIdentifierTypeVariable,
  upperIdentifierVariable,
 )

import Language.Memento.Parser.Patterns (clauseParser, patternParser) -- clauseParser for matchExprParser
import Language.Memento.Parser.Types (typeExpr, typeTerm)
import Language.Memento.Syntax (BinOp (..), Expr (..), ExprMetadata (ExprMetadata), ExprWithMetadata (ExprWithMetadata), HandlerClause (..), HandlerClauseMetadata (HandlerClauseMetadata), HandlerClauseWithMetadata (HandlerClauseWithMetadata))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- For L.symbol inside op, if still needed

-- Forward declaration for expr to be used in clauseParser, imported by Patterns
-- This is a common way to handle mutual recursion across modules if you split them.
-- However, given our current structure, Patterns.hs imports Expressions.hs,
-- and Expressions.hs imports Patterns.hs. This creates a circular dependency.

-- To resolve:
-- 1. `matchExprParser` and `handlerExprParser` use `typeTerm` (from Types) and `clauseParser`/`handlerClauseParser` (from Patterns/self).
-- 2. `clauseParser` (in Patterns) uses `expr` (from this module).

-- The simplest way is to ensure `expr` is fully defined here.
-- `clauseParser` in `Patterns.hs` will then correctly use this `expr`.

-- | オペランドのパーサー (op function)
op :: Text -> Parser Text
op n = try $ lexeme (string n <* notFollowedBy (symbolChar <|> punctuationChar))

mkOperator ::
  (ExprWithMetadata -> ExprWithMetadata -> Expr) ->
  Parser
    (ExprWithMetadata -> ExprWithMetadata -> ExprWithMetadata)
mkOperator opParser = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ \left right -> ExprWithMetadata (opParser left right) (ExprMetadata sp uniqueId)

-- | 演算子の優先順位テーブル
operatorTable :: [[Operator Parser ExprWithMetadata]]
operatorTable =
  [
    [ InfixL (mkOperator (BinOp Mul) <* op "*")
    , InfixL (mkOperator (BinOp Div) <* op "/")
    ]
  ,
    [ InfixL (mkOperator (BinOp Add) <* op "+")
    , InfixL (mkOperator (BinOp Sub) <* op "-")
    ]
  ,
    [ InfixN (mkOperator (BinOp Eq) <* op "==")
    , InfixN (mkOperator (BinOp Lt) <* op "<")
    , InfixN (mkOperator (BinOp Gt) <* op ">")
    ]
  , [InfixR (mkOperator Apply <* op "<|")]
  , [InfixL (mkOperator (flip Apply) <* op "|>")]
  , [InfixR (mkOperator HandleApply <* op "<<|")]
  , [InfixL (mkOperator (flip HandleApply) <* op "|>>")]
  ]

-- | if式
ifExpr :: Parser ExprWithMetadata
ifExpr = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "if"
  cond <- term -- Changed from expr to term to avoid direct left-recursion with operatorTable
  rword "then"
  thenExpr <- term -- Changed from expr to term
  rword "else"
  elseExpr <- term -- Changed from expr to term
  return $ ExprWithMetadata (If cond thenExpr elseExpr) (ExprMetadata sp uniqueId) -- Changed from expr to term

-- | ラムダ式
lambdaExpr :: Parser ExprWithMetadata
lambdaExpr = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "let"
  patt <- patternParser -- Changed from lowerIdentifier to patternParser
  mType <- optional $ do
    void $ symbol ":"
    typeTerm -- from Types
  void $ symbol "->"
  body <- expr -- Recursive call to expr
  return $ ExprWithMetadata (Lambda patt mType body) (ExprMetadata sp uniqueId)

{- | match式のパーサー
例: branch myValue [ (Some x) -> x, (None) -> 0 ]
-}
matchExprParser :: Parser ExprWithMetadata
matchExprParser = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "branch"
  clauses <- brackets (sepBy (clauseParser expr) (symbol ",")) -- Pass expr to clauseParser
  return $ ExprWithMetadata (Match clauses) (ExprMetadata sp uniqueId)

{- | ハンドラ節のパーサー
例:
  (n |> NumBool |> k) -> n > 0 |> k
  (x) -> x |> Right
-}
handlerClauseParser :: Parser HandlerClauseWithMetadata
handlerClauseParser = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  symbol "("
  try
    ( do
        argVar <- lowerIdentifierArgument -- 小文字で始まる識別子
        symbol "|>"
        opName <- upperIdentifierTypeVariable -- 大文字で始まる識別子
        symbol "|>"
        kVar <- lowerIdentifierArgument -- 小文字で始まる識別子
        symbol ")"
        symbol "->"
        body <- expr -- Recursive call to expr
        return $ HandlerClauseWithMetadata (HandlerClause opName argVar kVar body) (HandlerClauseMetadata sp uniqueId)
    )
    <|> ( do
            retVar <- lowerIdentifierArgument -- 小文字で始まる識別子
            symbol ")"
            symbol "->"
            body <- expr -- Recursive call to expr
            return $ HandlerClauseWithMetadata (HandlerReturnClause retVar body) (HandlerClauseMetadata sp uniqueId)
        )

{- | ハンドル式のパーサー
例: handle <Trans, Throw> [ (n |> NumBool |> k) -> n > 0 |> k, (x) -> x |> Right ]
-}
handlerExprParser :: Parser ExprWithMetadata
handlerExprParser = lexeme $ do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  rword "handle"
  clauses <- brackets (sepBy handlerClauseParser (symbol ","))
  return $ ExprWithMetadata (Handle clauses) (ExprMetadata sp uniqueId)

-- | 項
term :: Parser ExprWithMetadata
term =
  choice
    [ lambdaExpr
    , matchExprParser
    , handlerExprParser
    , ifExpr
    , tupleExprParser -- Added tupleExprParser
    , parens expr -- Recursive call to expr
    , do
        uniqueId <- newUniqueId
        sp <- getSourcePos
        name <- upperIdentifierVariable <|> lowerIdentifierVariable
        return $ ExprWithMetadata (Var name) (ExprMetadata sp uniqueId)
    , do
        uniqueId <- newUniqueId
        sp <- getSourcePos
        number <- number
        return $ ExprWithMetadata (Number number) (ExprMetadata sp uniqueId)
    , do
        uniqueId <- newUniqueId
        sp <- getSourcePos
        bool <- True <$ rword "true" <|> False <$ rword "false"
        return $ ExprWithMetadata (Bool bool) (ExprMetadata sp uniqueId)
    ]

-- | タプル式のパーサー
tupleExprParser :: Parser ExprWithMetadata
tupleExprParser = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  exprs <- brackets (sepBy expr (symbol ","))
  return $ ExprWithMetadata (Tuple exprs) (ExprMetadata sp uniqueId)

-- | 式
expr :: Parser ExprWithMetadata
expr = makeExprParser term operatorTable
