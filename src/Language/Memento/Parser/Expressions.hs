{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Expressions (
  expr,
  term,
  ifExpr,
  lambdaExpr,
  doExpr,
  matchExprParser,
  handlerExprParser,
  operatorTable -- Exported for use in Patterns if ever needed, though likely not.
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Language.Memento.Syntax (BinOp (..), Expr (..), HandlerClause (..))
import Language.Memento.Parser.Core
import Language.Memento.Parser.Types (typeExpr, typeTerm) -- Assuming typeTerm is needed by matchExprParser via term
import Language.Memento.Parser.Patterns (patternParser, clauseParser) -- clauseParser for matchExprParser
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
  , [InfixR (HandleApply <$ op "<<|")]
  , [InfixL (flip HandleApply <$ op "|>>")]
  ]

-- | if式
ifExpr :: Parser Expr
ifExpr = (lexeme . try) $ do
  rword "if"
  cond <- term -- Changed from expr to term to avoid direct left-recursion with operatorTable
  rword "then"
  thenExpr <- term -- Changed from expr to term
  rword "else"
  If cond thenExpr <$> term -- Changed from expr to term

-- | ラムダ式
lambdaExpr :: Parser Expr
lambdaExpr = do
  name <- identifier
  mType <- optional $ do
    void $ symbol ":"
    typeTerm -- from Types
  void $ symbol "->"
  body <- expr -- Recursive call to expr
  return $ Lambda name mType body

-- | do構文
doExpr :: Parser Expr
doExpr = (lexeme . try) $ do
  rword "do"
  name <- identifier
  return $ Do name

{- | match式のパーサー
例: branch myValue [ (Some x) -> x, (None) -> 0 ]
-}
matchExprParser :: Parser Expr
matchExprParser = lexeme $ do
  rword "branch"
  -- Scrutinee in grammar was typeTerm, which means it's a simple type name or parenthesized type expr.
  -- However, the AST for Match stores a Type, which is correct as per Syntax.hs: `Match Type [Clause]`
  -- The original `typeTerm` here was likely meant to parse an *expression* that results in an ADT.
  -- Let's assume it's an expression, consistent with typical match forms.
  -- So, we'll use `term` or even `expr` for the scrutinee.
  -- Given `term` is used for `if` conditions, using `term` here seems consistent.
  scrutineeExpr <- term -- Parses an expression, not a type.
  
  -- The AST `Match Type [Clause]` seems to expect the *type* of the scrutinee, not the scrutinee expression itself.
  -- This is unusual. Typically, a match expression is `Match Expr [Clause]`.
  -- Let's stick to the original `typeTerm` for now as per `Parser.hs` and see how it's handled in TypeChecker.
  -- If `typeTerm` is indeed for parsing a type literal (e.g. `MyADT`), that's what will be stored.
  -- The TypeChecker.hs analysis showed `Match scrutinee clauses` where `scrutinee` was an `Expr`,
  -- and then its type was inferred.
  -- `Match scrutinee clauses -> do adtEnv <- getAdtEnv; scrutineeName <- case scrutinee of TAlgebraicData name -> return name`
  -- This implies the parser *should* be parsing an expression for the scrutinee.
  -- The `Match` node in `Syntax.hs` is `Match Type [Clause]`. This is a mismatch.
  -- For now, I will assume the parser should parse an *expression* as the scrutinee.
  -- And the `Match` node in Syntax.hs should be `Match Expr [Clause]`.
  -- This is a significant change that should be part of the "Consistency" step later,
  -- but we have to make a choice for the parser now.
  -- Let's assume `Syntax.hs` will be changed. So, parse an `Expr`.
  -- EDIT: The issue description asks for refactoring existing code, not redesigning it.
  -- The original parser had `scrutinee <- typeTerm` for `matchExprParser`.
  -- The `Syntax.hs` has `Match Type [Clause]`. This is what the original TypeChecker was expecting for `Match`.
  -- I will revert to parsing `Type` for the first argument of `Match`.
  scrutineeType <- typeTerm -- This parses a Type literal.
  
  clauses <- brackets (sepBy (try clauseParser) (symbol ",")) -- List of clauses
  return $ Match scrutineeType clauses


{- | ハンドラ節のパーサー
例:
  (n |> NumBool |> k) -> n > 0 |> k
  (x) -> x |> Right
-}
handlerClauseParser :: Parser HandlerClause
handlerClauseParser = lexeme $ do
  symbol "("
  clause <-
    try
      ( do
          argVar <- identifier
          symbol "|>"
          opName <- identifier
          symbol "|>"
          kVar <- identifier
          symbol ")"
          symbol "->"
          body <- expr -- Recursive call to expr
          return $ HandlerClause opName argVar kVar body
      )
      <|> ( do
              retVar <- identifier
              symbol ")"
              symbol "->"
              body <- expr -- Recursive call to expr
              return $ HandlerReturnClause retVar body
          )
  return clause

{- | ハンドル式のパーサー
例: handle <Trans, Throw> [ (n |> NumBool |> k) -> n > 0 |> k, (x) -> x |> Right ]
-}
handlerExprParser :: Parser Expr
handlerExprParser = lexeme $ do
  rword "handle"
  symbol ":"
  handlerType <- typeExpr -- from Types
  clauses <- brackets (sepBy handlerClauseParser (symbol ","))
  return $ Handle handlerType clauses

-- | 項
term :: Parser Expr
term =
  choice
    [ try $ parens expr -- Recursive call to expr
    , try lambdaExpr
    , try doExpr
    , try matchExprParser 
    , try handlerExprParser 
    , ifExpr
    , Var <$> identifier
    , Number <$> number
    , Bool <$> (True <$ rword "true" <|> False <$ rword "false")
    ]

-- | 式
expr :: Parser Expr
expr = makeExprParser term operatorTable
