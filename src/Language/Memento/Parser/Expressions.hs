{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Language.Memento.Parser.Expressions where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.BinOp (BinOp (Add, Div, Eq, Gt, Lt, Mul, Sub))
import Language.Memento.Syntax.Expr (Expr (EApply, EBinOp, EBlock, EIf, ELambda, ELiteral, EMatch, EVar), Let (Let))
import Language.Memento.Syntax.Tag (KBinOp, KExpr, KLet)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), choice, many, sepBy, sepEndBy)
import Text.Megaparsec.Char (punctuationChar, symbolChar)

{-

Expr f KExpr -> Expr f KExpr -> Expr f KExpr もしくは
f KExpr -> f KExpr -> f KExpr を作る？
Expr f KExpr -> f KExpr は fix で可能 ← でもこの段階でパースが必要 (例えば Metadata を付与する場合は fix 時に SourcePos を付与する必要があるため)

~~自力で書くかぁ~~ → propagateFix を使う

-}

mkOp ::
  forall h f m s c.
  ( Monad m
  , Injective BinOp h
  , PClass.FixParser h f m
  ) =>
  m c ->
  BinOp f KBinOp ->
  m (f KBinOp)
mkOp opTextParser expectedOp = PClass.parseFix @h $ do
  opTextParser
  return $ hInject expectedOp

mkBinOperator ::
  forall h f m.
  ( Monad m
  , Injective Expr h
  , PClass.PropagateFix h f
  ) =>
  m (f KBinOp) -> -- Operator パーサ
  m (f KExpr -> f KExpr -> f KExpr)
mkBinOperator opParser = do
  fixedOp <- opParser
  return $ \x y -> PClass.propagateFix @h x y $ hInject $ EBinOp fixedOp x y

-- | 演算子の優先順位テーブル
operatorTable ::
  forall h f m s.
  ( Monad m
  , PClass.CoreParser m
  , Injective Expr h
  , Injective BinOp h
  , PClass.PropagateFix h f
  , PClass.FixParser h f m
  ) =>
  [[Operator m (f KExpr)]]
operatorTable =
  [
    [ InfixL (mkBinOperator @h (mkOp @h (PClass.parseSymbol "*") Mul))
    , InfixL (mkBinOperator @h (mkOp @h (PClass.parseSymbol "/") Div))
    ]
  ,
    [ InfixL (mkBinOperator @h (mkOp @h (PClass.parseSymbol "+") Add))
    , InfixL (mkBinOperator @h (mkOp @h (PClass.parseSymbol "-") Sub))
    ]
  ,
    [ InfixN (mkBinOperator @h (mkOp @h (PClass.parseSymbol "==") Eq))
    , InfixN (mkBinOperator @h (mkOp @h (PClass.parseSymbol "<") Lt))
    , InfixN (mkBinOperator @h (mkOp @h (PClass.parseSymbol ">") Gt))
    ]
  ]

-- | if式
parseIfExpr ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.FixParser h f m
  , PClass.MTypeParser f m
  , PClass.PatternParser f m
  , Injective Let h
  , PClass.PropagateFix h f
  , Injective BinOp h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  ) =>
  m (f KExpr)
parseIfExpr = PClass.parseFix @h $ do
  PClass.parseReservedWord "if"
  cond <- PClass.parseParens (parseExpr @h)
  thenExpr <- parseBlock @h
  PClass.parseReservedWord "else"
  elseExpr <- parseBlock @h
  return $ hInject $ EIf cond thenExpr elseExpr

{- | ラムダ式
| (x : arg) => body
| (Constructor(x) : arg) => body
| (Tuple(x, y) : Tuple) => ...
-}
parseLambdaExpr ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , PClass.PropagateFix h f
  , Injective BinOp h
  , Injective Let h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  ) =>
  m (f KExpr)
parseLambdaExpr = PClass.parseFix @h $ do
  args <-
    PClass.parseParens $
      sepEndBy
        ( do
            patt <- PClass.parsePattern
            PClass.parseSymbol ":"
            typ <- PClass.parseMType

            return (patt, typ)
        )
        (PClass.parseSymbol ",")
  PClass.parseSymbol "=>"
  body <- parseExpr @h
  return $ hInject $ ELambda args body

{- | match式のパーサー λ式の列のような感じ
例: switch(arg) [ (Some(x) : argType) => x, (None : argType) => 0 ]
-}
parseMatchExpr ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , PClass.PropagateFix h f
  , PClass.VariableParser f m
  , Injective BinOp h
  , Injective Let h
  , PClass.LiteralParser f m
  ) =>
  m (f KExpr)
parseMatchExpr = PClass.parseFix @h $ do
  PClass.parseReservedWord "switch"
  args <- PClass.parseParens $ sepEndBy (parseExpr @h) (PClass.parseSymbol ",")
  clauses <-
    PClass.parseBrackets $
      sepEndBy
        ( do
            args <-
              PClass.parseParens $
                sepEndBy
                  ( do
                      patt <- PClass.parsePattern
                      PClass.parseSymbol ":"
                      typ <- PClass.parseMType
                      return (patt, typ)
                  )
                  (PClass.parseSymbol ",")
            PClass.parseSymbol "=>"
            body <- parseExpr @h
            return (args, body)
        )
        (PClass.parseSymbol ",")
  return $ hInject $ EMatch args clauses

parseLet ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Let h
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , PClass.PropagateFix h f
  , Injective BinOp h
  , Injective Let h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  , Injective Expr h
  ) =>
  m (f KLet)
parseLet = PClass.parseFix @h $ do
  PClass.parseReservedWord "let"
  pat <- PClass.parsePattern
  PClass.parseSymbol ":"
  typ <- PClass.parseMType
  PClass.parseSymbol "="
  body <- parseExpr @h
  PClass.parseSymbol ";"
  return $ hInject $ Let pat typ body

parseBlock ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , Injective Let h
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , PClass.PropagateFix h f
  , Injective BinOp h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  ) =>
  m (f KExpr)
parseBlock = PClass.parseFix @h $ do
  PClass.parseBraces $ do
    lets <- many (parseLet @h)
    body <- parseExpr @h
    return $ hInject $ EBlock lets body

parseLiteral ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.FixParser h f m
  , PClass.LiteralParser f m
  ) =>
  m (f KExpr)
parseLiteral = PClass.parseFix @h $ do
  lit <- PClass.parseLiteral
  return $ hInject $ ELiteral lit

parseVar ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.FixParser h f m
  , PClass.VariableParser f m
  ) =>
  m (f KExpr)
parseVar = PClass.parseFix @h $ do
  var <- PClass.parseVariable
  return $ hInject $ EVar var

{- | 関数適用
例: f(x, y) や f(x)(y) のような形式
-}
parseApp ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , Injective Let h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  , PClass.PropagateFix h f
  , Injective BinOp h
  , Injective Let h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  ) =>
  m (f KExpr)
parseApp = PClass.parseFix @h $ do
  func <- parseTerm @h
  args <- PClass.parseParens $ sepEndBy (parseExpr @h) (PClass.parseSymbol ",")
  return $ hInject $ EApply func args

{- | 項
| 括弧が無くても独立したものとして判断できる (そうな) もの
-}
parseTerm ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , Injective Let h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  , PClass.PropagateFix h f
  , Injective BinOp h
  ) =>
  m (f KExpr)
parseTerm =
  choice
    [ parseApp @h -- Function application
    , PClass.parseParens (parseExpr @h) -- Recursive call to expr
    , parseBlock @h -- Block
    , parseLiteral @h -- Literal
    , parseVar @h -- Var
    ]

-- | バイナリ演算子とタームで構成される式
parseTermAndBinOp ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.PropagateFix h f
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , Injective BinOp h
  , Injective Let h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  ) =>
  m (f KExpr)
parseTermAndBinOp = makeExprParser (parseTerm @h) (operatorTable @h)

-- | バイナリ演算子とタームで構成される式、若しくは関数適用、switch, lambda 式
parseExpr ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , Injective Expr h
  , PClass.PropagateFix h f
  , PClass.FixParser h f m
  , PClass.PatternParser f m
  , PClass.MTypeParser f m
  , Injective BinOp h
  , Injective Let h
  , PClass.LiteralParser f m
  , PClass.VariableParser f m
  ) =>
  m (f KExpr)
parseExpr =
  choice
    [ parseIfExpr @h
    , parseMatchExpr @h
    , parseLambdaExpr @h
    , parseTermAndBinOp @h
    ]

{-

-- | タプル式のパーサー
tupleExprParser :: Parser ExprWithMetadata
tupleExprParser = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  exprs <- brackets (sepBy expr (symbol ","))
  return $ ExprWithMetadata (Tuple exprs) (ExprMetadata sp uniqueId)

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

-}