{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Memento.Syntax (BinOp (..), Definition (..), Effect (..), Effects, Expr (..), Match(..), Program (..), Type (..), TAlgebraicData(..), ConstructorDef(..), DataDef(..), Pattern(..), PConstructor, PVar, PWildcard, Clause(..)) -- Added Match
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
reservedWords = ["if", "then", "else", "true", "false", "number", "bool", "do", "val", "with", "Throw", "ZeroDiv", "data", "branch"] -- Added "data", "branch"

-- | 識別子
identifier :: Parser Text
identifier = (lexeme . try) $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name

-- | コンストラクタ定義のパーサー
-- 例: Cons : number -> bool
constructorDefinitionParser :: Parser ConstructorDef
constructorDefinitionParser = lexeme $ do
  name <- identifier
  symbol ":"
  -- Parse types separated by "->", but these are argument types.
  -- The typeExpr already handles function types TFunction arg ret eff.
  -- We need to parse a sequence of types that are arguments to the constructor.
  -- A constructor like C a b is `C : TypeA -> TypeB -> YourADT`.
  -- Syntax.hs's ConstructorDef only stores [Type] for args.
  -- So if we parse `number -> bool`, it means [TNumber, TBool]
  argTypes <- sepBy typeExpr (symbol "->") -- This might be too greedy if typeExpr itself is a function type.
                                        -- Let's refine this to parse individual, non-function types as arguments.
                                        -- For `Cons : (number -> bool) -> string`, argTypes should be [TFunction TNumber TBool Set.empty, TString] (assuming TString)
                                        -- For `Cons : number -> bool -> string`, argTypes should be [TNumber, TBool, TString]

  -- We need to parse a list of argument types. `typeExpr` can parse complex types including function types.
  -- The issue is how to interpret `T1 -> T2 -> T3` in the context of a constructor signature.
  -- Does `Cons : T1 -> T2 -> T3` mean `ConstructorDef "Cons" [T1, T2, T3]`?
  -- Or does it mean `ConstructorDef "Cons" [TFunction T1 T2 emptyEffects, T3]`?
  -- Given `ConstructorDef Text [Type]`, the former interpretation is more direct.
  -- Let's assume for now that `->` in a constructor signature separates distinct arguments.
  -- If an argument itself is a function type, it should be parenthesized: `Cons : (number -> bool) -> number`.
  
  -- To correctly parse `T1 -> T2 -> T3` as `[T1, T2, T3]`, we should use typeTerm' (or a similar parser for single types)
  -- and `sepBy1` with `symbol "->"`.
  -- However, an argument *can* be a function type: `Cons : (number -> bool) -> number`.
  -- So, `typeExpr` is correct for each argument, and `sepBy` for the list of arguments.
  
  -- Let's reconsider the structure `Cons : type1 -> type2 -> type3`.
  -- This is usually shorthand for `Cons : type1 -> (type2 -> type3)`.
  -- But in our `ConstructorDef Text [Type]`, we want `[type1, type2, type3]`.
  -- This means our parser for the list of types within a constructor def needs to be careful.
  -- `typeExpr` is designed to parse a whole type, which can be `T1 -> T2 -> T3` (a single TFunction).
  -- We need to parse a list of *argument* types.
  
  -- Let's try parsing with `typeExpr` and see. If we have `Cons : number -> bool`,
  -- `typeExpr` will parse `TFunction TNumber TBool Set.empty`. This is not what we want for `[Type]`.
  -- We want `[TNumber, TBool]`.
  
  -- The `sepBy typeTerm' (symbol "->")` approach seems more plausible for flattening.
  -- But `typeTerm'` doesn't parse function types.
  -- So, for `Cons : (number -> bool) -> number`, `typeTerm'` would fail on `(number -> bool)`.
  
  -- A constructor definition `C : T1 -> T2 -> ... -> Tn -> T_adt` has argument types `T1, T2, ..., Tn`.
  -- The problem statement implies `ConstructorName : type1 -> type2 -> ...`
  -- This means `type1, type2, ...` are the argument types.
  
  -- The most robust way is to parse a full type expression, then deconstruct it if it's a series of right-nested functions.
  -- Or, more simply, `sepBy1 typeAtom (symbol "->")` where `typeAtom` is `typeTerm'` or `parens typeExpr`.
  
  let typeAtom = choice [ try (parens typeExpr) -- for function types as arguments e.g. (A -> B)
                        , typeTerm'           -- for simple types or ADT names
                        ]
  args <- sepBy1 typeAtom (try (lexeme (string "->"))) -- try is important here for backtracking
  
  -- The above `sepBy1 typeAtom (symbol "->")` will parse `T1 -> T2 -> T3` into `[T1, T2, T3]`
  -- if T1, T2, T3 are parsed by `typeAtom`.
  -- If we have `Constructor : (Number -> Bool) -> Number`
  -- typeAtom will parse `(Number -> Bool)` as one TFunction type, and `Number` as another.
  -- This seems correct. `sepBy1` will give `[TFunction Number Bool, Number]`.

  -- The prompt is `ConstructorName : type1 -> type2 -> ...`
  -- This implies `type1`, `type2` are the argument types.
  -- Let's assume `typeExpr` for each type, and they are separated by `symbol "->"` for now.
  -- This is ambiguous if a constructor takes a single function argument vs multiple arguments.
  -- e.g. `MyCons : (A -> B) -> C` (two args) vs `MyCons : A -> B -> C` (three args).
  -- The syntax `ConstructorDef Text [Type]` suggests the latter should be `[A, B, C]`.

  -- Let's define an argument type parser that is `typeTerm'` or a parenthesized `typeExpr`.
  let argTypeParser = typeTerm' <|> parens typeExpr
  argTypesList <- sepBy argTypeParser (symbol "->")
  -- This still has the issue: `number -> bool` parsed by `sepBy argTypeParser` would give `[TNumber, TBool]`.
  -- If `argTypeParser` was `typeExpr`, `number -> bool` would be parsed as a single `TFunction TNumber TBool`, so `[TFunction TNumber TBool]`.
  -- This is a classic parsing ambiguity for types.
  -- Given `ConstructorDef Text [Type]`, we want the list of types.
  -- So, `Cons : T1 -> T2 -> T3` means arguments `T1, T2, T3`.
  -- `Cons : (T1 -> T2) -> T3` means arguments `(T1 -> T2), T3`.
  
  -- The common way to parse `t1 -> t2 -> t3` as a list `[t1, t2, t3]` is to parse `t1` then `optional (-> t2 -> t3)`.
  -- Or, `typeAtom `sepBy1` (symbol "->")`.
  -- `typeAtom` here must be a type that is NOT a TFunction itself, unless parenthesized.
  -- So, `typeTerm'` (which includes `TAlgebraicData` and `parens typeExpr`) is a good candidate for `typeAtom`.
  
  -- `typeTerm'` is: `parens typeExpr | TNumber | TBool | TAlgebraicData <$> identifier`
  -- This is suitable for `typeAtom`.
  types <- sepBy1 typeTerm' (symbol "->")
  
  return $ ConstructorDef name types

-- | データ定義のパーサー
-- 例: data MyList [ Nil, Cons : number -> MyList ];
dataDefinitionParser :: Parser Definition
dataDefinitionParser = lexeme $ do
  rword "data"
  name <- identifier
  constructors <- brackets (sepBy constructorDefinitionParser (symbol ","))
  symbol ";"
  return $ DataDef name constructors

-- | パターンパーサー
patternParser :: Parser Pattern
patternParser = lexeme $ choice
  [ -- コンストラクタパターン: (ConstructorName var1 var2 ...)
    try $ parens $ do
        constructorName <- identifier
        varNames <- many identifier -- Zero or more variable names
        return $ PConstructor constructorName varNames
  -- ワイルドカードパターン: _
  , PWildcard <$ symbol "_"
  -- 変数パターン: varName (must come after wildcard and constructor to avoid ambiguity)
  , PVar <$> identifier
  ]

-- | match節のパーサー
-- 例: ( (Cons x xs) ) -> x
clauseParser :: Parser Clause
clauseParser = lexeme $ do
  pat <- parens patternParser -- Pattern enclosed in parentheses
  symbol "->"
  ex <- expr
  return $ Clause pat ex

-- | match式のパーサー
-- 例: branch myValue [ ( (Some x) ) -> x, ( (None) ) -> 0 ]
matchExprParser :: Parser Expr
matchExprParser = lexeme $ do
  rword "branch"
  scrutinee <- expr -- The expression to match against
  clauses <- brackets (sepBy clauseParser (symbol ",")) -- List of clauses
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
definitionParser = lexeme $ choice
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
