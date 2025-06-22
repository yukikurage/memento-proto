{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.Parser.Definitions where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Language.Memento.Data.HCoproduct (Injective (hInject))
import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax (extractSyntax, unMType)
import Language.Memento.Syntax.Definition (ConstructorDef (..), Definition (..))
import qualified Language.Memento.Syntax.MType as SMType
import Language.Memento.Syntax.Tag (KDefinition, KType, KVariable, KExpr, KPattern)
import qualified Language.Memento.Syntax.Expr as SExpr
import qualified Language.Memento.Syntax.Pattern as SPattern
import Text.Megaparsec (MonadParsec, choice, sepBy, try, (<?>))
import Text.Megaparsec.Char (string)

-- | Parse optional type parameter list like <T, U, V>
parseTypeParameters ::
  forall f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.VariableParser f m
  ) =>
  m [f KVariable]
parseTypeParameters = do
  try (PClass.parseAngleBrackets $ sepBy PClass.parseVariable (PClass.parseSymbol ","))
    <|> return [] -- Empty list if no type parameters

parseTypeAssignments ::
  forall f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.VariableParser f m
  , PClass.MTypeParser f m
  ) =>
  m [f KType]
parseTypeAssignments = do
  try (PClass.parseAngleBrackets $ sepBy PClass.parseMType (PClass.parseSymbol ","))
    <|> return [] -- Empty list if no assignments

-- Parse individual constructor: ConstructorName<T, U> : (args...) => ReturnType
-- or new syntax: ConstructorName<T, U>(args...) : ReturnType
parseConstructorDef ::
  forall f m s h.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , Injective SMType.MType h
  , PClass.FixParser h f m
  ) =>
  m (ConstructorDef f)
parseConstructorDef = do
  constructorName <- PClass.parseVariable
  ctorTypeParams <- parseTypeParameters
  -- Try function syntax first: Cons<T>(x : T, xs : List<T>) : List<T>
  constructorType <- try parseFunctionSyntax <|> parseNormalSyntax
  return $ ConstructorDef constructorName ctorTypeParams constructorType
  where
    -- Normal syntax: Cons<T> : (x : T, xs : List<T>) => List<T>
    parseNormalSyntax = do
      PClass.parseSymbol ":"
      PClass.parseMType
    
    -- Function syntax: Cons<T>(x : T, xs : List<T>) : List<T>
    parseFunctionSyntax = do
      -- Parse parameter list
      params <- PClass.parseParens $ 
        sepBy (do
          paramName <- PClass.parseVariable
          PClass.parseSymbol ":"
          paramType <- PClass.parseMType
          return (paramName, paramType)
        ) (PClass.parseSymbol ",")
      
      -- Parse return type
      PClass.parseSymbol ":"
      returnType <- PClass.parseMType
      
      -- Build function type from params and return type
      buildFunctionType @h params returnType

-- Parse new multi-constructor data definition
-- data Maybe [Some<T> : (x : T) => Maybe<T>, None<T> : () => Maybe<T>];
-- or new syntax: data Maybe [Some<T>(x : T) : Maybe<T>, None<T>() : Maybe<T>];
parseDataDefinition ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , Injective Definition h
  , Injective SMType.MType h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseDataDefinition = PClass.parseFix @h $ do
  dataName <- PClass.parseVariable
  constructors <- PClass.parseBrackets $ sepBy (parseConstructorDef @f @m @s @h) (PClass.parseSymbol ",")
  _ <- PClass.parseSymbol ";"
  return $ hInject $ DataDef dataName constructors

parseValDefinition ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , PClass.ExpressionParser f m
  , Injective Definition h
  , Injective SMType.MType h
  , Injective SExpr.Expr h
  , Injective SPattern.Pattern h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseValDefinition = PClass.parseFix @h $ do
  name <- PClass.parseVariable
  typeParams <- parseTypeParameters
  -- Try function syntax first: id<T>(x : T) : T := body
  (typ, body) <- try parseFunctionSyntax <|> parseNormalSyntax
  _ <- PClass.parseSymbol ";"
  return $ hInject $ ValDef name typeParams typ body
  where
    -- Normal syntax: val id<T> : (x : T) => T := body
    parseNormalSyntax = do
      PClass.parseSymbol ":"
      typ <- PClass.parseMType
      PClass.parseSymbol ":="
      body <- PClass.parseExpr
      return (typ, body)
    
    -- Function syntax: val id<T>(x : T) : T := body
    parseFunctionSyntax = do
      -- Parse parameter list
      params <- PClass.parseParens $ 
        sepBy (do
          paramName <- PClass.parseVariable
          PClass.parseSymbol ":"
          paramType <- PClass.parseMType
          return (paramName, paramType)
        ) (PClass.parseSymbol ",")
      
      -- Parse return type
      PClass.parseSymbol ":"
      returnType <- PClass.parseMType
      
      -- Build function type from params and return type
      functionType <- buildFunctionType @h params returnType
      
      -- Parse body
      PClass.parseSymbol ":="
      bodyExpr <- PClass.parseExpr
      
      -- Wrap body in lambda expressions for each parameter
      wrappedBody <- wrapInLambdas @h params bodyExpr
      
      return (functionType, wrappedBody)

parseTypeDef ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , Injective Definition h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseTypeDef = PClass.parseFix @h $ do
  name <- PClass.parseVariable
  typeParams <- parseTypeParameters
  PClass.parseSymbol ":="
  typ <- PClass.parseMType
  _ <- PClass.parseSymbol ";"
  return $ hInject $ TypeDef name typeParams typ

parseDefinition ::
  forall h f m s.
  ( MonadParsec s Text m
  , PClass.CoreParser m
  , PClass.MTypeParser f m
  , PClass.VariableParser f m
  , PClass.ExpressionParser f m
  , Injective Definition h
  , Injective SMType.MType h
  , Injective SExpr.Expr h
  , Injective SPattern.Pattern h
  , PClass.FixParser h f m
  ) =>
  m (f KDefinition)
parseDefinition =
  choice
    [ PClass.parseReservedWord "val" *> parseValDefinition @h <?> "val definition"
    , PClass.parseReservedWord "data" *> parseDataDefinition @h <?> "data definition"
    , PClass.parseReservedWord "type" *> parseTypeDef @h <?> "type definition"
    ]

-- {- | エフェクトオペレータ定義のパーサー
-- 例: NumBool : number -> bool
-- -}
-- operatorDefParser :: Parser OperatorDefWithMetadata
-- operatorDefParser = lexeme $ do
--   name <- upperIdentifierArgument -- 大文字で始まる識初
--   symbol ":"
--   typ <- typeExpr -- from Types
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   return $ OperatorDefWithMetadata (OperatorDef name typ) (OperatorDefMetadata sp uniqueId)

-- {- | データ定義のパーサー
-- 例: data MyList [ Nil, Cons : number -> MyList ];
-- -}
-- dataDefinitionParser :: Parser DefinitionWithMetadata
-- dataDefinitionParser = lexeme $ do
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   rword "data"
--   name <- upperIdentifierTypeArgument -- 大文字で始まる識別子
--   constructors <- brackets (sepEndBy constructorDefinitionParser (symbol ","))
--   symbol ";"
--   return $ DefinitionWithMetadata (DataDef name constructors) (DefinitionMetadata sp uniqueId)

-- {- | エフェクト定義のパーサー
-- 例: effect Trans [ NumBool : number -> bool, BoolNum : bool -> number ];
-- -}
-- effectDefinitionParser :: Parser DefinitionWithMetadata
-- effectDefinitionParser = lexeme $ do
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   rword "effect"
--   name <- upperIdentifierTypeArgument -- 大文字で始まる識別子
--   operators <- brackets (sepEndBy operatorDefParser (symbol ","))
--   symbol ";"
--   return $ DefinitionWithMetadata (EffectDef name operators) (DefinitionMetadata sp uniqueId)

-- valDefinitionParser :: Parser ExprWithMetadata -> Parser DefinitionWithMetadata
-- valDefinitionParser exprParser = lexeme $ do
--   uniqueId <- newUniqueId
--   sp <- getSourcePos
--   rword "val"
--   name <- lowerIdentifierArgument -- 小文字で始まる識別子
--   symbol ":"
--   typ <- typeAnnotation -- from Types
--   symbol ":="
--   body <- exprParser -- from Expressions
--   symbol ";" -- Ensure semicolon termination
--   return $ DefinitionWithMetadata (ValDef name typ body) (DefinitionMetadata sp uniqueId)

-- -- | 値定義のパーサー
-- definitionParser :: Parser ExprWithMetadata -> Parser DefinitionWithMetadata
-- definitionParser exprParser =
--   lexeme $
--     choice
--       [ valDefinitionParser exprParser
--       , dataDefinitionParser
--       , effectDefinitionParser
--       ]

-- Helper functions for function syntax sugar

-- Build a function type from parameters and return type
buildFunctionType ::
  forall h f m s.
  ( Injective SMType.MType h
  , PClass.FixParser h f m
  , MonadParsec s Text m
  ) =>
  [(f KVariable, f KType)] -> f KType -> m (f KType)
buildFunctionType params returnType = 
  PClass.parseFix @h $ return $ hInject $ SMType.TFunction params returnType

-- Wrap an expression in lambda expressions for each parameter
wrapInLambdas ::
  forall h f m s.
  ( Injective SExpr.Expr h
  , Injective SPattern.Pattern h
  , PClass.FixParser h f m
  , MonadParsec s Text m
  ) =>
  [(f KVariable, f KType)] -> f KExpr -> m (f KExpr)
wrapInLambdas [] body = return body
wrapInLambdas params body = do
  -- Convert parameters to pattern/type pairs
  patternParams <- mapM (\(var, typ) -> do
      pat <- PClass.parseFix @h $ return $ hInject $ SPattern.PVar var
      return (pat, Just typ)
    ) params
  
  -- Create lambda expression with all parameters
  PClass.parseFix @h $ return $ hInject $ SExpr.ELambda patternParams body
