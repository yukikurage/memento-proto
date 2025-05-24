{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Types (
  effectsParser,
  typeTerm,
  functionTypeSuffixParser,
  handlerTypeSuffixParser,
  typeExpr,
  typeAnnotation,
) where

import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Parser.Core (
  Parser,
  parens,
  rword,
  symbol,
  upperIdentifier, -- 大文字で始まる識別子を使用
 )
import Language.Memento.Syntax (Effect (..), Effects, Type (..))
import Text.Megaparsec
import Text.Megaparsec.Char

-- | エフェクトのパーサー
effectParser :: Parser Effect
effectParser = Effect <$> upperIdentifier -- 大文字で始まる識別子

effectsParser :: Parser Effects
effectsParser =
  symbol "<"
    *> (Set.fromList <$> sepBy effectParser (symbol ","))
    <* symbol ">"
      <|> pure Set.empty -- Allows for `with` followed by nothing, implying <>

-- | 型の項 (非関数型、括弧で囲まれた型)
typeTerm :: Parser Type
typeTerm =
  choice
    [ try $ parens typeExpr -- Recursive call to the new typeExpr
    , TNumber <$ rword "number"
    , TBool <$ rword "bool"
    , TAlgebraicData <$> upperIdentifier -- 大文字で始まる識別子
    ]

-- | 関数型の右辺のパーサー (-> Type [with Effects])
functionTypeSuffixParser :: Type -> Parser Type
functionTypeSuffixParser argType = do
  symbol "->"
  retType <- typeExpr
  effects <- option Set.empty (rword "with" *> effectsParser)
  return $ TFunction argType (retType, effects)

handlerTypeSuffixParser :: Type -> Effects -> Parser Type
handlerTypeSuffixParser argType argEffects = do
  symbol "=>"
  retType <- typeExpr
  effects <- option Set.empty (rword "with" *> effectsParser)
  return $ THandler (argType, argEffects) (retType, effects)

-- | 型のパーサー (関数型を含む)
typeExpr :: Parser Type
typeExpr =
  try
    ( do
        argType <- typeTerm
        argEffects <- option Set.empty (rword "with" *> effectsParser)
        handlerTypeSuffixParser argType argEffects
    )
    <|> try
      ( do
          argType <- typeTerm
          functionTypeSuffixParser argType
      )
    <|> typeTerm

-- | 型注釈のパーサー
typeAnnotation :: Parser Type
typeAnnotation = typeExpr -- Uses the new typeExpr
