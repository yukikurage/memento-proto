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
  brackets,
  identifier,
  lowerIdentifierTypeVariable,
  newUniqueId,
  parens,
  rword,
  symbol,
  upperIdentifier,
  upperIdentifierTypeVariable,
 )
import Language.Memento.Syntax (Effect (..), EffectMetadata (EffectMetadata), EffectWithMetadata (EffectWithMetadata), Effects, EffectsMetadata (EffectsMetadata), EffectsWithMetadata (EffectsWithMetadata), Type (..), TypeMetadata (TypeMetadata), TypeWithMetadata (TypeWithMetadata))
import Text.Megaparsec
import Text.Megaparsec.Char

-- | エフェクトのパーサー
effectParser :: Parser EffectWithMetadata
effectParser = do
  name <- upperIdentifierTypeVariable <|> lowerIdentifierTypeVariable -- 識別子
  uniqueId <- newUniqueId
  sp <- getSourcePos
  return $ EffectWithMetadata (Effect name) (EffectMetadata sp uniqueId)

effectsParser :: Parser EffectsWithMetadata
effectsParser = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  symbol "<"
  effects <- Set.fromList <$> sepBy effectParser (symbol ",")
  symbol ">"
  return $ EffectsWithMetadata effects (EffectsMetadata sp uniqueId)

-- | 型の項 (非関数型、括弧で囲まれた型)
typeTerm :: Parser TypeWithMetadata
typeTerm =
  choice
    [ tupleTypeParser -- Added tupleTypeParser
    , parens typeExpr -- Recursive call to the new typeExpr
    , do
        rword "number"
        uniqueId <- newUniqueId
        sp <- getSourcePos
        return $ TypeWithMetadata TNumber (TypeMetadata sp uniqueId)
    , do
        rword "bool"
        uniqueId <- newUniqueId
        sp <- getSourcePos
        return $ TypeWithMetadata TBool (TypeMetadata sp uniqueId)
    , do
        name <- upperIdentifierTypeVariable
        uniqueId <- newUniqueId
        sp <- getSourcePos
        return $ TypeWithMetadata (TAlgebraicData name) (TypeMetadata sp uniqueId)
    ]

-- | タプル型のパーサー
tupleTypeParser :: Parser TypeWithMetadata
tupleTypeParser = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  types <- brackets (sepBy typeExpr (symbol ","))
  return $ TypeWithMetadata (TTuple types) (TypeMetadata sp uniqueId)

-- | 関数型の右辺のパーサー (-> Type [with Effects])
functionTypeSuffixParser :: TypeWithMetadata -> Parser TypeWithMetadata
functionTypeSuffixParser argType = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  symbol "->"
  retType <- typeExpr
  effects <- option (EffectsWithMetadata Set.empty (EffectsMetadata sp uniqueId)) (rword "with" *> effectsParser)
  return $ TypeWithMetadata (TFunction argType (retType, effects)) (TypeMetadata sp uniqueId)

handlerTypeSuffixParser :: TypeWithMetadata -> EffectsWithMetadata -> Parser TypeWithMetadata
handlerTypeSuffixParser argType argEffects = do
  uniqueId <- newUniqueId
  sp <- getSourcePos
  symbol "=>"
  retType <- typeExpr
  effects <- option (EffectsWithMetadata Set.empty (EffectsMetadata sp uniqueId)) (rword "with" *> effectsParser)
  return $ TypeWithMetadata (THandler (argType, argEffects) (retType, effects)) (TypeMetadata sp uniqueId)

-- | 型のパーサー (関数型を含む)
typeExpr :: Parser TypeWithMetadata
typeExpr =
  try
    ( do
        uniqueId <- newUniqueId
        sp <- getSourcePos
        argType <- typeTerm
        argEffects <- option (EffectsWithMetadata Set.empty (EffectsMetadata sp uniqueId)) (rword "with" *> effectsParser)
        handlerTypeSuffixParser argType argEffects
    )
    <|> try
      ( do
          uniqueId <- newUniqueId
          sp <- getSourcePos
          argType <- typeTerm
          functionTypeSuffixParser argType
      )
    <|> typeTerm

-- | 型注釈のパーサー
typeAnnotation :: Parser TypeWithMetadata
typeAnnotation = typeExpr -- Uses the new typeExpr
