{-# LANGUAGE GADTs #-}

module Language.Memento.Syntax.Definition where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Syntax.Kind (KDefinition, KExpr, KToken, KType)

data Definition f a where
  ValDef ::
    f (KToken Text) ->
    f KType ->
    f KExpr ->
    Definition f KDefinition
  -- 今回の言語では data は単一のコンストラクタしか持たない
  -- Union を使って複数コンストラクタをまとめる
  -- data Just : Num -> Just
  -- data Nothing : Nothing
  -- type Maybe = Just | Nothing
  -- justVal : Maybe := Just(10)
  -- nothingVal : Maybe := Nothing
  DataDef ::
    f (KToken Text) -> -- Data type & constructor name
    f KType -> -- Data constructor type
    Definition f KDefinition