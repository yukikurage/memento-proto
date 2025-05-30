{-# LANGUAGE GADTs #-}

module Language.Memento.Syntax.Token where

import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Syntax.Kind (KToken, KType)

data Token f a where
  NumberToken :: Double -> Token f (KToken Double)
  BoolToken :: Bool -> Token f (KToken Bool)
  TextToken :: Text -> Token f (KToken Text)