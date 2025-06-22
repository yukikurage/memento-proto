{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.Data.HFunctor where

import Language.Memento.Data.NaturalTransformation

class HFunctor h where
  hmap :: (f ~> g) -> h f ~> h g