{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.Data.HProduct where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))

data (h1 :*: h2) (f :: Type -> Type) a
  = (h1 f a) :*: (h2 f a)
