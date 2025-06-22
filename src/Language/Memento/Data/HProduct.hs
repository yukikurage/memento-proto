{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.Data.HProduct where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))

data (h1 :*: h2) (f :: Type -> Type) a
  = (h1 f a) :*: (h2 f a)

deriving instance (Show (h1 f a), Show (h2 f a)) => Show ((h1 :*: h2) f a)
deriving instance (Eq (h1 f a), Eq (h2 f a)) => Eq ((h1 :*: h2) f a)
deriving instance (Ord (h1 f a), Ord (h2 f a)) => Ord ((h1 :*: h2) f a)

data HUnit (f :: Type -> Type) a = HUnit

deriving instance Show (HUnit f a)
deriving instance Eq (HUnit f a)
deriving instance Ord (HUnit f a)

instance HFunctor HUnit where
  hmap f HUnit = HUnit

instance (HFunctor h1, HFunctor h2) => HFunctor (h1 :*: h2) where
  hmap f (h1 :*: h2) = hmap f h1 :*: hmap f h2