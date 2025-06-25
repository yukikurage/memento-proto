{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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

-- | Extract a component from a product type.
class Extractive h1 h2 where
  hExtract :: forall (f :: Type -> Type) a. h2 f a -> h1 f a

instance {-# OVERLAPPING #-} Extractive h1 (h2 :*: h1) where
  hExtract :: (h2 :*: h1) f a -> h1 f a
  hExtract (h2' :*: h1') = h1'

instance {-# OVERLAPPABLE #-} (Extractive h1 h2) => Extractive h1 (h2 :*: h3) where
  hExtract :: (h2 :*: h3) f a -> h1 f a
  hExtract (h2' :*: h3') = hExtract h2'
