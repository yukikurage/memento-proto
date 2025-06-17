{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Data.HCoproduct where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))

data (h1 :+: h2) (f :: Type -> Type) a
  = HInjL (h1 f a)
  | HInjR (h2 f a)

deriving instance (Show (h1 f a), Show (h2 f a)) => Show ((h1 :+: h2) f a)
deriving instance (Eq (h1 f a), Eq (h2 f a)) => Eq ((h1 :+: h2) f a)
deriving instance (Ord (h1 f a), Ord (h2 f a)) => Ord ((h1 :+: h2) f a)

data HVoid (f :: Type -> Type) a 

deriving instance Show (HVoid f a)
deriving instance Eq (HVoid f a)
deriving instance Ord (HVoid f a)

class Injective h1 h2 where
  hInject :: forall (f :: Type -> Type) a. h1 f a -> h2 f a
  hProject :: forall (f :: Type -> Type) a. h2 f a -> Maybe (h1 f a)

instance {-# OVERLAPPING #-} Injective h1 (h2 :+: h1) where
  hInject :: h1 f a -> (h2 :+: h1) f a
  hInject = HInjR
  hProject :: (h2 :+: h1) f a -> Maybe (h1 f a)
  hProject (HInjL h) = Nothing
  hProject (HInjR h) = Just h

instance {-# OVERLAPPABLE #-} (Injective h1 h2) => Injective h1 (h2 :+: h3) where
  hInject :: h1 f a -> (h2 :+: h3) f a
  hInject = HInjL . hInject
  hProject :: (h2 :+: h3) f a -> Maybe (h1 f a)
  hProject (HInjL h) = hProject h
  hProject (HInjR h) = Nothing

instance HFunctor HVoid where
  hmap f v = case v of {}

instance (HFunctor h1, HFunctor h2) => HFunctor (h1 :+: h2) where
  hmap f (HInjL h) = HInjL (hmap f h)
  hmap f (HInjR h) = HInjR (hmap f h)