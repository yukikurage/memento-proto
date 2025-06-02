{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Memento.Data.HCoproduct where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))

data (h1 :+: h2) (f :: Type -> Type) a
  = HInjL (h1 f a)
  | HInjR (h2 f a)

data HVoid (f :: Type -> Type) a

class Injective h1 h2 where
  hInject :: forall (f :: Type -> Type) a. h1 f a -> h2 f a
  hProject :: forall (f :: Type -> Type) a. h2 f a -> Maybe (h1 f a)

instance {-# OVERLAPPING #-} Injective h1 (h2 :+: h1) where
  hInject :: h1 f a -> (h2 :+: h1) f a
  hInject = HInjR
  hProject :: (h2 :+: h1) f a -> Maybe (h1 f a)
  hProject = \case
    HInjR x -> Just x
    _ -> Nothing

instance {-# OVERLAPPING #-} (Injective h1 h2) => Injective h1 (h2 :+: h3) where
  hInject :: h1 f a -> (h2 :+: h3) f a
  hInject = HInjL . hInject
  hProject :: (h2 :+: h3) f a -> Maybe (h1 f a)
  hProject = \case
    HInjL x -> hProject x
    _ -> Nothing