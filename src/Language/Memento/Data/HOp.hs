{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.HOp where

import Data.Kind (Type)
import Language.Memento.Data.HCoproduct (HVoid, type (:+:) (..))
import Language.Memento.Data.HProduct (HUnit (HUnit), type (:*:) (..))

newtype HOp h b (f :: Type -> Type) a = HOp (h f a -> b)

-- h1 :+: h2 :+: ...:+: hn
-- HOp h1 b :*: HOp h2 b :*: ... :*: HOp hn b
-- をマッチして b を得る
class
  HMatch
    (hCoproduct :: (Type -> Type) -> Type -> Type)
    (hOpProduct :: (Type -> Type) -> Type -> Type)
    (b :: Type)
    | hCoproduct b -> hOpProduct
    , hOpProduct -> hCoproduct
  where
  hMatch :: hCoproduct f a -> hOpProduct f a -> b

instance
  (HMatch hcps hops b) =>
  HMatch (hcps :+: hcp) (hops :*: HOp hcp b) b
  where
  hMatch hcps (hops :*: HOp f) = case hcps of
    HInjL hcps' -> hMatch hcps' hops
    HInjR hcp' -> f hcp'

instance HMatch HVoid HUnit b where
  hMatch v HUnit = case v of {}