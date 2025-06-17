{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.HFix where

import Data.Kind (Type)

-- Higher-order Fixed Point
data HFix :: ((Type -> Type) -> (Type -> Type)) -> Type -> Type where
  HFix :: {unHFix :: h (HFix h) a} -> HFix h a

deriving instance (Show (h (HFix h) a)) => Show (HFix h a)
deriving instance (Eq (h (HFix h) a)) => Eq (HFix h a)
deriving instance (Ord (h (HFix h) a)) => Ord (HFix h a)