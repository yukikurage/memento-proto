{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.Data.HFix where

import Data.Kind (Type)

-- Higher-order Fixed Point
data HFix :: ((Type -> Type) -> (Type -> Type)) -> Type -> Type where
  HFix :: {unHFix :: h (HFix h) a} -> HFix h a
