{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.Data.NaturalTransformation where

-- Natural Transformation
type f ~> g = forall a. f a -> g a