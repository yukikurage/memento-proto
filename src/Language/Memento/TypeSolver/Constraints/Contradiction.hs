{-# LANGUAGE OverloadedStrings #-}

-- | Contradiction checking for the Memento type solver
-- Checks for contradictory type constraints that cannot be satisfied
module Language.Memento.TypeSolver.Constraints.Contradiction
  ( checkContradictions,
  )
where

import Control.Monad (mapM_)
import Control.Monad.Error.Class (MonadError, throwError)
import qualified Data.Set as Set
import Language.Memento.TypeSolver.Constraints.Subtype (isSubtype)
import Language.Memento.TypeSolver.Core.Types

-- | Check for contradictory constraints
-- For constraints without variables, verify that subtype relationships hold
-- If a constraint t1 <: t2 is contradicted by the type system, throw an error
checkContradictions :: (MonadError TypeError m) => TypeConstructorVariances -> GenericBoundsMap -> ConstraintSet -> m ()
checkContradictions varMap genMap cs = mapM_ check $ Set.toList cs
  where
    check (Subtype t1 t2)
      | not (containsVar t1 || containsVar t2) = do
          result <- isSubtype varMap genMap t1 t2
          if result
            then pure ()
            else throwError $ TypeNotSubtype Nothing t1 t2
      | otherwise = do
          pure ()