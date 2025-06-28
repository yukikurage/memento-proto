{-# LANGUAGE OverloadedStrings #-}

-- | Constraint propagation for the Memento type solver
-- Propagates type variable bounds through constraint sets
module Language.Memento.TypeSolver.Constraints.Propagation
  ( calculatePropagationAll,
    calculatePropagation,
    calculateBounds,
    constraintVars,
  )
where

import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Memento.TypeSolver.Core.Types

-- | Extract all type variables from a constraint
constraintVars :: Constraint -> Set.Set TypeVar
constraintVars (Subtype t1 t2) =
  Set.union (typeVars t1) (typeVars t2)

-- | Calculate bounds for a specific type variable from constraint set
calculateBounds :: TypeVar -> ConstraintSet -> Bounds
calculateBounds var cs =
  let lowerBounds = catMaybes [getLowerBound var c | c <- Set.toList cs]
      upperBounds = catMaybes [getUpperBound var c | c <- Set.toList cs]
   in Bounds lowerBounds upperBounds

-- | Extract lower bound for a variable from a constraint (t <: var)
getLowerBound :: TypeVar -> Constraint -> Maybe Type
getLowerBound var (Subtype t1 (TVar v)) | v == var = Just t1
getLowerBound _ _ = Nothing

-- | Extract upper bound for a variable from a constraint (var <: t)
getUpperBound :: TypeVar -> Constraint -> Maybe Type
getUpperBound var (Subtype (TVar v) t2) | v == var = Just t2
getUpperBound _ _ = Nothing

-- | Perform one round of constraint propagation
-- Creates new constraints from variable bounds (lower <: upper)
calculatePropagation :: ConstraintSet -> Maybe ConstraintSet
calculatePropagation cs =
  let vars = Set.unions (Set.map constraintVars cs)
      boundsMap = Map.fromList [(var, calculateBounds var cs) | var <- Set.toList vars]
      newConstraints = concat $ Map.elems $ Map.map (\(Bounds lowers uppers) -> [Subtype lower upper | lower <- lowers, upper <- uppers]) boundsMap
      -- Filter out constraints that already exist or are trivial
      actuallyNewConstraints = filter (\c -> not (Set.member c cs) && not (isTrivial c)) newConstraints
   in if null actuallyNewConstraints
        then Nothing -- No new constraints to add
        else Just (Set.fromList actuallyNewConstraints) -- Return only truly new constraints
  where
    isTrivial (Subtype t1 t2) = t1 == t2

-- | Recursively calculate propagation until no new constraints are added
-- This is the main propagation function used by the solver pipeline
calculatePropagationAll :: ConstraintSet -> ConstraintSet
calculatePropagationAll cs =
  case calculatePropagation cs of
    Nothing -> cs -- No new constraints, return original
    Just newCs ->
      -- Add new constraints and continue propagating
      calculatePropagationAll (Set.union cs newCs)