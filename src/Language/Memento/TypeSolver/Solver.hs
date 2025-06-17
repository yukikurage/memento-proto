module Language.Memento.TypeSolver.Solver where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Control.Monad (foldM)
import Language.Memento.TypeSolver.Types
import Language.Memento.TypeSolver.Normalize
import Language.Memento.TypeSolver.Subtype

-- Main solver entry point
solveConstraints :: ConstraintSet -> SolveResult
solveConstraints cs = solve cs Map.empty

-- Internal solve function
solve :: ConstraintSet -> Substitution -> SolveResult
solve cs subst = 
  case solveStep cs subst of
    Success newSubst -> Success newSubst
    Contradiction -> Contradiction
    Ambiguous branches -> 
      let results = map (`solve` subst) branches
          successes = [s | Success s <- results]
      in case successes of
        [] -> Contradiction
        [s] -> Success s
        ss -> Ambiguous [] -- Multiple solutions, simplified representation

-- Single solving step
solveStep :: ConstraintSet -> Substitution -> SolveResult
solveStep cs subst = 
  let cs' = Set.map (applySubstConstraint subst) cs
      normalized = Set.map normalizeConstraint cs'
  in case step1_decompose normalized of
    Left err -> Contradiction
    Right decomposed -> 
      case step2_contradictionCheck decomposed of
        Left err -> Contradiction
        Right (checked, remaining) -> 
          case step3_4_boundaryAndAssignment remaining of
            Left err -> Contradiction
            Right (newSubst, remaining') -> 
              if Map.null newSubst && remaining' == remaining
              then step5_branch remaining' -- No progress, try branching
              else Success (Map.union newSubst subst) -- Progress made, continue

-- Step 1: Decompose constraints
step1_decompose :: ConstraintSet -> Either String ConstraintSet
step1_decompose cs = 
  let decomposed = Set.unions (Set.map decomposeConstraint cs)
  in Right decomposed

decomposeConstraint :: Constraint -> Set.Set Constraint
decomposeConstraint (Subtype t1 t2) = decomposeSubtype t1 t2

decomposeSubtype :: Type -> Type -> Set.Set Constraint
-- VS <: and(VTn) → VS <: VT_n for each n
decomposeSubtype t1 (TIntersection ts) = 
  Set.fromList [Subtype t1 t | t <- Set.toList ts]

-- or(VSn) <: VT → VS_n <: VT for each n
decomposeSubtype (TUnion ts) t2 = 
  Set.fromList [Subtype t t2 | t <- Set.toList ts]

-- (VS1 -> VS2) <: (VT1 -> VT2) → VT1 <: VS1 and VS2 <: VT2
decomposeSubtype (TFunction a1 r1) (TFunction a2 r2) = 
  Set.fromList [Subtype a2 a1, Subtype r1 r2]

-- Base case: no decomposition needed
decomposeSubtype t1 t2 = Set.singleton (Subtype t1 t2)

-- Step 2: Contradiction check
step2_contradictionCheck :: ConstraintSet -> Either String (Set.Set Constraint, ConstraintSet)
step2_contradictionCheck cs = 
  let (checkable, remaining) = Set.partition isCheckableConstraint cs
      checked = Set.filter (not . isContradictory) checkable
  in if Set.size checked == Set.size checkable
     then Right (checked, remaining)
     else Left "Contradiction found"

isCheckableConstraint :: Constraint -> Bool
isCheckableConstraint (Subtype t1 t2) = 
  case (t1, t2) of
    -- Case i: both sides are concrete types
    _ | not (containsVar t1) && not (containsVar t2) -> True
    -- Case ii: x <: x
    (TVar v1, TVar v2) | v1 == v2 -> True
    -- Case v: function vs non-function
    (TFunction {}, _) | not (containsVar t2) && not (isFunction t2) -> True
    (_, TFunction {}) | not (containsVar t1) && not (isFunction t1) -> True
    _ -> False

isFunction :: Type -> Bool
isFunction (TFunction _ _) = True
isFunction _ = False

isContradictory :: Constraint -> Bool
isContradictory (Subtype t1 t2) = 
  case (t1, t2) of
    _ | not (containsVar t1) && not (containsVar t2) -> not (isSubtype t1 t2)
    (TVar v1, TVar v2) | v1 == v2 -> False
    (TFunction {}, _) | not (containsVar t2) && not (isFunction t2) -> 
      not (isSubtype t1 t2)
    (_, TFunction {}) | not (containsVar t1) && not (isFunction t1) -> 
      not (isSubtype t1 t2)
    _ -> False

-- Step 3 & 4: Boundary calculation and assignment
step3_4_boundaryAndAssignment :: ConstraintSet -> Either String (Substitution, ConstraintSet)
step3_4_boundaryAndAssignment cs = 
  let vars = Set.unions (Set.map constraintVars cs)
  in foldM processSingleVar (Map.empty, cs) (Set.toList vars)
  where
    processSingleVar (subst, cs') var = 
      case calculateBounds var cs' of
        Left err -> Left err
        Right (bounds, remaining) -> 
          case unifyBounds bounds of
            Nothing -> Right (subst, remaining)
            Just unifiedType -> 
              let newSubst = Map.insert var unifiedType subst
              in Right (newSubst, remaining)

constraintVars :: Constraint -> Set.Set TypeVar
constraintVars (Subtype t1 t2) = typeVars t1 `Set.union` typeVars t2

-- Calculate bounds for a single variable
calculateBounds :: TypeVar -> ConstraintSet -> Either String (Bounds, ConstraintSet)
calculateBounds var cs = 
  let (relevant, irrelevant) = Set.partition (mentionsVar var) cs
      lowerBounds = catMaybes [getLowerBound var c | c <- Set.toList relevant]
      upperBounds = catMaybes [getUpperBound var c | c <- Set.toList relevant]
  in Right (Bounds lowerBounds upperBounds, irrelevant)

data Bounds = Bounds [Type] [Type]

mentionsVar :: TypeVar -> Constraint -> Bool
mentionsVar var (Subtype t1 t2) = var `Set.member` typeVars t1 || var `Set.member` typeVars t2

getLowerBound :: TypeVar -> Constraint -> Maybe Type
getLowerBound var (Subtype t1 (TVar v)) | v == var = Just t1
getLowerBound _ _ = Nothing

getUpperBound :: TypeVar -> Constraint -> Maybe Type
getUpperBound var (Subtype (TVar v) t2) | v == var = Just t2
getUpperBound _ _ = Nothing

-- Try to unify bounds into a single type
unifyBounds :: Bounds -> Maybe Type
unifyBounds (Bounds lowers uppers) = 
  case (lowers, uppers) of
    ([], []) -> Nothing
    ([t], []) -> Just t
    ([], [t]) -> Just t
    ([t1], [t2]) | not (containsVar t1) && not (containsVar t2) -> 
      if isSubtype t1 t2 then Just t1 else Nothing
    _ -> Nothing

-- Step 5: Branch splitting
step5_branch :: ConstraintSet -> SolveResult
step5_branch cs = 
  case findBranchableConstraint cs of
    Nothing -> Success Map.empty -- No more constraints to solve
    Just (c, remaining) -> 
      let branches = branchConstraint c
      in Ambiguous (map (`Set.insert` remaining) branches)

findBranchableConstraint :: ConstraintSet -> Maybe (Constraint, ConstraintSet)
findBranchableConstraint cs = 
  case Set.toList cs of
    [] -> Nothing
    (c:_) -> Just (c, Set.delete c cs)

branchConstraint :: Constraint -> [Constraint]
branchConstraint (Subtype (TVar var) (TUnion ts)) = 
  [Subtype (TVar var) t | t <- Set.toList ts]
branchConstraint (Subtype (TIntersection ts) (TVar var)) = 
  [Subtype t (TVar var) | t <- Set.toList ts]
branchConstraint c = [c] -- No branching needed

-- Helper functions
normalizeConstraint :: Constraint -> Constraint
normalizeConstraint (Subtype t1 t2) = Subtype (normalize t1) (normalize t2)