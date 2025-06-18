module Language.Memento.TypeSolver.Solver where

{-
Based on the docs/TYPE_SOLVER.md
-}

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
    StepSuccess newSubst remainingCs -> Success (Map.union newSubst subst)
    StepContradiction -> Contradiction
    StepAmbiguous branches ->
      let results = map (`solve` subst) branches
          successes = [s | Success s <- results]
      in case successes of
        [] -> Contradiction
        [s] -> Success s
        ss -> Ambiguous [] -- Multiple solutions, simplified representation

-- Single solving step
solveStep :: ConstraintSet -> Substitution -> SolveStepResult
solveStep cs subst =
  let cs' = Set.map (applySubstConstraint subst) cs
      normalized = Set.map normalizeConstraint cs'
  in case step1_decompose normalized of
    Left err -> StepContradiction
    Right decomposed ->
      case step2_contradictionCheck decomposed of
        Left err -> StepContradiction
        Right (checked, remaining) ->
          case step3_4_boundaryAndAssignment remaining of
            Left err -> StepContradiction
            Right (newSubst, remaining') ->
              if Map.null newSubst && remaining' == remaining
              then step5_branchStep remaining' -- No progress, try branching
              else StepSuccess newSubst remaining' -- Progress made, continue

-- Result type for a single solving step
data SolveStepResult
  = StepSuccess Substitution ConstraintSet
  | StepContradiction
  | StepAmbiguous [ConstraintSet]
  deriving (Show)

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

-- FIXED: Type application decomposition with variance
decomposeSubtype (TApplication base1 args1) (TApplication base2 args2)
  | base1 == base2 && length args1 == length args2 =
    -- For same type constructor, decompose arguments
    -- TODO: Use variance information for proper decomposition
    Set.fromList [Subtype a1 a2 | (a1, a2) <- zip args1 args2]

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
    _ | not (containsVar t1) && not (containsVar t2) && not (containsGeneric t1) && not (containsGeneric t2) -> True
    -- (Special case of) Case ii: x <: x
    (TVar v1, TVar v2) | v1 == v2 -> True
    -- FIXED: Generic type cases
    (TGeneric n1, TGeneric n2) | n1 == n2 -> True
    (TGeneric _, _) | not (containsVar t2) && not (containsGeneric t2) -> True
    (_, TGeneric _) | not (containsVar t1) && not (containsGeneric t1) -> True
    -- Case v: function vs non-function
    (TFunction {}, _) | not (containsVar t2) && not (isFunction t2) && not (containsGeneric t2) -> True
    (_, TFunction {}) | not (containsVar t1) && not (isFunction t1) && not (containsGeneric t1) -> True
    _ -> False

isContradictory :: Constraint -> Bool
isContradictory (Subtype t1 t2) =
  case (t1, t2) of
    _ | not (containsVar t1) && not (containsVar t2) && not (containsGeneric t1) && not (containsGeneric t2) -> not (isSubtype t1 t2)
    (TVar v1, TVar v2) | v1 == v2 -> False
    -- FIXED: Enhanced generic type contradiction checking
    (TGeneric name1, TGeneric name2) | name1 == name2 -> False  -- Same generic, no contradiction
    (TGeneric name1, TGeneric name2) | name1 /= name2 -> True   -- Different generics can contradict
    (TGeneric _, _) | not (containsVar t2) && not (containsGeneric t2) -> False  -- Generic can be any concrete type
    (_, TGeneric _) | not (containsVar t1) && not (containsGeneric t1) -> False  -- Any concrete type can be subtype of generic
    -- Function type contradictions with generic awareness
    (TFunction {}, _) | not (containsVar t2) && not (isFunction t2) && not (containsGeneric t2) ->
      not (isSubtype t1 t2)
    (_, TFunction {}) | not (containsVar t1) && not (isFunction t1) && not (containsGeneric t1) ->
      not (isSubtype t1 t2)
    _ -> False

-- Step 3 & 4: Boundary calculation and assignment
step3_4_boundaryAndAssignment :: ConstraintSet -> Either String (Substitution, ConstraintSet)
step3_4_boundaryAndAssignment cs =
  let vars = Set.unions (Set.map constraintVars cs)
      -- Calculate all bounds at once, WITHOUT removing constraints
      allBounds = Map.fromList [(var, calculateBoundsKeepConstraints var cs) | var <- Set.toList vars]
  in processBounds allBounds cs
  where
    processBounds :: Map.Map TypeVar Bounds -> ConstraintSet -> Either String (Substitution, ConstraintSet)
    processBounds boundsMap cs =
      let unifications = Map.mapMaybe unifyBounds boundsMap
      in if Map.null unifications
         then Right (Map.empty, cs)  -- No unifications possible
         else
           let -- Apply unifications and remove solved constraints
               newSubst = unifications
               remainingCs = Set.filter (not . isUnifiedConstraint newSubst) cs
           in Right (newSubst, remainingCs)

    -- Check if a constraint is solved by the unifications
    isUnifiedConstraint :: Substitution -> Constraint -> Bool
    isUnifiedConstraint subst (Subtype t1 t2) =
      let t1' = applySubst subst t1
          t2' = applySubst subst t2
      in case (t1', t2') of
           (TVar v1, TVar v2) -> v1 == v2
           _ -> not (containsVar t1') && not (containsVar t2') && isSubtype t1' t2'

constraintVars :: Constraint -> Set.Set TypeVar
constraintVars (Subtype t1 t2) = typeVars t1 `Set.union` typeVars t2

-- Calculate bounds for a single variable WITHOUT removing constraints
-- FIXED: Properly handle bidirectional constraints
calculateBoundsKeepConstraints :: TypeVar -> ConstraintSet -> Bounds
calculateBoundsKeepConstraints var cs =
  let relevant = Set.filter (mentionsVar var) cs
      lowerBounds = catMaybes [getLowerBound var c | c <- Set.toList relevant]
      upperBounds = catMaybes [getUpperBound var c | c <- Set.toList relevant]
      -- Add bounds from generic types and other type variables
      boundsFromGenerics = catMaybes [getBoundsFromGenerics var c | c <- Set.toList relevant]
  in Bounds (lowerBounds ++ boundsFromGenerics) upperBounds

-- Calculate bounds for a single variable (legacy function for compatibility)
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

-- FIXED: Extract bounds from constraints involving generic types
getBoundsFromGenerics :: TypeVar -> Constraint -> Maybe Type
getBoundsFromGenerics var (Subtype (TVar v) (TGeneric name)) | v == var = Just (TGeneric name)
getBoundsFromGenerics var (Subtype (TGeneric name) (TVar v)) | v == var = Just (TGeneric name)
getBoundsFromGenerics _ _ = Nothing

-- Check if a type is a function type
isFunction :: Type -> Bool
isFunction (TFunction _ _) = True
isFunction _ = False

-- FIXED: Check if a type contains generic types
containsGeneric :: Type -> Bool
containsGeneric (TGeneric _) = True
containsGeneric (TFunction arg ret) = containsGeneric arg || containsGeneric ret
containsGeneric (TUnion types) = any containsGeneric (Set.toList types)
containsGeneric (TIntersection types) = any containsGeneric (Set.toList types)
containsGeneric (TApplication base args) = containsGeneric base || any containsGeneric args
containsGeneric _ = False

-- Generic types should not be unified with type variables
-- (they represent bound type parameters, not unification variables)
canUnifyWithGeneric :: Type -> Bool
canUnifyWithGeneric (TGeneric _) = False
canUnifyWithGeneric _ = True

-- Try to unify bounds into a single type
-- FIXED: Only unify when same type appears in both bounds (per TYPE_SOLVER.md step 4)
unifyBounds :: Bounds -> Maybe Type
unifyBounds (Bounds lowers uppers) =
  case (lowers, uppers) of
    ([], []) -> Nothing
    -- REMOVED UNSOUND CASES:
    -- ([t], []) -> Just t  -- UNSOUND: could assign any supertype
    -- ([], [t]) -> Just t  -- UNSOUND: could assign any subtype

    -- Find types that appear in BOTH lower and upper bounds
    _ -> case findCommonTypes lowers uppers of
           [] -> Nothing
           [t] -> Just t  -- Exactly one common type
           _ -> Nothing   -- Multiple common types = ambiguous
  where
    -- Find types that appear in both lists (intersection)
    -- Exclude type variables and generics from unification
    findCommonTypes :: [Type] -> [Type] -> [Type]
    findCommonTypes ls us = [t | t <- ls, t `elem` us, not (containsVar t), canUnifyWithGeneric t]

-- Step 5: Branch splitting (for step-by-step solving)
step5_branchStep :: ConstraintSet -> SolveStepResult
step5_branchStep cs =
  case findBranchableConstraint cs of
    Nothing -> StepSuccess Map.empty Set.empty -- No more constraints to solve
    Just (c, remaining) ->
      let branches = branchConstraint c
      in StepAmbiguous (map (`Set.insert` remaining) branches)

-- Step 5: Branch splitting (legacy function for compatibility)
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
-- FIXED: Generic type branching
branchConstraint (Subtype (TGeneric name) (TUnion ts)) =
  [Subtype (TGeneric name) t | t <- Set.toList ts]
branchConstraint (Subtype (TIntersection ts) (TGeneric name)) =
  [Subtype t (TGeneric name) | t <- Set.toList ts]
branchConstraint c = [c] -- No branching needed

-- Helper functions
normalizeConstraint :: Constraint -> Constraint
normalizeConstraint (Subtype t1 t2) = Subtype (normalize t1) (normalize t2)
