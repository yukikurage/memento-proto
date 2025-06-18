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

-- Internal solve function with loop detection
solve :: ConstraintSet -> Substitution -> SolveResult
solve = solveWithHistory Set.empty
  where
    solveWithHistory :: Set.Set ConstraintSet -> ConstraintSet -> Substitution -> SolveResult
    solveWithHistory history cs subst =
      if cs `Set.member` history
      then Contradiction  -- Loop detected! Treat as unsolvable
      else
        let newHistory = Set.insert cs history  -- Add to history BEFORE solving
        in case solveStep cs subst of
          StepSuccess newSubst remainingCs ->
            if Set.null remainingCs
            then Success (Map.union newSubst subst)
            else solveWithHistory newHistory remainingCs (Map.union newSubst subst)
          StepContradiction -> Contradiction
          StepAmbiguous branches ->
            let results = map (\branch -> solveWithHistory newHistory branch subst) branches
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
              if Map.null newSubst && Set.size remaining' == Set.size remaining
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

-- | decompose & check clear contradictions (nothing means contradiction)
-- |
-- | NOTE: these insights are based on type application == type assignment for type constructor of data type
-- | with type synonym, some of these will be wrong.
-- | Ex).  Generics <: t2 where t2 in type application   is no longer unknown <: t2.
-- |       however, without type synonym, generics and t2 (type constructor) are 100% incompatible so unknown <: t2 is enough
-- |
-- |
-- | CLEAR CONTRADICTIONS (contradiction)
-- | - t1 <: t2 where t1, t2 don't contain type variable -> statically checkable
-- | - Generics x <: Generics y   -> CONTRADICTION iff x /= y
-- | - NOTE: Var x <: Var y is not contradiction, because these are type variables
-- | - type application t1 <: type application t2 where t1 is incompatible with t2  -> CONTRADICTION
-- |
-- | REMOVABLE
-- | - t1 <: t2 where t1, t2 don't contain type variable -> statically checkable
-- | - Generics x <: Generics x   -> no effects
-- | - Var x <: Var x   -> no effects
-- |
-- | DECOMPOSES:
-- | - t1 <: Intersection t2
-- |   -> t1 <: t2 for each t2 in t2
-- | - Union t1 <: t2
-- |   -> t1 <: t2 for each t1 in t1
-- | MEMO: FOLLOWINGS don't contain REMAINS section
-- | - Generics <: t2 where t2 in {literal, prim, function, unknown, never, type application}
-- |   -> this can be decompose as unknown <: t2
-- | - t1 <: Generics where t1 in {literal, prim, function, unknown, never, type application}
-- |   -> same as above, to t1 <: never
-- | - function <: non-function t2 in {literal, prim, unknown, never, type application}
-- | - -> this can be decomposed as unknown <: t2
-- | - non-function t1 in {literal, prim, unknown, never, type application} <: function
-- | - -> same as above, to t1 <: never
-- | - Type Application <: t2 where t2 in {literal, prim, unknown, never}
-- | - -> unknown <: t2
-- | - t1 <: Type Application where t1 in {literal, prim, unknown, never}
-- | - -> t1 <: never
-- |
-- | REMAINS:
-- | - t1 <: Union ts    -- handled by world branching
-- | - Inter ts <: t2    -- Same as above
-- | - Var <: t2 -- handled by bounding
-- | - t1 <: Var -- same as above

decomposeConstraint :: Constraint -> Maybe (Set.Set Constraint)
decomposeConstraint cns = case cns of
  Subtype t1 (TIntersection ts) ->
    Just $ Set.fromList [Subtype t1 t | t <- Set.toList ts]
  Subtype (TUnion ts) t2 ->
    Just $ Set.fromList [Subtype t t2 | t <- Set.toList ts]
-- ([VS1, VS2...] -> VS3) <: ([VT1, VT2,...] -> VT3) → VT1 <: VS1, VT2 <: VS2, ... , and return type : VS3 <: VT3
  Subtype (TFunction as1 r1) (TFunction as2 r2) -> Set.fromList $
    [Subtype a2 a1 | (a1, a2) <- zip as1 as2] ++
    [Subtype r1 r2]
  -- TODO: Future work; for type synonyms, Some<number> <: Maybe<unknown> should be decomposable to number <: unknown,
  -- but currently it's not necessary because we don't have type synonyms in the language.
  -- NOTE: currently, type synonyms are replaced with it's definition, but in the future we may want to support recursive type synonym,
  -- which is not replaced with concrete type.
  -- So base1 == base2 should be modified in the some future.
  Subtype (TApplication base1 args1) (TApplication base2 args2)
    | base1 == base2 && length args1 == length args2 ->
      -- For same type constructor, decompose arguments
      Set.fromList [Subtype a1 a2 | (a1, a2) <- zip args1 args2]
  -- Path through:


-- FIXED: Type application decomposition with variance
decomposeSubtype (TApplication base1 args1) (TApplication base2 args2)
  | base1 == base2 && length args1 == length args2 =
    -- For same type constructor, decompose arguments
    -- TODO: Use variance information for proper decomposition
    Set.fromList [Subtype a1 a2 | (a1, a2) <- zip args1 args2]

-- Base case: no decomposition needed
decomposeSubtype t1 t2 = Set.singleton (Subtype t1 t2)

-- Step 2: Contradiction check - IMPROVED ALGORITHM
-- Only check contradictions on fully concrete constraints, never with type variables
step2_contradictionCheck :: ConstraintSet -> Either String (Set.Set Constraint, ConstraintSet)
step2_contradictionCheck cs =
  let (fullyConcreteConstraints, remainingConstraints) = Set.partition isFullyConcrete cs
      contradictoryConstraints = Set.filter isDefinitelyContradictory fullyConcreteConstraints
  in if Set.null contradictoryConstraints
     then Right (fullyConcreteConstraints, remainingConstraints)
     else Left $ "Contradiction found: " ++ show (Set.toList contradictoryConstraints)
  where
    -- A constraint is fully concrete if both sides have no variables and no generics
    isFullyConcrete :: Constraint -> Bool
    isFullyConcrete (Subtype t1 t2) =
      isConcreteType t1 && isConcreteType t2

    isConcreteType :: Type -> Bool
    isConcreteType t = not (containsVar t) && not (containsGeneric t)

    -- Only call isSubtype on definitely concrete constraints
    isDefinitelyContradictory :: Constraint -> Bool
    isDefinitelyContradictory (Subtype t1 t2) =
      not (isSubtype t1 t2)  -- Safe to call since we checked isFullyConcrete

-- REMOVED: isCheckableConstraint - replaced by improved algorithm

-- REMOVED: Old unused contradiction checking functions

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

-- Check if a type is a type application
isApplication :: Type -> Bool
isApplication (TApplication _ _) = True
isApplication _ = False

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

    -- Special case: if we have exactly one concrete (non-variable) lower bound
    -- and all upper bounds are type variables, we can safely assign the concrete type
    -- BUT: generalize literal types to their base types for better polymorphic inference
    _ | length concreteLowers == 1 && all isTypeVar uppers ->
      Just (generalizeToBaseType (head concreteLowers))

    -- Special case: if we have only concrete lower bounds and no upper bounds,
    -- we can pick the most general (for now, just pick the first)
    -- BUT: generalize literal types to their base types
    _ | not (null concreteLowers) && null uppers ->
      Just (generalizeToBaseType (head concreteLowers))

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

    -- Get concrete (non-variable) types from lower bounds
    concreteLowers = [t | t <- lowers, not (containsVar t), canUnifyWithGeneric t]

    -- Check if a type is a type variable
    isTypeVar (TVar _) = True
    isTypeVar _ = False

    -- Generalize literal types to their base types for polymorphic inference
    generalizeToBaseType :: Type -> Type
    generalizeToBaseType (TLiteral (LNumber _)) = TNumber
    generalizeToBaseType (TLiteral (LBool _)) = TBool
    generalizeToBaseType (TLiteral (LString _)) = TString
    generalizeToBaseType t = t

-- Step 5: Branch splitting (for step-by-step solving)
step5_branchStep :: ConstraintSet -> SolveStepResult
step5_branchStep cs =
  case findBranchableConstraint cs of
    Nothing ->
      if Set.null cs
      then StepSuccess Map.empty Set.empty -- No more constraints to solve
      else StepSuccess Map.empty cs -- Can't make progress, but constraints remain
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
