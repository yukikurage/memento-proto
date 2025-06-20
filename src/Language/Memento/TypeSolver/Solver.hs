{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.Solver where

{-
Based on the docs/TYPE_SOLVER.md
-}

import Control.Monad (foldM)

-- import Debug.Trace (trace)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.TypeSolver.Normalize
import Language.Memento.TypeSolver.Subtype
import Language.Memento.TypeSolver.Types

-- Single solving step
solve :: TypeConstructorVariances -> ConstraintSet -> SolveResult
solve varMap cs =
  let normalized = Set.map normalizeConstraint cs
   in -- _ = trace ("solve: constraints = " ++ show (Set.size cs) ++ " constraints: " ++ show cs) ()
      case decomposeConstraintsAll varMap normalized of
        Left err -> Contradiction err
        Right remaining ->
          let substed = substInstancesAsPossible remaining -- Try to substitute instances as much as possible
              bounds = calculateInstanceFromBounds
           in -- _ = trace ("solve: after substitution, constraints = " ++ show substed) ()
              case branchConstraints varMap substed of
                Nothing ->
                  -- Then, we can assume there are only BOUND constraints (ref: DECOMPOSE.md)
                  case checkContradictions (calculatePropagationAll substed) of
                    Left err -> Contradiction err
                    Right () -> Success
                Just branches ->
                  let branchResults = map (solve varMap) branches
                   in if elem Success branchResults
                        then Success
                        else Contradiction "Ambiguous branches found"

{- | Repeat decomposition while there are still constraints to decompose
| All remaining may be "BRANCH" or "BOUND" pair (ref: DECOMPOSE.md)
-}
decomposeConstraintsAll :: TypeConstructorVariances -> ConstraintSet -> Either String ConstraintSet
decomposeConstraintsAll varMap cs = do
  (decomposed, remaining) <- decomposeConstraints varMap cs
  if Set.null remaining
    then Right decomposed -- No more constraints to decompose
    else do
      nextDecomposed <- decomposeConstraintsAll varMap remaining
      Right (Set.union decomposed nextDecomposed)

-- Step 1: Decompose constraints
decomposeConstraints :: TypeConstructorVariances -> ConstraintSet -> Either String (ConstraintSet, ConstraintSet)
decomposeConstraints varMap cs =
  fmap (\css -> (Set.unions $ map fst css, Set.unions $ map snd css)) $
    mapM (decomposeConstraint varMap) $
      Set.toList cs

{- | decompose & check clear contradictions (nothing means contradiction)
| NOTE: DECOMPOSE.md
| Return type : (branching later, decomposed)
-}
decomposeConstraint :: TypeConstructorVariances -> Constraint -> Either String (ConstraintSet, ConstraintSet)
decomposeConstraint varMap cns = case cns of
  -- If both sides are concrete, check subtypes directly
  Subtype t1 t2
    | not (containsVar t1 || containsVar t2) ->
        if isSubtype t1 t2
          then Right (Set.empty, Set.empty) -- No contradiction, return empty set
          else Left $ "Contradiction found: (" ++ show t1 ++ ") is not a subtype of (" ++ show t2 ++ ")"
  Subtype t1 t2 | t1 == t2 -> Right (Set.empty, Set.empty) -- Same type, no contradiction
  Subtype TNever t2 -> Right (Set.empty, Set.empty) -- Never is a subtype of anything
  Subtype t1 TUnknown -> Right (Set.empty, Set.empty) -- Unknown is a supertype of anything
  Subtype (TUnion ts1) t2 -> Right (Set.empty, Set.fromList [Subtype t t2 | t <- Set.toList ts1]) -- Decompose union
  Subtype t1 (TIntersection ts2) -> Right (Set.empty, Set.fromList [Subtype t1 t | t <- Set.toList ts2]) -- Decompose intersection
  Subtype (TIntersection ts1) t2 -> Right (Set.singleton cns, Set.empty) -- Leave for world branching later
  Subtype t1 (TUnion ts2) -> Right (Set.singleton cns, Set.empty) -- Leave for world branching later
  Subtype (TVar var) t2 -> Right (Set.singleton (Subtype (TVar var) t2), Set.empty) -- Type variable, leave for branching or boundary check
  Subtype t1 (TVar var) -> Right (Set.singleton (Subtype t1 (TVar var)), Set.empty) -- Type variable, leave for branching or boundary check
  Subtype (TApplication name1 args1) (TApplication name2 args2)
    | name1 == name2 && length args1 == length args2 -> case Map.lookup name1 varMap of
        Nothing -> Left $ "No variance information for type constructor: " ++ show name1
        Just variances ->
          Right
            ( Set.empty
            , Set.fromList
                [ constraint
                | (variance, t1, t2) <- zip3 variances args1 args2
                , constraint <- mkConstraintWithVariance t1 t2 variance
                ]
            )
  Subtype (TFunction args1 ret1) (TFunction args2 ret2)
    | length args1 == length args2 ->
        Right
          ( Set.empty
          , Set.fromList $
              [Subtype a2 a1 | (a1, a2) <- zip args1 args2]
                ++ [Subtype ret1 ret2] -- Decompose function arguments and return type
          )
  Subtype t1 t2 -> Left $ "Contradiction found: (" ++ show t1 ++ ") is not a subtype of (" ++ show t2 ++ ")"

data Bounds = Bounds [Type] [Type]

calculateBounds :: TypeVar -> ConstraintSet -> Bounds
calculateBounds var cs =
  let lowerBounds = catMaybes [getLowerBound var c | c <- Set.toList cs]
      upperBounds = catMaybes [getUpperBound var c | c <- Set.toList cs]
   in Bounds lowerBounds upperBounds

getLowerBound :: TypeVar -> Constraint -> Maybe Type
getLowerBound var (Subtype t1 (TVar v)) | v == var = Just t1
getLowerBound _ _ = Nothing

getUpperBound :: TypeVar -> Constraint -> Maybe Type
getUpperBound var (Subtype (TVar v) t2) | v == var = Just t2
getUpperBound _ _ = Nothing

-- | If possible, instantiate the type variable from its bounds
calculateInstanceFromBounds :: Bounds -> Maybe Type
calculateInstanceFromBounds (Bounds lowers uppers)
  | any isGeneric uppers || elem TNever uppers = Just TNever
  | any isGeneric lowers || elem TUnknown lowers = Just TUnknown
  | not (Set.null intersections) =
      -- Pick any element from intersections, but avoid self-substitution
      -- We want to allow x <- y but not x <- x
      Just (Set.findMin intersections)
  | otherwise = Nothing
 where
  intersections = Set.intersection (Set.fromList lowers) (Set.fromList uppers)

  -- Check if a type is a function type
  isFunction :: Type -> Bool
  isFunction (TFunction _ _) = True
  isFunction _ = False

  -- Check if a type is a type application
  isApplication :: Type -> Bool
  isApplication (TApplication _ _) = True
  isApplication _ = False

  isGeneric :: Type -> Bool
  isGeneric (TGeneric _) = True
  isGeneric _ = False

constraintVars :: Constraint -> Set.Set TypeVar
constraintVars (Subtype t1 t2) =
  Set.union (typeVars t1) (typeVars t2)

-- | Try to instantiate type variables as much as possible
substInstancesAsPossible :: ConstraintSet -> ConstraintSet
substInstancesAsPossible cs =
  let csFiltered = filterTrivialConstraints cs -- Remove reflexive constraints
      vars = Set.unions (Set.map constraintVars csFiltered)
      boundsMap = Map.fromList [(var, calculateBounds var csFiltered) | var <- Set.toList vars]
      instances = Map.mapMaybe calculateInstanceFromBounds boundsMap
      -- Filter out self-substitutions (x <- x)
      validInstances = Map.filterWithKey (\var instType -> TVar var /= instType) instances
   in -- _ = trace ("substInstancesAsPossible: vars = " ++ show vars ++ ", instances = " ++ show instances ++ ", valid = " ++ show validInstances) ()
      case Map.lookupMin validInstances of
        Just (var, instanceType) ->
          let newCs = Set.map (applySubstConstraint (Map.singleton var instanceType)) csFiltered
           in substInstancesAsPossible newCs -- Recur with the new constraints
        Nothing -> csFiltered -- No more instances to substitute

{- | Branching BRANCH node (ref : DECOMPOSE.md)
| with some nodes, may contain substitutions
| Returned list represents branches with substitutions
-}
branchConstraint :: TypeConstructorVariances -> Constraint -> Maybe [(Substitution, ConstraintSet)]
branchConstraint varMap cns = case cns of
  Subtype (TIntersection ts) t2 ->
    Just [(Map.empty, Set.fromList [Subtype t t2 | t <- Set.toList ts])] -- Branch on intersection
  Subtype t1 (TUnion ts) ->
    Just [(Map.empty, Set.fromList [Subtype t1 t | t <- Set.toList ts])] -- Branch on union
  Subtype (TVar var@(TypeVar varName)) (TFunction args ret) ->
    let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
        retVar = TVar $ TypeVar (varName <> "_ret")
        substIfNever = Map.singleton var TNever
        substIfFunc = Map.singleton var (TFunction argVars retVar)
     in Just
          [ (substIfNever, Set.empty)
          ,
            ( substIfFunc
            , Set.fromList [Subtype arg argVar | (argVar, arg) <- zip argVars args] -- Contravariant
                `Set.union` Set.singleton (Subtype retVar ret) -- Covariant
            )
          ]
  Subtype (TFunction args ret) (TVar var@(TypeVar varName)) ->
    let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
        retVar = TVar $ TypeVar (varName <> "_ret")
        substIfUnknown = Map.singleton var TUnknown
        substIfFunc = Map.singleton var (TFunction argVars retVar)
     in Just
          [ (substIfUnknown, Set.empty) -- Unknown is a supertype of function
          ,
            ( substIfFunc
            , Set.fromList [Subtype argVar arg | (argVar, arg) <- zip argVars args] -- Covariant
                `Set.union` Set.singleton (Subtype ret retVar) -- Contravariant
            )
          ]
  Subtype (TVar var@(TypeVar varName)) (TApplication name args)
    | Just variances <- Map.lookup name varMap ->
        let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
            substIfNever = Map.singleton var TNever
            substIfApp = Map.singleton var (TApplication name argVars)
         in Just
              [ (substIfNever, Set.empty) -- Never is a subtype of application
              ,
                ( substIfApp
                , Set.fromList (concat $ zipWith3 mkConstraintWithVariance argVars args variances) -- Apply variances
                )
              ]
  Subtype (TApplication name args) (TVar var@(TypeVar varName))
    | Just variances <- Map.lookup name varMap ->
        let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
            substIfUnknown = Map.singleton var TUnknown
            substIfApp = Map.singleton var (TApplication name argVars)
         in Just
              [ (substIfUnknown, Set.empty) -- Unknown is a supertype of application
              ,
                ( substIfApp
                , Set.fromList (concat $ zipWith3 mkConstraintWithVariance args argVars variances) -- Apply variances
                )
              ]
  Subtype _ _ -> Nothing

mkConstraintWithVariance :: Type -> Type -> Variance -> [Constraint]
mkConstraintWithVariance left right variance =
  case variance of
    Invariant -> [Subtype left right, Subtype right left] -- Invariant: both directions
    Covariant -> [Subtype left right] -- Covariant: only t1 is
    Contravariant -> [Subtype right left] -- Contravariant: only t2 is
    Bivariant -> []

branchConstraints :: TypeConstructorVariances -> ConstraintSet -> Maybe [ConstraintSet]
branchConstraints varMap cs = case partitionFirst (branchConstraint varMap) (Set.toList cs) of
  Nothing -> Nothing -- No branchable constraints found
  Just (branches, remaining) ->
    Just $
      map
        ( \(subst, cs') ->
            Set.map (applySubstConstraint subst) $ Set.union (Set.fromList remaining) cs'
        )
        branches

partitionFirst :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
partitionFirst f xs = go f xs []
 where
  go _ [] acc = Nothing -- No match found
  go f (x : xs) acc =
    case f x of
      Just b -> Just (b, acc ++ xs) -- Found a match, return it with remaining elements
      Nothing -> go f xs (x : acc) -- Continue searching

{- | Assume that all constraints is BOUND (ref : DECOMPOSE.md)
| propagate bounds
-}
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
calculatePropagationAll :: ConstraintSet -> ConstraintSet
calculatePropagationAll cs =
  case calculatePropagation cs of
    Nothing -> cs -- No new constraints, return original
    Just newCs ->
      -- Don't call substInstancesAsPossible on newCs - just add them and continue propagating
      calculatePropagationAll (Set.union cs newCs)

{- | Assume that constraints is made by calculatePropagationAll.
| Now, we can check contradictions.
| all constraint has one of a following structures.
| 0. {PRIM, never, lit, var} <: {PRIM, never, lit, var}
| 1. var <: {PRIM, never, lit, var}
| 2. {PRIM, unknown, lit, var} <: var
| 3. var <: var
| It is enough to check pattern 0, because 1, 2, 3 are already propagated to 0.
-}
checkContradictions :: ConstraintSet -> Either String ()
checkContradictions cs = mapM_ check $ Set.toList cs
 where
  check (Subtype t1 t2)
    | not (containsVar t1 || containsVar t2) =
        if isSubtype t1 t2
          then Right () -- No contradiction
          else Left $ "Contradiction found: (" ++ show t1 ++ ") is not a subtype of (" ++ show t2 ++ ")"
  check _ = Right ()

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint (Subtype t1 t2) = Subtype (normalize t1) (normalize t2)

-- Remove reflexive constraints and other trivial constraints
filterTrivialConstraints :: ConstraintSet -> ConstraintSet
filterTrivialConstraints = Set.filter (not . isTrivial)
 where
  isTrivial (Subtype t1 t2) = t1 == t2
