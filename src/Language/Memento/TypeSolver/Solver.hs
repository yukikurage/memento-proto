{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Memento.TypeSolver.Solver where

{-
Based on the docs/TYPE_SOLVER.md
-}

import Control.Monad (foldM)

import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace, traceM)
import Language.Memento.TypeSolver.Assumption (calculateGenericBounds, decomposeAssumptionAll)
import Language.Memento.TypeSolver.Normalize
import Language.Memento.TypeSolver.Subtype
import Language.Memento.TypeSolver.Types
import Safe

solve :: (MonadError TypeError m) => TypeConstructorVariances -> AssumptionSet -> ConstraintSet -> m ()
solve varMap assumptions constraints = do
  traceM ("solve: initial assumptions = " ++ T.unpack (formatConstraintSet assumptions))
  traceM ("solve: initial constraints = " ++ T.unpack (formatConstraintSet constraints))
  normalizedConstraints <- Set.fromList <$> mapM normalizeConstraint (Set.toList constraints)
  normalizedAssumptions <- Set.fromList <$> mapM normalizeConstraint (Set.toList assumptions)
  let
    decomposedAssumptionsFirst = decomposeAssumptionAll varMap normalizedAssumptions
    genBndMapFirst = calculateGenericBounds varMap decomposedAssumptionsFirst -- Calculate generic bounds from assumptions
  traceM ("solve: decomposedAssumptionsFirst = " ++ T.unpack (formatConstraintSet decomposedAssumptionsFirst))
  traceM ("solve: genBndMapFirst = " ++ show genBndMapFirst)
  decomposedConstraintsFirst <- decomposeConstraintsAll varMap genBndMapFirst normalizedConstraints
  traceM ("solve: decomposedConstraintsFirst = " ++ T.unpack (formatConstraintSet decomposedConstraintsFirst))
  let
    (substedAssumptions, substedConstraints) = substInstancesAsPossible (decomposedAssumptionsFirst, decomposedConstraintsFirst) -- Try to substitute instances as much as possible
    decomposedAssumptionsSecond = decomposeAssumptionAll varMap substedAssumptions -- Decompose assumptions again after substitution
    genBndMapSecond = calculateGenericBounds varMap $ decomposedAssumptionsSecond -- Calculate generic bounds from assumptions
  traceM ("solve: decomposedAssumptionsSecond = " ++ T.unpack (formatConstraintSet decomposedAssumptionsSecond))
  traceM ("solve: genBndMapSecond = " ++ show genBndMapSecond)
  decomposedConstraintsSecond <- decomposeConstraintsAll varMap genBndMapSecond substedConstraints
  traceM ("solve: decomposedConstraintsSecond = " ++ T.unpack (formatConstraintSet decomposedConstraintsSecond))

  case branchConstraints varMap decomposedAssumptionsSecond decomposedConstraintsSecond of
    Nothing -> do
      -- Then, we can assume there are only BOUND constraints (ref: DECOMPOSE.md)
      checkContradictions varMap genBndMapSecond $ calculatePropagationAll decomposedConstraintsSecond
    Just branches -> do
      branchResults <- untilSuccess branches $ uncurry (solve varMap)
      case branchResults of
        Right _ -> return () -- At least one branch succeeded
        Left errors -> throwError $ head errors -- All branches failed, return the first error

-- | Run a function until it succeeds or all inputs are exhausted
untilSuccess :: (MonadError e m) => [a] -> (a -> m b) -> m (Either [e] b)
untilSuccess xs f = case xs of
  [] -> pure $ Left []
  (x : xs') -> do
    result <- (Right <$> f x) `catchError` \e -> pure $ Left e
    case result of
      Right v -> pure $ Right v -- Success, return the value
      Left e -> do
        restResult <- untilSuccess xs' f -- Try the rest of the inputs
        case restResult of
          Right v -> pure $ Right v -- Found a success in the rest
          Left es -> pure $ Left (e : es) -- Collect errors

{- | Repeat decomposition while there are still constraints to decompose
| All remaining may be "BRANCH" or "BOUND" pair (ref: DECOMPOSE.md)
-}
decomposeConstraintsAll ::
  (MonadError TypeError m) =>
  TypeConstructorVariances ->
  GenericBoundsMap ->
  ConstraintSet ->
  m ConstraintSet
decomposeConstraintsAll varMap bounds cs = do
  -- traceM ("decomposeConstraintsAll: constraints = " ++ formatConstraintSet cs)
  (decomposed, remaining) <- decomposeConstraints varMap bounds cs
  if Set.null remaining
    then pure decomposed -- No more constraints to decompose
    else do
      nextDecomposed <- decomposeConstraintsAll varMap bounds remaining
      pure (Set.union decomposed nextDecomposed)

-- Step 1: Decompose constraints
decomposeConstraints :: (MonadError TypeError m) => TypeConstructorVariances -> GenericBoundsMap -> ConstraintSet -> m (ConstraintSet, ConstraintSet)
decomposeConstraints varMap bounds cs =
  fmap (\css -> (Set.unions $ map fst css, Set.unions $ map snd css)) $
    mapM (decomposeConstraint varMap bounds) $
      Set.toList cs

{- | decompose & check clear contradictions (nothing means contradiction)
| NOTE: DECOMPOSE.md
| Return type : (branching later, currentlly decomposed)
-}
decomposeConstraint :: (MonadError TypeError m) => TypeConstructorVariances -> GenericBoundsMap -> Constraint -> m (ConstraintSet, ConstraintSet)
decomposeConstraint varMap bounds cns =
  -- trace ("decomposeConstraint: " ++ show cns ++ "\n") $
  case cns of
    Subtype t1 t2 | t1 == t2 -> pure (Set.empty, Set.empty) -- Same type, no contradiction
    Subtype TNever t2 -> pure (Set.empty, Set.empty) -- Never is a subtype of anything
    Subtype t1 TUnknown -> pure (Set.empty, Set.empty) -- Unknown is a supertype of anything
    -- If both sides are concrete (no type variables), check subtypes directly
    Subtype (TGeneric name) t2 -> pure (Set.singleton cns, Set.empty)
    -- case Map.lookup name bounds of
    --   Just (GenericBounds _ uppers) -> Right (Set.empty, Set.fromList [Subtype upper t2 | upper <- uppers])
    --   Nothing -> Right (Set.singleton cns, Set.empty) -- No bounds info, leave for later
    Subtype t1 (TGeneric name) -> pure (Set.singleton cns, Set.empty)
    -- case Map.lookup name bounds of
    --   Just (GenericBounds lowers _) -> Right (Set.empty, Set.fromList [Subtype t1 lower | lower <- lowers])
    --   Nothing -> Right (Set.singleton cns, Set.empty) -- No bounds info, leave for later
    Subtype t1 t2
      | not (containsVar t1 || containsVar t2) -> do
          result <- isSubtype varMap bounds t1 t2
          if result
            then pure (Set.empty, Set.empty) -- No contradiction, return empty set
            else throwError $ TypeNotSubtype Nothing t1 t2
    Subtype (TUnion ts1) t2 -> pure (Set.empty, Set.fromList [Subtype t t2 | t <- Set.toList ts1]) -- Decompose union
    Subtype t1 (TIntersection ts2) -> pure (Set.empty, Set.fromList [Subtype t1 t | t <- Set.toList ts2]) -- Decompose intersection
    Subtype (TIntersection ts1) t2 -> pure (Set.singleton cns, Set.empty) -- Leave for world branching later
    Subtype t1 (TUnion ts2) -> pure (Set.singleton cns, Set.empty) -- Leave for world branching later
    Subtype (TVar var) t2 -> pure (Set.singleton (Subtype (TVar var) t2), Set.empty) -- Type variable, leave for branching or boundary check
    Subtype t1 (TVar var) -> pure (Set.singleton (Subtype t1 (TVar var)), Set.empty) -- Type variable, leave for branching or boundary check
    Subtype (TApplication name1 args1) (TApplication name2 args2)
      | name1 == name2 && length args1 == length args2 -> case Map.lookup name1 varMap of
          Nothing -> throwError $ UnboundTypeVariable Nothing name1
          Just variances ->
            pure
              ( Set.empty
              , Set.fromList
                  [ constraint
                  | (variance, t1, t2) <- zip3 variances args1 args2
                  , constraint <- mkConstraintWithVariance t1 t2 variance
                  ]
              )
    Subtype (TFunction args1 ret1) (TFunction args2 ret2)
      | length args1 == length args2 ->
          pure
            ( Set.empty
            , Set.fromList $
                [Subtype a2 a1 | (a1, a2) <- zip args1 args2]
                  ++ [Subtype ret1 ret2] -- Decompose function arguments and return type
            )
    Subtype t1 t2 -> throwError $ TypeNotSubtype Nothing t1 t2

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
  | TNever `elem` uppers = Just TNever
  | TUnknown `elem` lowers = Just TUnknown
  | (TGeneric g) : _ <- takeGenericsOnly lowers = Just (TGeneric g) -- Prefer generic types
  | (TGeneric g) : _ <- takeGenericsOnly uppers = Just (TGeneric g) -- Prefer generic types
  | not (Set.null intersections) =
      -- Pick any element from intersections, but avoid self-substitution
      -- We want to allow x <- y but not x <- x
      Just (Set.findMin intersections)
  | otherwise = Nothing
 where
  takeGenericsOnly = filter isGeneric

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

{- | Calculate full propagation, that is, the variable that don't appears nested positions.
| ex)
|  x <: T, U <: x    ------>     U <: T
|  x is non-nested var. Following y is not non-nested var
|  y <: T, U <: y, x <: (W => y)
|  if y is nested, then y is not removable.
| NOTE: Infinite Loop?
|   a <: b
|   b <: c
|   c <: a
|     ↓
|   c <: b   <-  a's lower and upper
|   a <: c   <-  b's lower and upper
|   b <: a   <-  c's lower and upper
|     ↓
|    ...
-}
calculateFullPropagation :: AssumptionSet -> ConstraintSet -> Maybe ConstraintSet
calculateFullPropagation as cns =
  -- trace ("calculateFullPropagation: " ++ formatConstraintSet cns ++ "\n") $
  let
    -- All vars
    vars = Set.unions (Set.map constraintVars cns)
    -- vars in nested positions
    nestedVars =
      Set.unions
        ( Set.map
            ( \case
                Subtype (TVar var1) (TVar var2) -> Set.empty
                Subtype (TVar var1) t2 -> typeVars t2 -- t2 is nested
                Subtype t1 (TVar var2) -> typeVars t1 -- t1 is nested
                Subtype t1 t2 -> Set.union (typeVars t1) (typeVars t2) -- Both t1 and t2 are nested
            )
            cns
        )
    nestedVarsAs =
      Set.unions
        ( Set.map
            ( \case
                Subtype t1 t2 -> Set.union (typeVars t1) (typeVars t2) -- Both t1 and t2 are nested
            )
            as
        )
    -- vars that are not nested
    nonNestedVars = (vars `Set.difference` nestedVars) `Set.difference` nestedVarsAs
   in
    trace ("calculateFullPropagation: vars = " ++ show vars ++ ", nestedVars = " ++ show nestedVars ++ ", nestedVarsAs = " ++ show nestedVarsAs ++ ", nonNestedVars = " ++ show nonNestedVars) $
      if Set.null nonNestedVars
        then Nothing -- No non-nested variables, no propagation
        else
          let targetNNV = head $ Set.toList nonNestedVars -- Take one non-nested variable
              Bounds lowers uppers = calculateBounds targetNNV cns
              newConstraints = [Subtype lower upper | lower <- lowers, upper <- uppers]
              oldConstraintsRemoveNonNestedVars =
                Set.filter
                  ( \case
                      Subtype (TVar var1) (TVar var2) ->
                        not $ Set.member var1 nonNestedVars || Set.member var2 nonNestedVars
                      Subtype (TVar var) _ -> not $ Set.member var nonNestedVars
                      Subtype _ (TVar var) -> not $ Set.member var nonNestedVars
                      _ -> True
                  )
                  cns -- Remove constraints that involve non-nested vars
           in Just $
                Set.union
                  oldConstraintsRemoveNonNestedVars
                  (Set.fromList newConstraints) -- Add only truly new constraints

-- Recursive call of calculateFullPropagation, while there is still non-nested vars
calculateFullPropagationAll :: AssumptionSet -> ConstraintSet -> ConstraintSet
calculateFullPropagationAll as cs =
  case calculateFullPropagation as cs of
    Nothing -> cs -- No more non-nested variables, return original
    Just newCs -> calculateFullPropagationAll as newCs -- Recur with the new constraints

-- | Try to instantiate type variables as much as possible
substInstancesAsPossible :: (AssumptionSet, ConstraintSet) -> (AssumptionSet, ConstraintSet)
substInstancesAsPossible (as, cs) =
  -- trace ("substInstancesAsPossible: " ++ formatConstraintSet cs ++ "\n") $
  let csFiltered = filterTrivialConstraints cs -- Remove reflexive constraints
      vars = Set.unions (Set.map constraintVars csFiltered)
      -- boundsMap = Map.fromList [(var, calculateBounds var csFiltered) | var <- Set.toList vars]
      -- instances = Map.mapMaybe calculateInstanceFromBounds boundsMap
      -- -- Filter out self-substitutions (x <- x)
      -- validInstances = Map.filterWithKey (\var instType -> TVar var /= instType) instances
      mValidInstance =
        headMay $
          filter (\(var, instType) -> TVar var /= instType) $
            mapMaybe
              (\var -> (var,) <$> calculateInstanceFromBounds (calculateBounds var csFiltered))
              (Set.toList vars)
   in case mValidInstance of
        Just (var, instanceType) ->
          let newCs = applySubstConstraintSet var instanceType csFiltered
              newAs = applySubstConstraintSet var instanceType as
           in substInstancesAsPossible (newAs, newCs) -- Recur with the new constraints
        Nothing -> (as, calculateFullPropagationAll as csFiltered)

{- | Branching BRANCH node (ref : DECOMPOSE.md)
| with some nodes, may contain substitutions
| Returned list represents branches with substitutions
-}
branchConstraint :: TypeConstructorVariances -> Constraint -> Maybe [([(TypeVar, Type)], ConstraintSet)]
branchConstraint varMap cns = case cns of
  Subtype (TIntersection ts) t2 ->
    Just [([], Set.fromList [Subtype t t2]) | t <- Set.toList ts] -- Branch on intersection
  Subtype t1 (TUnion ts) ->
    Just [([], Set.fromList [Subtype t1 t]) | t <- Set.toList ts] -- Branch on union
  Subtype (TVar var@(TypeVar varName)) (TFunction args ret) ->
    let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
        retVar = TVar $ TypeVar (varName <> "_ret")
        substIfNever = [(var, TNever)]
        substIfFunc = [(var, TFunction argVars retVar)]
     in Just
          [
            ( substIfFunc
            , Set.fromList [Subtype arg argVar | (argVar, arg) <- zip argVars args] -- Contravariant
                `Set.union` Set.singleton (Subtype retVar ret) -- Covariant
            )
          , (substIfNever, Set.empty)
          ]
  Subtype (TFunction args ret) (TVar var@(TypeVar varName)) ->
    let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
        retVar = TVar $ TypeVar (varName <> "_ret")
        substIfUnknown = [(var, TUnknown)]
        substIfFunc = [(var, TFunction argVars retVar)]
     in Just
          [
            ( substIfFunc
            , Set.fromList [Subtype argVar arg | (argVar, arg) <- zip argVars args] -- Covariant
                `Set.union` Set.singleton (Subtype ret retVar) -- Contravariant
            )
          , (substIfUnknown, Set.empty) -- Unknown is a supertype of function
          ]
  Subtype (TVar var@(TypeVar varName)) (TApplication name args)
    | Just variances <- Map.lookup name varMap ->
        let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
            substIfNever = [(var, TNever)]
            substIfApp = [(var, TApplication name argVars)]
         in Just
              [
                ( substIfApp
                , Set.fromList (concat $ zipWith3 mkConstraintWithVariance argVars args variances) -- Apply variances
                )
              , (substIfNever, Set.empty) -- Never is a subtype of application
              ]
  Subtype (TApplication name args) (TVar var@(TypeVar varName))
    | Just variances <- Map.lookup name varMap ->
        let argVars = [TVar $ TypeVar (varName <> "_arg_" <> T.pack (show n)) | n <- [1 .. length args]]
            substIfUnknown = [(var, TUnknown)]
            substIfApp = [(var, TApplication name argVars)]
         in Just
              [
                ( substIfApp
                , Set.fromList (concat $ zipWith3 mkConstraintWithVariance args argVars variances) -- Apply variances
                )
              , (substIfUnknown, Set.empty) -- Unknown is a supertype of application
              ]
  Subtype _ _ -> Nothing

mkConstraintWithVariance :: Type -> Type -> Variance -> [Constraint]
mkConstraintWithVariance left right variance =
  case variance of
    Invariant -> [Subtype left right, Subtype right left] -- Invariant: both directions
    Covariant -> [Subtype left right] -- Covariant: only t1 is
    Contravariant -> [Subtype right left] -- Contravariant: only t2 is
    Bivariant -> []

branchConstraints :: TypeConstructorVariances -> AssumptionSet -> ConstraintSet -> Maybe [(AssumptionSet, ConstraintSet)]
branchConstraints varMap as cs = case partitionFirst (branchConstraint varMap) (Set.toList cs) of
  Nothing -> Nothing -- No branchable constraints found
  Just (branches, remaining) ->
    Just $
      map
        ( \(subst, cs') ->
            ( applySubstConstraintSetRecursive subst as
            , applySubstConstraintSetRecursive subst $
                Set.union (Set.fromList remaining) cs'
            )
        )
        branches

applySubstRecursive :: [(TypeVar, Type)] -> Type -> Type
applySubstRecursive subst t = case subst of
  [] -> t -- No substitutions, return original type
  ((var, instType) : rest) -> applySubstRecursive rest (applySubst var instType t)

applySubstConstraintRecursive :: [(TypeVar, Type)] -> Constraint -> Constraint
applySubstConstraintRecursive subst (Subtype t1 t2) =
  Subtype (applySubstRecursive subst t1) (applySubstRecursive subst t2)

applySubstConstraintSetRecursive :: [(TypeVar, Type)] -> ConstraintSet -> ConstraintSet
applySubstConstraintSetRecursive subst = Set.map (applySubstConstraintRecursive subst)

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

normalizeConstraint :: (MonadError TypeError m) => Constraint -> m Constraint
normalizeConstraint (Subtype t1 t2) = liftA2 Subtype (normalize t1) (normalize t2)

-- Remove reflexive constraints and other trivial constraints
filterTrivialConstraints :: ConstraintSet -> ConstraintSet
filterTrivialConstraints = Set.filter (not . isTrivial)
 where
  isTrivial (Subtype t1 t2) = t1 == t2
