{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Language.Memento.TypeSolver.Assumption where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace)
import Language.Memento.TypeSolver.Normalize
import Language.Memento.TypeSolver.Subtype
import Language.Memento.TypeSolver.Types

calculateGenericBounds :: TypeConstructorVariances -> AssumptionSet -> GenericBoundsMap
calculateGenericBounds varMap assumptions =
  -- trace ("calculateGenericBounds: assumptions = " ++ show assumptions) $
  let
    decomposed = decomposeAssumptionAll varMap assumptions

    takeUpper :: T.Text -> Constraint -> Maybe Type
    takeUpper name cns = case cns of
      Subtype (TGeneric n) t2 | name == n -> Just t2
      _ -> Nothing

    takeLower :: T.Text -> Constraint -> Maybe Type
    takeLower name cns = case cns of
      Subtype t1 (TGeneric n) | name == n -> Just t1
      _ -> Nothing

    takeUppers name = mapMaybe (takeUpper name) $ Set.toList decomposed
    takeLowers name = mapMaybe (takeLower name) $ Set.toList decomposed

    generics =
      mapMaybe
        ( \cns -> case cns of
            Subtype (TGeneric n) _ -> Just n
            Subtype _ (TGeneric n) -> Just n
            _ -> Nothing
        )
        $ Set.toList decomposed
   in
    Map.fromList $
      map
        ( \name ->
            let uppers = takeUppers name
                lowers = takeLowers name
             in ( name
                , (Set.fromList lowers, Set.fromList uppers)
                )
        )
        generics

decomposeAssumptionAll :: TypeConstructorVariances -> AssumptionSet -> AssumptionSet
decomposeAssumptionAll varMap assumptions =
  let decomposeResult = Set.map (decomposeAssumption varMap) assumptions
      remained = Set.unions $ Set.map fst decomposeResult
      decomposed = Set.unions $ Set.map snd decomposeResult
   in -- trace ("decomposeAssumptionAll: remained = " ++ show remained ++ ", decomposed = " ++ show decomposed) $
      if Set.null decomposed
        then remained
        else Set.union remained $ decomposeAssumptionAll varMap decomposed

{- | Decomposes a assumption into assumptions.
| (remained for later generics bound computation, decomposed)
| TODO: Propagation
-}
decomposeAssumption :: TypeConstructorVariances -> Constraint -> (AssumptionSet, AssumptionSet)
decomposeAssumption varMap cns = case cns of
  Subtype t1 t2 | t1 == t2 -> (Set.empty, Set.empty) -- Same type, no contradiction
  Subtype TNever t2 -> (Set.empty, Set.empty) -- Never is a subtype of anything
  Subtype t1 TUnknown -> (Set.empty, Set.empty) -- Unknown is a supertype of anything]
  Subtype t1 t2
    | not (containsGeneric t1 || containsGeneric t2) -> (Set.empty, Set.empty)
  Subtype (TUnion ts) t -> (Set.empty,) $ Set.fromList [Subtype t' t | t' <- Set.toList ts]
  Subtype t (TIntersection ts) -> (Set.empty,) $ Set.fromList [Subtype t t' | t' <- Set.toList ts]
  Subtype t (TUnion ts) -> (Set.empty, Set.empty)
  Subtype (TIntersection ts) t -> (Set.empty, Set.empty)
  -- Save
  Subtype (TGeneric n) t -> (Set.singleton $ Subtype (TGeneric n) t, Set.empty)
  Subtype t (TGeneric n) -> (Set.singleton $ Subtype t (TGeneric n), Set.empty)
  -- Decompose structured types
  Subtype (TFunction args1 ret1) (TFunction args2 ret2)
    | length args1 == length args2 ->
        let argAssumptions = Set.fromList $ zipWith Subtype args2 args1 -- Contravariant
            retAssumptions = Set.singleton $ Subtype ret1 ret2 -- Covariant
         in (Set.empty, argAssumptions `Set.union` retAssumptions)
  Subtype (TApplication tc1 args1) (TApplication tc2 args2)
    | tc1 == tc2
    , Just variances <- Map.lookup tc1 varMap
    , length args1 == length args2 ->
        let argAssumptions = Set.unions $ zipWith3 mkAssumption variances args2 args1
            mkAssumption variance arg1 arg2 =
              case variance of
                Covariant -> Set.singleton $ Subtype arg1 arg2 -- Covariant
                Contravariant -> Set.singleton $ Subtype arg2 arg1 -- Contravariant
                Invariant -> Set.fromList [Subtype arg1 arg2, Subtype arg2 arg1] -- Invariant
                Bivariant -> Set.empty
         in (Set.empty, argAssumptions)
  Subtype _ _ -> (Set.empty, Set.empty) -- Generic case, keep as is
