{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeSolver.Constraints.Subtype where

import Control.Monad (when, zipWithM)
import Control.Monad.Error.Class (MonadError (throwError))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace)
import Language.Memento.TypeSolver.Core.Types

someM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
someM _ [] = pure False
someM f (x : xs) = do
  result <- f x
  if result
    then pure True
    else someM f xs

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM f (x : xs) = do
  result <- f x
  if result
    then allM f xs
    else pure False

orM :: (Monad m) => [m Bool] -> m Bool
orM = someM id

andM :: (Monad m) => [m Bool] -> m Bool
andM = allM id

-- | Check if type `t1` is a subtype of type `t2`.

-- Default case
isSubtype ::
  forall m.
  (MonadError TypeError m) =>
  TypeConstructorVariances ->
  GenericBoundsMap ->
  Type ->
  Type ->
  m Bool
isSubtype variances bounds t1 t2 = do
  when (containsVar t1 || containsVar t2) $
    throwError $
      InternalTypeError $
        "Cannot check subtype relation for types with variables: "
          <> formatType t1
          <> " and "
          <> formatType t2

  go t1 t2
 where
  go t1 t2 = case (t1, t2) of
    (_, TUnknown) -> pure True
    (TNever, _) -> pure True
    _ | t1 == t2 -> pure True
    (TNumber, TNumber) -> pure True
    (TBool, TBool) -> pure True
    (TString, TString) -> pure True
    (TLiteral (LNumber _), TNumber) -> pure True
    (TLiteral (LBool _), TBool) -> pure True
    (TLiteral (LString _), TString) -> pure True
    (TFunction args1 r1, TFunction args2 r2)
      | length args1 == length args2 -> do
          argResults <- zipWithM go args2 args1
          returnResult <- go r1 r2
          return $ and argResults && returnResult
      | otherwise -> pure False
    (TUnion ts1, t2) -> do
      tsResults <- mapM (go t2) (Set.toList ts1)
      return $ and tsResults
    (t1, TIntersection ts2) -> do
      tsResults <- mapM (go t1) (Set.toList ts2)
      return $ and tsResults
    (t1, TUnion ts2) -> do
      tsResults <- mapM (go t1) (Set.toList ts2)
      return $ or tsResults
    (TIntersection ts1, t2) -> do
      tsResults <- mapM (go t2) (Set.toList ts1)
      return $ or tsResults
    (TGeneric name1, TGeneric name2)
      | name1 == name2 -> pure True
      | otherwise ->
          let (_, upperOf1) = lookupGenericBounds name1 bounds
              (lowerOf2, _) = lookupGenericBounds name2 bounds
           in pure $ Set.member t2 upperOf1 || Set.member t1 lowerOf2
    (TGeneric name, t2) ->
      let
        (_, uppers) = lookupGenericBounds name bounds
        uppersExceptGeneric = Set.filter (not . containsGeneric) uppers
       in
        do
          someM (`go` t2) $ Set.toList uppersExceptGeneric
    (t1, TGeneric name) -> do
      let
        (lowers, _) = lookupGenericBounds name bounds
        lowersExceptGeneric = Set.filter (not . containsGeneric) lowers
       in
        someM (go t1) $ Set.toList lowersExceptGeneric
    (TApplication typeConsName1 args1, TApplication typeConsName2 args2)
      | typeConsName1 /= typeConsName2 -> pure False
      | length args1 == length args2 -> pure False
      | otherwise -> checkArgument typeConsName1 args1 args2
     where
      checkArgument :: T.Text -> [Type] -> [Type] -> m Bool
      checkArgument typeConsName args1 args2
        | Just variances <- Map.lookup typeConsName variances =
            checkArgumentsWithVariance variances args1 args2
        | otherwise = throwError $ UnboundTypeVariable Nothing typeConsName

      checkArgumentsWithVariance :: [Variance] -> [Type] -> [Type] -> m Bool
      checkArgumentsWithVariance [] [] [] = pure True
      checkArgumentsWithVariance (v : variancesRest) (arg1 : args1Rest) (arg2 : args2Rest) = do
        let argCheck = case v of
              Covariant -> go arg1 arg2
              Contravariant -> go arg2 arg1
              Invariant -> andM [go arg1 arg2, go arg2 arg1]
              Bivariant -> orM [go arg1 arg2, go arg2 arg1]
        andM
          [ argCheck
          , checkArgumentsWithVariance variancesRest args1Rest args2Rest
          ]

    -- Decompose Boolean to literals
    (_, TBool) -> do
      falseResult <- go t1 (TLiteral $ LBool False)
      trueResult <- go t1 (TLiteral $ LBool True)
      return $ falseResult || trueResult
    _ -> pure False
