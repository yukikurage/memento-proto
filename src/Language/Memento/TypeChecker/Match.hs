{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Match (
  inferMatchType,
) where

import Control.Monad (foldM, unless, when, zipWithM)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Patterns (checkPatternAndGetBindings, checkPatternCoverage)
import Language.Memento.TypeChecker.Types

-- Type of the inferType function passed as an argument
type InferTypeFunc = Expr -> Maybe Type -> TypeCheck (Type, Effects)

processMatchClause ::
  InferTypeFunc ->
  Map.Map Text ConstructorSignature ->
  Type -> -- Scrutinee type
  Maybe Type -> -- Expected type for the branch body
  (Maybe Type, Effects) -> -- (current overall branch type, accumulated effects)
  Clause ->
  TypeCheck (Maybe Type, Effects)
processMatchClause inferTypeFunc _ scrutineeType mExpectedBranchBodyType (firstBranchTypeOpt, accumulatedEffects) (Clause pattern bodyExpr) =
  do
    (bindings, patternActualType) <- checkPatternAndGetBindings pattern (Just scrutineeType)
    unify scrutineeType patternActualType

    -- Pass mExpectedBranchBodyType to inferTypeFunc for the clause body
    (currentBranchExprType, currentBranchEffects) <- withBindings bindings $ inferTypeFunc bodyExpr mExpectedBranchBodyType

    newFirstBranchTypeOpt <- case firstBranchTypeOpt of
      Nothing -> return $ Just currentBranchExprType
      Just firstBranchType -> do
        unify firstBranchType currentBranchExprType
        return $ Just firstBranchType

    return (newFirstBranchTypeOpt, accumulatedEffects `Set.union` currentBranchEffects)

inferMatchType :: InferTypeFunc -> Type -> [Clause] -> Maybe Type -> TypeCheck (Type, Effects)
inferMatchType inferTypeFunc scrutineeType clauses mExpectedMatchType = do
  when (null clauses) $
    throwError $
      CustomErrorType "Match expression cannot have empty clauses"

  (mExpectedArgType, mExpectedRetType) <- case mExpectedMatchType of
    Just expectedReturnType -> do
      (expectedArgType, (expectedRetType, _)) <- extractConstructorType expectedReturnType
      unify scrutineeType expectedArgType
      return (Just expectedArgType, Just expectedRetType)
    Nothing -> return (Nothing, Nothing) -- Wildcard for arg and ret types
  adtEnv <- getAdtEnv
  (adtConstructorsMap, _) <- case scrutineeType of
    TAlgebraicData name -> do
      adtInfo <- case Map.lookup name adtEnv of
        Nothing -> throwError $ CustomErrorType $ "ADT info not found for type: " <> name
        Just info -> return info
      return (adtConstructors adtInfo, clauses)
    _ -> return (Map.empty, clauses)

  -- Pass mExpectedMatchType (expected type for branches) to processMatchClause
  (firstBranchTypeOpt, totalClauseEffects) <-
    foldM (processMatchClause inferTypeFunc adtConstructorsMap scrutineeType mExpectedRetType) (Nothing, Set.empty) clauses

  finalMatchExprType <- case firstBranchTypeOpt of
    Nothing -> throwError $ CustomErrorType "Could not determine type of match expression (possibly empty or unresolvable clauses)"
    Just t -> return t

  -- Return match function

  return (TFunction scrutineeType (finalMatchExprType, totalClauseEffects), Set.empty)