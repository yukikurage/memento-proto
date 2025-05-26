{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Patterns (
  checkPatternAndGetBindings,
  checkPatternCoverage,
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
import Language.Memento.TypeChecker.Types

{- | Check a pattern against an expected type (or derive type from annotation),
| and return the bindings and the actual type of the pattern.
-}
checkPatternAndGetBindings :: Pattern -> Maybe Type -> TypeCheck ([(Text, Type)], Type)
checkPatternAndGetBindings pattern mExpectedType = case pattern of
  PVar name -> do
    actualType <- case mExpectedType of
      Just t -> return t
      Nothing -> throwError $ CustomErrorType $ "Cannot infer type for variable pattern '" <> name <> "' without expected type or annotation."
    return ([(name, actualType)], actualType)
  PWildcard -> do
    actualType <- case mExpectedType of
      Just t -> return t
      Nothing -> throwError $ CustomErrorType "Cannot infer type for wildcard pattern '_' without expected type."
    return ([], actualType)
  PNumber n -> do
    case mExpectedType of
      Just TNumber -> return ([], TNumber)
      Just other -> throwError $ TypeMismatch other TNumber
      Nothing -> return ([], TNumber) -- Default to TNumber if no expected type
  PBool b -> do
    case mExpectedType of
      Just TBool -> return ([], TBool)
      Just other -> throwError $ TypeMismatch other TBool
      Nothing -> return ([], TBool) -- Default to TBool if no expected type
  PConstructor consName nestedPattern -> do -- Changed varName to nestedPattern
    adtEnv <- getAdtEnv
    -- Retrieve constructor signature (argType and resultADT)
    -- Assuming getConstructorSignature is available or logic is inlined:
    -- For simplicity, directly look up from tsEnv. A dedicated getConstructorSignature helper would be cleaner.
    globalEnv <- getEnv
    (argType, resultADT@(TAlgebraicData adtName)) <- case Map.lookup consName globalEnv of
      Just (TFunction at rtTuple) -> case rtTuple of
        (rADT@(TAlgebraicData an), _effs) -> return (at, rADT)
        (otherType, _effs) -> throwError $ CustomErrorType $ "Constructor " <> consName <> " does not return an ADT. Got: " <> T.pack (show otherType)
      Just other -> throwError $ CustomErrorType $ "Constructor " <> consName <> " is not a function type in env: " <> T.pack (show other)
      Nothing -> throwError $ UnboundVariable consName

    -- Validate the constructor's ADT type against the expected type for the whole PConstructor pattern
    case mExpectedType of
      Just expectedAdt@(TAlgebraicData expectedAdtName) ->
        unless (adtName == expectedAdtName && resultADT == expectedAdt) $ throwError $ TypeMismatch expectedAdt resultADT
      Just other -> throwError $ TypeMismatch other resultADT
      Nothing -> return ()

    -- Recursively check the nested pattern, expecting the constructor's argument type
    (nestedBindings, nestedPatternType) <- checkPatternAndGetBindings nestedPattern (Just argType)
    unify argType nestedPatternType -- Ensure the nested pattern actually matches the argument type
    return (nestedBindings, resultADT) -- Bindings from nested, overall type is the ADT
  PTuple ps -> do
    case mExpectedType of
      Just (TTuple expectedTypes) -> do
        if length ps /= length expectedTypes
          then throwError $ CustomErrorType $ "Tuple pattern arity mismatch. Expected " <> T.pack (show (length expectedTypes)) <> " elements, got " <> T.pack (show (length ps)) <> "."
          else do
            results <- zipWithM (\p t -> checkPatternAndGetBindings p (Just t)) ps expectedTypes
            let allBindings = concatMap fst results
            -- The actual type of the pattern is the TTuple of expectedTypes
            return (allBindings, TTuple expectedTypes)
      Just other -> throwError $ TypeMismatch other (TTuple []) -- Placeholder for expected tuple type
      Nothing -> do
        -- Infer from sub-patterns, this is complex, requires more thought for full inference
        -- For now, if no expected type, try to infer from sub-patterns. If sub-patterns also don't have annotations, this will fail.
        -- A simple approach: If all sub-patterns successfully infer their types, combine them.
        inferredSubTypesAndBindings <- mapM (\p -> checkPatternAndGetBindings p Nothing) ps
        let allBindings = concatMap fst inferredSubTypesAndBindings
        let inferredTypes = map snd inferredSubTypesAndBindings
        return (allBindings, TTuple inferredTypes)

{- | Check if a list of patterns is exhaustive for a given type.
| This is a simplified version and might need enhancement for full ADT checking logic from Match.
-}
checkPatternCoverage :: Type -> [Pattern] -> TypeCheck ()
checkPatternCoverage typ patterns = case typ of
  TBool -> do
    let hasTrue = any (\case PBool True -> True; _ -> False) patterns
    let hasFalse = any (\case PBool False -> True; _ -> False) patterns
    let hasWildcardOrVar = any (\case PVar _ -> True; PWildcard -> True; _ -> False) patterns
    unless (hasWildcardOrVar || (hasTrue && hasFalse)) $
      throwError $
        CustomErrorType "Pattern matching for Boolean is not exhaustive. Need to cover True and False, or use a wildcard/variable."
  TNumber -> do
    -- For numbers, any specific number pattern or a wildcard/variable is usually enough unless ranges are involved.
    -- The original Match expr had a more complex check for TNumber.
    -- For lambda, simple var/wildcard is typical.
    let hasNumberPattern = any (\case PNumber _ -> True; _ -> False) patterns
    let hasWildcardOrVar = any (\case PVar _ -> True; PWildcard -> True; _ -> False) patterns
    unless (hasNumberPattern || hasWildcardOrVar) $
      throwError $
        CustomErrorType "Pattern matching for Number is not exhaustive. Add a number pattern or wildcard/variable."
  TTuple types -> do
    -- Check if there's a PTuple pattern matching the arity or a wildcard/var
    let hasMatchingTuplePattern = any (\case PTuple ps -> length ps == length types; _ -> False) patterns
    let hasWildcardOrVar = any (\case PVar _ -> True; PWildcard -> True; _ -> False) patterns
    unless (hasMatchingTuplePattern || hasWildcardOrVar) $
      throwError $
        CustomErrorType $
          "Pattern matching for Tuple type with " <> T.pack (show (length types)) <> " elements is not exhaustive."
  TAlgebraicData adtName -> do
    adtEnv <- getAdtEnv
    adtInfo <- case Map.lookup adtName adtEnv of
      Just info -> return info
      Nothing -> throwError $ CustomErrorType $ "ADT info not found for type: " <> adtName

    let allAdtConstructors = Map.keysSet (adtConstructors adtInfo)
    let (coveredConstructors, hasWildcardOrVar) =
          foldr
            ( \p (accSet, accBool) -> case p of
                PConstructor cn _nestedPattern -> (Set.insert cn accSet, accBool) -- Correctly ignore nested pattern for this check
                PVar _ -> (accSet, True)
                PWildcard -> (accSet, True)
                _ -> (accSet, accBool)
            )
            (Set.empty, False)
            patterns

    unless hasWildcardOrVar $
      unless (allAdtConstructors `Set.isSubsetOf` coveredConstructors) $
        throwError $
          CustomErrorType $
            "Pattern matching is not exhaustive for ADT '" <> adtName <> "'. Missing constructors: " <> T.pack (show (Set.toList (allAdtConstructors `Set.difference` coveredConstructors)))
  TFunction{} -> throwError $ CustomErrorType "Cannot perform exhaustiveness check on function types."
  THandler{} -> throwError $ CustomErrorType "Cannot perform exhaustiveness check on handler types."

-- Add other types as needed