{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Patterns where

import Control.Monad (foldM, forM, unless, when, zipWithM)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Solver (ConstraintOperator (COEqual, COGreaterThanOrEqual, COLessThanOrEqual), Preference (Maximize), SolvedType (STAlgebraicData, STBool, STNumber, STTuple), UnsolvedType (UTAlgebraicData, UTBottom, UTFunction, UTHandler, UTTuple, UTVar), UnsolvedTypeVariable (UnsolvedTypeVariable), serializeST, typeToSolvedType)
import Language.Memento.TypeChecker.Types

-- | パターンを表す型変数はパターンがカバーできる範囲になるように solve される
collectPatternConstraintAndGetBindings :: PatternWithMetadata -> ConstraintCollectorM (UnsolvedTypeVariable, Map.Map Variable UnsolvedTypeVariable)
collectPatternConstraintAndGetBindings (PatternWithMetadata pattern (PatternMetadata _ uniqueId)) = do
  let
    typeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId
  addTypePreference typeVariable Maximize -- なるべく大きくなるようにとりたい
  bindings <- case pattern of
    PVar (ArgumentWithMetadata var (ArgumentMetadata _ uniquId)) -> do
      let
        varTypeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId
      addTypePreference varTypeVariable Maximize
      return $ Map.singleton var varTypeVariable
    PWildcard -> pure Map.empty -- ワイルドカードは制約もバインディングも起こさない
    PNumber _ -> do
      -- 制約を追加する
      let
        -- typeVariable は Number より小さく無ければいけない
        constraint = (typeVariable, serializeST STNumber, COLessThanOrEqual)
      addCSTypeConstraints constraint
      return Map.empty
    PBool _ -> do
      -- 制約を追加する
      let
        -- typeVariable は Bool より小さく無ければ行けない
        -- Bool より小さい範囲ならカバー可能
        constraint = (typeVariable, serializeST STBool, COLessThanOrEqual)
      addCSTypeConstraints constraint
      return Map.empty
    PConstructor (VariableWithMetadata consName (VariableMetadata _ uniquId)) nestedPattern -> do
      -- データ型の名前を取得
      mDataTyName <- getCSConstructorInfo consName
      case mDataTyName of
        Just (ConstructorInfo argT retT dataTyName) -> do
          -- 制約を追加する
          let
            -- typeVariable は consName より小さく無ければ行けない
            constraint = (typeVariable, serializeST (STAlgebraicData dataTyName), COLessThanOrEqual)
          addCSTypeConstraints constraint
          (nestedTypeVariable, nestedBindings) <- collectPatternConstraintAndGetBindings nestedPattern
          -- さらに制約を追加する
          let
            -- Return type の制約 (パターンの型は、コンストラクタの戻り値の型より小さい)
            constraintRet = (typeVariable, serializeST (typeToSolvedType retT), COLessThanOrEqual)
            -- Argument type の制約 (ネストパターンがカバーしている範囲が、コンストラクタの引数より大きい)
            constraintArg = (nestedTypeVariable, serializeST (typeToSolvedType argT), COGreaterThanOrEqual)
          addCSTypeConstraints constraintRet
          addCSTypeConstraints constraintArg
          return nestedBindings
        Nothing -> throwError $ UnboundConstructor consName -- Constructor が環境に無い場合
    PTuple patterns -> do
      nestedBindingAndTypes <- forM patterns collectPatternConstraintAndGetBindings
      let
        nestedBindings = Map.unions (map snd nestedBindingAndTypes)
        nestedTypes = map fst nestedBindingAndTypes
        -- 制約 : typeVariable はタプル型より小さい必要がある (nest した型の方が大きい)
        constraint = (typeVariable, UTTuple $ map UTVar nestedTypes, COLessThanOrEqual)
      addCSTypeConstraints constraint
      return nestedBindings
  pure (typeVariable, bindings)

-- checkPatternAndGetBindings :: Pattern -> Maybe Type -> TypeCheck ([(Text, Type)], Type)
-- checkPatternAndGetBindings pattern mExpectedType = case pattern of
--   PVar name -> do
--     actualType <- case mExpectedType of
--       Just t -> return t
--       Nothing -> throwError $ CustomErrorType $ "Cannot infer type for variable pattern '" <> name <> "' without expected type or annotation."
--     return ([(name, actualType)], actualType)
--   PWildcard -> do
--     actualType <- case mExpectedType of
--       Just t -> return t
--       Nothing -> throwError $ CustomErrorType "Cannot infer type for wildcard pattern '_' without expected type."
--     return ([], actualType)
--   PNumber n -> do
--     case mExpectedType of
--       Just TNumber -> return ([], TNumber)
--       Just other -> throwError $ TypeMismatch other TNumber
--       Nothing -> return ([], TNumber) -- Default to TNumber if no expected type
--   PBool b -> do
--     case mExpectedType of
--       Just TBool -> return ([], TBool)
--       Just other -> throwError $ TypeMismatch other TBool
--       Nothing -> return ([], TBool) -- Default to TBool if no expected type
--   PConstructor consName nestedPattern -> do
--     -- Changed varName to nestedPattern
--     adtEnv <- getAdtEnv
--     -- Retrieve constructor signature (argType and resultADT)
--     -- Assuming getConstructorSignature is available or logic is inlined:
--     -- For simplicity, directly look up from tsEnv. A dedicated getConstructorSignature helper would be cleaner.
--     globalEnv <- getEnv
--     (argType, resultADT) <- case Map.lookup consName globalEnv of
--       Just (TFunction at rtTuple) -> case rtTuple of
--         (rADT, _effs) -> return (at, rADT)
--       Just other -> throwError $ CustomErrorType $ "Constructor " <> consName <> " is not a function type in env: " <> T.pack (show other)
--       Nothing -> throwError $ UnboundVariable consName

--     -- Validate the constructor's ADT type against the expected type for the whole PConstructor pattern
--     case mExpectedType of
--       Just expectedAdt@(TAlgebraicData expectedAdtName) ->
--         unless (resultADT == expectedAdt) $ throwError $ TypeMismatch expectedAdt resultADT
--       Just other -> throwError $ TypeMismatch other resultADT
--       Nothing -> return ()

--     -- Recursively check the nested pattern, expecting the constructor's argument type
--     (nestedBindings, nestedPatternType) <- checkPatternAndGetBindings nestedPattern (Just argType)
--     unify argType nestedPatternType -- Ensure the nested pattern actually matches the argument type
--     return (nestedBindings, resultADT) -- Bindings from nested, overall type is the ADT
--   PTuple ps -> do
--     case mExpectedType of
--       Just (TTuple expectedTypes) -> do
--         if length ps /= length expectedTypes
--           then throwError $ CustomErrorType $ "Tuple pattern arity mismatch. Expected " <> T.pack (show (length expectedTypes)) <> " elements, got " <> T.pack (show (length ps)) <> "."
--           else do
--             results <- zipWithM (\p t -> checkPatternAndGetBindings p (Just t)) ps expectedTypes
--             let allBindings = concatMap fst results
--             -- The actual type of the pattern is the TTuple of expectedTypes
--             return (allBindings, TTuple expectedTypes)
--       Just other -> throwError $ TypeMismatch other (TTuple []) -- Placeholder for expected tuple type
--       Nothing -> do
--         -- Infer from sub-patterns, this is complex, requires more thought for full inference
--         -- For now, if no expected type, try to infer from sub-patterns. If sub-patterns also don't have annotations, this will fail.
--         -- A simple approach: If all sub-patterns successfully infer their types, combine them.
--         inferredSubTypesAndBindings <- mapM (\p -> checkPatternAndGetBindings p Nothing) ps
--         let allBindings = concatMap fst inferredSubTypesAndBindings
--         let inferredTypes = map snd inferredSubTypesAndBindings
--         return (allBindings, TTuple inferredTypes)
