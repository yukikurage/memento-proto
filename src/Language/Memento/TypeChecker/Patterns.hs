{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Patterns where

{-

import Control.Monad (foldM, forM, unless, when, zipWithM)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Solver (ConstraintOperator (COEqual, COGreaterThanOrEqual, COLessThanOrEqual), Preference (Maximize), SolvedType (STAlgebraicData, STBool, STNumber, STTuple), UnsolvedType (UTAlgebraicData, UTNever, UTFunction, UTHandler, UTTuple, UTVar), UnsolvedTypeVariable (UnsolvedTypeVariable), serializeST, typeToSolvedType)
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

-}