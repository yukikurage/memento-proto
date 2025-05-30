{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Handle where

import Control.Monad (foldM, unless, when)
import Control.Monad.Except (throwError)
import qualified Data.List as List (partition)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Patterns (collectPatternConstraintAndGetBindings)
import Language.Memento.TypeChecker.Solver (ConstraintOperator (COEqual, COGreaterThanOrEqual), Preference (Maximize, Minimize), UnsolvedEffects (UEVar), UnsolvedEffectsVariable (UnsolvedEffectsVariable), UnsolvedType (UTFunction, UTHandler, UTVar), UnsolvedTypeVariable (UnsolvedTypeVariable))
import Language.Memento.TypeChecker.Types

-- | Match と同様に網羅性は飛ばす
collectHandleConstraint ::
  (ExprWithMetadata -> ConstraintCollectorM (UnsolvedTypeVariable, UnsolvedEffectsVariable)) -> -- ボディの型制約を取得する関数
  [HandlerClauseWithMetadata] -> -- Clause の集合
  ExprMetadata -> -- handle 式のメタデータ
  UnsolvedTypeVariable -> -- これに一致させる
  ConstraintCollectorM ()
collectHandleConstraint collectBodyConstraint clauses (ExprMetadata _ uniqueId) typeVariable = do
  -- 分解する
  let
    typeVariableOfArg = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_arg"
    typeVariableOfArgEffect = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_arg_effect" -- Handler は Match と違ってここに Effect がある
    typeVariableOfRet = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_ret"
    typeVariableOfRetEffect = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_ret_effect"
  addTypePreference typeVariableOfArg Maximize -- Arg はなるべく大きくなるように取りたい (反変なのでそちらの方が都合が良い)
  addEffectPreference typeVariableOfArgEffect Maximize -- ハンドル対象のエフェクトはなるべく大きく取る
  addTypePreference typeVariableOfRet Minimize -- Ret はなるべく小さくなるように取りたい
  addEffectPreference typeVariableOfRetEffect Minimize -- Effect はなるべく小さくなるように取りたい
  let
    -- Handle 全体をこれに一致させる (split 的な感じ)
    constraintFunc =
      ( typeVariable
      , UTHandler (UTVar typeVariableOfArg, UEVar typeVariableOfArgEffect) (UTVar typeVariableOfRet, UEVar typeVariableOfRetEffect)
      , COEqual
      )
  addCSTypeConstraints constraintFunc
  -- とりあえず網羅性チェックは行わない
  -- それぞれの Clause について、パターンとボディの型制約を取得する
  unsolvedTypeVariableOfPatternAndBody <-
    mapM
      ( \case
          ( HandlerClauseWithMetadata
              ( HandlerClause
                  (TypeVariableWithMetadata opName _)
                  argVar
                  kVar
                  bodyWithMetadata
                )
              (HandlerClauseMetadata _ uniqueId)
            ) -> do
              let
                -- それぞれの引数用の TypeVariable を導入
                argTypeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_arg" -- オペレーションに渡される引数
                argEffectVariable = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_handle_effect" -- オペレーションが発火するエフェクト
                kTypeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_k" -- 継続
                kArgTypeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_k_arg" -- 継続の引数
                kBodyTypeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_k_body" -- 継続の本体の型
                kEffectVariable = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_k_effect" -- 継続が発火すると期待されるエフェクト
                bodyTypeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_body" -- 本体の型
                bodyEffectVariable = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_body_effect" -- 本体が発火するエフェクト
              addTypePreference argTypeVariable Maximize
              addEffectPreference argEffectVariable Maximize
              addTypePreference kTypeVariable Minimize
              addTypePreference kArgTypeVariable Maximize
              addTypePreference kBodyTypeVariable Minimize
              addEffectPreference kEffectVariable Maximize
              addEffectPreference bodyEffectVariable Minimize
              addEffectPreference argEffectVariable Maximize
          (HandlerClauseWithMetadata (HandlerReturnClause retVar bodyWithMetadata) (HandlerClauseMetadata _ uniqueId)) -> undefined
      )
      clauses
  -- 今回はパターンの型は全部同じものを持つと仮定する
  let
    typeVariableOfArg = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_arg"
    typeVariableOfRet = UnsolvedTypeVariable $ T.pack $ show uniqueId <> "_ret"
    typeVariableOfEffect = UnsolvedEffectsVariable $ T.pack $ show uniqueId <> "_effect"
  addTypePreference typeVariableOfArg Maximize -- Arg はなるべく大きくなるように取りたい (反変なのでそちらの方が都合が良い)
  addTypePreference typeVariableOfRet Minimize -- Ret はなるべく小さくなるように取りたい
  addEffectPreference typeVariableOfEffect Minimize -- Effect はなるべく小さくなるように取りたい
  {-
  let
    -- Match 全体をこれに一致させる (split 的な感じ)
    constraintFunc =
      ( typeVariable
      , UTFunction (UTVar typeVariableOfArg) (UTVar typeVariableOfRet, UEVar typeVariableOfEffect)
      , COEqual
      )
    -- TODO : arg の制約は前述の通り作りにくい　とりあえず網羅性は飛ばす
    -- ret の制約 : typeVariableOfRet がそれぞれの Clause の ret よりも大きい
    constraintsRet =
      map
        ( \(_, typeVariableOfBody, _) ->
            (typeVariableOfRet, UTVar typeVariableOfBody, COGreaterThanOrEqual)
        )
        unsolvedTypeVariableOfPatternAndBody
    -- effect も同様
    constraintsEffect =
      map
        ( \(_, _, typeVariableOfEffect) ->
            (typeVariableOfEffect, UEVar typeVariableOfEffect, COGreaterThanOrEqual)
        )
        unsolvedTypeVariableOfPatternAndBody
  addCSTypeConstraints constraintFunc
  mapM_ addCSTypeConstraints constraintsRet
  mapM_ addCSEffectConstraints constraintsEffect
  -}