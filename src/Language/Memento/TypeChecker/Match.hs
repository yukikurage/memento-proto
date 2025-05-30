{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Match where

import Control.Monad (foldM, unless, when, zipWithM)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Patterns
import Language.Memento.TypeChecker.Solver (ConstraintOperator (COEqual, COGreaterThanOrEqual), Preference (Maximize, Minimize), UnsolvedEffects (UEVar), UnsolvedEffectsVariable (UnsolvedEffectsVariable), UnsolvedType (UTFunction, UTVar), UnsolvedTypeVariable (UnsolvedTypeVariable))
import Language.Memento.TypeChecker.Types

{- | Pattern の型制約を習得し環境に追加する。さらに変数と導入された型変数のマッピングを返す
| 導入される型変数が Maxbound を好むか Minbound を好むかを選ぶ事ができる
| 返り値は (パターン自身の型変数, 変数と導入された型変数のマッピング)
| Pattern は少しややこしい。なぜなら……
| (パターンマッチできる A と C に関して、) A -> B と C -> D という関数を用意して分岐させると、A|C -> B|D という関数を得る事ができる
| ここでこの関数と X -> Y という関数との間に Constraint を作りたいとする (X -> Y が A|C -> B|D の部分型であることを Constraint で表現したい)
| 引数に対して反変なので X > A|C を言いたい (それと Y < B|D)
| しかしながら、 X > A, かつ X > C は X > A & C になってしまう
| 型ユニオンがない今回の体系では、 X > A|C を表現できない
-}
collectMatchConstraint ::
  (ExprWithMetadata -> ConstraintCollectorM (UnsolvedTypeVariable, UnsolvedEffectsVariable)) -> -- ボディの型制約を取得する関数
  [ClauseWithMetadata] -> -- Clause の集合
  ExprMetadata -> -- Match 式のメタデータ
  UnsolvedTypeVariable -> -- これに一致させる
  ConstraintCollectorM ()
collectMatchConstraint collectBodyConstraint clauses (ExprMetadata _ uniqueId) typeVariable = do
  -- とりあえず網羅性チェックは行わない
  -- それぞれの Clause について、パターンとボディの型制約を取得する
  unsolvedTypeVariableOfPatternAndBody <-
    mapM
      ( \(ClauseWithMetadata (Clause patternWithMetadata bodyWithMetadata) _) -> do
          -- パターンの型引数を取得
          (typeVariableOfPattern, bindingsOfPattern) <- collectPatternConstraintAndGetBindings patternWithMetadata
          -- ボディの型引数を取得
          (typeVariableOfBody, typeVariableOfEffect) <- withBinds bindingsOfPattern $ collectBodyConstraint bodyWithMetadata
          return (typeVariableOfPattern, typeVariableOfBody, typeVariableOfEffect)
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
  let
    -- Match 全体をこれに一致させる (split 的な感じ)
    constraintFunc =
      ( typeVariable
      , UTFunction (UTVar typeVariableOfArg) (UTVar typeVariableOfRet, UEVar typeVariableOfEffect)
      , COEqual
      )
  addCSTypeConstraints constraintFunc
  let
    -- TODO : arg の制約は前述の通り作りにくい　とりあえず網羅性は飛ばし、(部分型が今はないので) 全て同じ型を持つことを強要する
    constraintsArg =
      map
        ( \(typeVariableOfPattern, _, _) ->
            (typeVariableOfArg, UTVar typeVariableOfPattern, COEqual)
        )
        unsolvedTypeVariableOfPatternAndBody
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
  mapM_ addCSTypeConstraints constraintsArg
  mapM_ addCSTypeConstraints constraintsRet
  mapM_ addCSEffectConstraints constraintsEffect
