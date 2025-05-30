{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- ScopedTypeVariables might not be needed here anymore

module Language.Memento.TypeChecker where

import Control.Monad (foldM, forM_, unless) -- `when` might not be needed
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (StateT (runStateT), evalState, execState, modify, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set -- Still needed for typeCheckProgram's inferredBodyEffects check
import Data.Text (Text)
import qualified Data.Text as T -- Still needed for typeCheckProgram's error messages

import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (traceM)
import Language.Memento.Syntax (ArgumentWithMetadata (ArgumentWithMetadata), Definition (..), DefinitionWithMetadata (DefinitionWithMetadata), Effects, Expr (..), Program (..), Type (..), TypeWithMetadata (TypeWithMetadata), VariableWithMetadata (VariableWithMetadata))
import Language.Memento.TypeChecker.Expressions
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Registration
import Language.Memento.TypeChecker.Solver (ConstraintOperator (COEqual, COLessThanOrEqual), TypeSolverError, UnsolvedEffects (UESet), serializeST, solve, typeToSolvedType)
import Language.Memento.TypeChecker.Types

-- | Type check a whole program
typeCheckProgram :: Program -> Either ConstraintCollectorError (Either TypeSolverError ())
typeCheckProgram (Program definitions) =
  let
    -- Phase 1 : Constraint の収集と公理の追加
    goCollectConstraints = do
      -- 公理の追加
      mapM_ registerDefinition definitions

      -- それぞれの val definitin に対して Expr の Constraint を収集 & 型変数を取得
      let
        valDefinitinos =
          mapMaybe
            ( \case
                (DefinitionWithMetadata (ValDef t m e) _) -> Just (t, m, e)
                _ -> Nothing
            )
            definitions

      forM_ valDefinitinos $ \(ArgumentWithMetadata var _, typ, expr) -> do
        (exprType, exprEffect) <- collectExpressionConstraint expr
        -- exprType が typ より小さい事を Constraint に追加
        addCSTypeConstraints (exprType, serializeST (typeToSolvedType typ), COLessThanOrEqual)
        -- exprEffect が empty である事を Constraint に追加
        addCSEffectConstraints (exprEffect, UESet Set.empty, COEqual)

    collectedConstraints = runState (runExceptT goCollectConstraints) initialState
   in
    -- Phase 2 : Constraint の解決
    case collectedConstraints of
      (Left err, _) -> Left err
      (Right (), ConstraintCollectorState{csConstraints = constraints, csPreferences = preferences}) ->
        Right $ solve 100 constraints preferences