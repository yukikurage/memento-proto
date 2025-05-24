{-# LANGUAGE OverloadedStrings #-}

-- ScopedTypeVariables might not be needed here anymore

module Language.Memento.TypeChecker (
  typeCheck, -- For individual expressions
  typeCheckProgram, -- For whole programs
  TypeError (..), -- Re-export from Syntax, but commonly associated with TypeChecker
  module Language.Memento.TypeChecker.Monad, -- Re-exporting Monad types/functions
  module Language.Memento.TypeChecker.Types, -- Re-exporting common types/signatures
  module Language.Memento.TypeChecker.Registration,
  module Language.Memento.TypeChecker.Expressions,
)
where

import Control.Monad (foldM, unless) -- `when` might not be needed
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (evalState, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set -- Still needed for typeCheckProgram's inferredBodyEffects check
import Data.Text (Text)
import qualified Data.Text as T -- Still needed for typeCheckProgram's error messages

import Debug.Trace (traceM)
import Language.Memento.Syntax (Definition (..), Effects, Expr (..), Program (..), Type (..), TypeError (..))
import Language.Memento.TypeChecker.Expressions
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Registration
import Language.Memento.TypeChecker.Types

{- | Type check a single expression (e.g., for testing or REPL)
This function is simplified as tsEffects is not part of TypeState for accumulation here.
-}
typeCheck :: Expr -> Either TypeError (Type, Effects)
typeCheck expr = evalState (runExceptT (inferType expr)) initialState

-- | Type check a whole program
typeCheckProgram :: Program -> Either TypeError (Map Text Type)
typeCheckProgram (Program definitions) = evalState (runExceptT go) initialState
 where
  go :: TypeCheck (Map Text Type)
  go = do
    -- Pass 1: Register ADTs and their constructors
    registerAdtsAndConstructors definitions

    -- Pass 2: Register Effects and their operator signatures
    registerEffects definitions

    -- Pass 3: Populate environment with declared types of ValDefs for mutual recursion.
    -- And type check ValDef bodies.
    valDefTypes <- foldM collectValDefTypes Map.empty definitions

    currentEnv <- getEnv
    let combinedEnv = Map.union currentEnv valDefTypes
    -- Modify state using the functions from Monad module indirectly if needed, or directly if exposed.
    -- modify $ \st -> st{tsEnv = combinedEnv}
    -- addBinding/withBinding are for single/scoped additions. For a bulk update like this, direct state modification is typical.
    -- Let's check if `tsEnv` is exported from `TypeState` in `Monad.hs`. It is.
    modify $ \st -> st{tsEnv = combinedEnv}

    -- Check ValDef bodies
    checkedValDefsData <- foldM checkValDefBody [] definitions

    return $ Map.fromList checkedValDefsData

  collectValDefTypes :: Map Text Type -> Definition -> TypeCheck (Map Text Type)
  collectValDefTypes acc def = case def of
    ValDef name typ _ -> return $ Map.insert name typ acc
    _ -> return acc

  checkValDefBody :: [(Text, Type)] -> Definition -> TypeCheck [(Text, Type)]
  checkValDefBody acc def = case def of
    ValDef name declType exprBody -> do
      (inferredBodyType, inferredBodyEffects) <- inferType exprBody
      unify declType inferredBodyType
      unless (Set.null inferredBodyEffects) $
        throwError $
          CustomErrorType $
            "In definition '" <> name <> "': effects are not allowed for top-level definitions but got " <> T.pack (show inferredBodyEffects)
      return $ (name, declType) : acc
    _ -> return acc