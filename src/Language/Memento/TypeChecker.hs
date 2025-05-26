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
import Control.Monad.Except (runExceptT, throwError, liftEither) -- Added liftEither
import Control.Monad.State (evalState) -- Removed modify, not directly used in go anymore
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
import Language.Memento.TypeChecker.InferTypes (TCType(..), EffectK(..), inferHM, unifyPure, applySubst, applySubstEffects) -- Removed unused imports like TypeVar, Constraint etc. for this file
import Language.Memento.TypeChecker.TypeConversion (typeToTCType, tcTypeToType) -- Added TypeConversion import

{- | Type check a single expression (e.g., for testing or REPL)
This function is simplified as tsEffects is not part of TypeState for accumulation here.
TODO: This will need updating to use inferHM or a similar TCType-based mechanism.
-}
typeCheck :: Expr -> Maybe Type -> Either TypeError (Type, Effects)
typeCheck expr mExpectedType = evalState (runExceptT (inferType expr mExpectedType)) initialState

-- | Type check a whole program
typeCheckProgram :: Program -> Either TypeError (Map Text Type) -- Return type is Syntax.Type
typeCheckProgram (Program definitions) = evalState (runExceptT go) initialState
 where
  go :: TypeCheck (Map Text Type) -- Return type is Syntax.Type
  go = do
    -- Pass 1: Register ADTs and their constructors
    registerAdtsAndConstructors definitions

    -- Pass 2: Register Effects and their operator signatures
    registerEffects definitions

    -- Pass 3: Type check ValDef bodies and populate environment.
    -- The environment is built progressively by checkValDefBody.
    -- `tsEnv` in TypeState now stores TCType.
    checkedValDefsData <- foldM checkValDefBody [] definitions

    return $ Map.fromList checkedValDefsData

  -- collectValDefTypes is removed.

  -- Accumulator and return type are Syntax.Type for the final map.
  checkValDefBody :: [(Text, Type)] -> Definition -> TypeCheck [(Text, Type)]
  checkValDefBody acc def = case def of
    ValDef name declSyntaxType exprBody -> do
      -- 1. Infer TCType using inferHM
      (inferredTcType, inferredEffects) <- inferHM exprBody

      -- 2. Check for effects in top-level val definitions
      case inferredEffects of
        EffectsConcrete s | Set.null s -> return ()
        EffectsVar _ -> throwError $ CustomErrorType $
            "In definition '" <> name <> "': effect variables are not allowed for top-level definitions, got " <> T.pack (show inferredEffects)
        _ -> throwError $ CustomErrorType $
            "In definition '" <> name <> "': effects are not allowed for top-level definitions but got " <> T.pack (show inferredEffects)

      -- 3. Convert declared Syntax.Type to TCType for unification
      let declaredTcType = typeToTCType declSyntaxType

      -- 4. Unify inferred TCType with the declared TCType
      unificationSubst <- liftEither $ unifyPure inferredTcType declaredTcType

      -- 5. Apply the substitution from unification to the inferred TCType
      let finalTcType = applySubst unificationSubst inferredTcType
      
      -- 6. Update the environment with the precise TCType (finalTcType)
      -- This allows subsequent definitions to see the inferred type.
      addBinding name finalTcType -- addBinding stores TCType in tsEnv

      -- 7. Convert final TCType back to Syntax.Type for the result list
      convertedSyntaxType <- tcTypeToType finalTcType
      return $ (name, convertedSyntaxType) : acc
      
    AdtDef {} -> return acc -- Handled by registerAdtsAndConstructors
    EffectDef {} -> return acc -- Handled by registerEffects
    OperatorDef {} -> return acc -- Handled by registerEffects