{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker
  ( typeCheck, -- For individual expressions
    typeCheckProgram, -- For whole programs
    TypeError (..), -- Re-export
  )
where

import Control.Monad (unless, forM, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, gets, modify, evalState) -- Added evalState
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
  ( BinOp (..),
    Definition (..),
    Effect (..),
    Effects,
    Expr (..),
    Program (..),
    Type (..),
    TypeError (..), -- Make sure CustomErrorType is included by previous step
  )

-- | Type checking monad: Error handling + State (for environment)
type TypeCheck a = ExceptT TypeError (State TypeState) a

-- | Type checking state: primarily the type environment.
-- tsEffects is not used for global accumulation across definitions in typeCheckProgram.
-- It's used by inferType locally if needed, but inferType now returns effects directly.
data TypeState = TypeState
  { tsEnv :: Map Text Type -- Type environment for variables (local and top-level)
  -- tsEffects :: Effects -- This was used for accumulating effects globally, now handled differently
  }

-- | Initial state for type checking
initialState :: TypeState
initialState = TypeState {tsEnv = Map.empty}

-- | Get the current type environment
getEnv :: TypeCheck (Map Text Type)
getEnv = gets tsEnv

-- | Add a binding to the type environment
addBinding :: Text -> Type -> TypeCheck ()
addBinding name typ = modify $ \st -> st {tsEnv = Map.insert name typ (tsEnv st)}

-- | Run a computation in a temporarily extended environment
withBinding :: Text -> Type -> TypeCheck a -> TypeCheck a
withBinding name typ action = do
  oldEnv <- getEnv
  addBinding name typ
  result <- action
  modify $ \st -> st {tsEnv = oldEnv} -- Restore original environment
  return result

-- | Effect operations mapping (name, (arg_type, return_type, effect_produced))
-- This remains useful for the 'Do' expression.
effectOps :: [(Text, (Type, Type, Effect))]
effectOps = [("throw", (TNumber, TNumber, Throw))] -- Example, can be expanded

-- | Lookup an effect operation by its name
lookupEffectOp :: Text -> Maybe (Type, Type, Effect)
lookupEffectOp name = lookup name effectOps

-- | Unify two types. Throws TypeMismatch if they are not equal.
-- Effects in TFunction are part of the type, so they must match if present.
unify :: Type -> Type -> TypeCheck ()
unify t1 t2 =
  when (t1 /= t2) $ -- This includes effects in TFunction due to derived Eq for Type
    throwError $ TypeMismatch t1 t2
    -- Note: The original unify had specific logic for TFunction.
    -- The derived Eq for Type should handle TFunction comparison correctly,
    -- including comparing the effects Set within TFunction.
    -- If more nuanced unification (like subtyping or ignoring effects) is needed,
    -- this would need to be more complex. For now, exact match is assumed as per plan.

-- | Type check a single expression (e.g., for testing or REPL)
-- This function is simplified as tsEffects is not part of TypeState for accumulation here.
typeCheck :: Expr -> Either TypeError (Type, Effects)
typeCheck expr = evalState (runExceptT (inferType expr)) initialState

-- | Infer the type and effects of an expression.
-- Effects are accumulated and returned alongside the type.
inferType :: Expr -> TypeCheck (Type, Effects)
inferType expr = case expr of
  Number _ -> return (TNumber, Set.empty)
  Bool _ -> return (TBool, Set.empty)
  Var name -> do
    env <- gets tsEnv
    case Map.lookup name env of
      Nothing -> throwError $ UnboundVariable name
      Just t -> return (t, Set.empty)
  BinOp op e1 e2 -> do
    (t1, eff1) <- inferType e1
    (t2, eff2) <- inferType e2
    let currentEffects = eff1 `Set.union` eff2
    resType <- case op of
      Add -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
      Sub -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
      Mul -> unify TNumber t1 >> unify TNumber t2 >> return TNumber
      Div -> unify TNumber t1 >> unify TNumber t2 >> return TNumber -- Potential ZeroDiv effect not handled here yet
      Eq -> unify t1 t2 >> return TBool
      Lt -> unify TNumber t1 >> unify TNumber t2 >> return TBool
      Gt -> unify TNumber t1 >> unify TNumber t2 >> return TBool
    return (resType, currentEffects)
  If cond th el -> do
    (condType, condEff) <- inferType cond
    unify TBool condType
    (thenType, thenEff) <- inferType th
    (elseType, elseEff) <- inferType el
    unify thenType elseType
    return (thenType, condEff `Set.union` thenEff `Set.union` elseEff)
  Lambda name mType body -> do
    -- For `mType`: If Nothing, we previously defaulted to TNumber.
    -- The plan suggests: `let actualParamType = fromMaybe TNumber mType`
    -- Or throw `CannotInferType expr`. Let's stick to `fromMaybe TNumber mType` for now.
    let actualParamType = fromMaybe TNumber mType
    (bodyType, bodyEffects) <- withBinding name actualParamType $ inferType body
    -- The effects of the lambda expression itself are empty.
    -- The body's effects are part of the function type.
    return (TFunction actualParamType bodyType bodyEffects, Set.empty)
  Apply func arg -> do
    (funcType, funcEffs) <- inferType func
    (argType, argEffs) <- inferType arg
    let accumulatedEffects = funcEffs `Set.union` argEffs
    case funcType of
      TFunction paramT retT funBodyEffs -> do
        unify paramT argType
        -- When a function is applied, its declared effects (funBodyEffs) are incurred.
        return (retT, accumulatedEffects `Set.union` funBodyEffs)
      _ -> throwError $ TypeMismatch (TFunction argType (error "Cannot construct expected type for error reporting easily") Set.empty) funcType
  Do name -> do
    case lookupEffectOp name of
      Just (argT, retT, effect) ->
        -- The 'Do' expression itself has no effects when evaluated to a function,
        -- the effect is part of the function type it represents.
        return (TFunction argT retT (Set.singleton effect), Set.empty)
      Nothing -> throwError $ UndefinedEffect name

-- | Type check a whole program (a list of definitions).
-- Returns a map of definition names to their types and declared effects if successful.
typeCheckProgram :: Program -> Either TypeError (Map.Map Text (Type, Effects))
typeCheckProgram (Program definitions) = evalState (runExceptT go) initialState
  where
    go :: TypeCheck (Map.Map Text (Type, Effects)) -- TypeCheck is ExceptT TypeError (State TypeState)
    go = do
      -- Pass 1: Populate environment with declared types of all definitions for mutual recursion.
      -- Effects are not stored in this env, only types.
      let declaredTypesEnv = Map.fromList $ map (\(ValDef name typ _ _) -> (name, typ)) definitions
      modify $ \st -> st {tsEnv = declaredTypesEnv}

      -- Pass 2: Type check each definition's body.
      checkedDefsData <- forM definitions $ \(ValDef name declType declEffects exprBody) -> do
        -- Infer the type and effects of the expression body.
        -- The environment (tsEnv) already contains all top-level declarations from Pass 1.
        (inferredBodyType, inferredBodyEffects) <- inferType exprBody

        -- Check 1: The inferred type of the body must match the declared type.
        unify declType inferredBodyType

        -- Check 2: The inferred effects from the body must be a subset of the declared effects.
        unless (inferredBodyEffects `Set.isSubsetOf` declEffects) $
          throwError $ CustomErrorType $ T.pack $
            "In definition '" <> T.unpack name <> "': expression effects " <> show inferredBodyEffects <>
            " are not a subset of declared effects " <> show declEffects

        -- If both checks pass, return the name with its declared type and effects.
        return (name, (declType, declEffects))

      return $ Map.fromList checkedDefsData
