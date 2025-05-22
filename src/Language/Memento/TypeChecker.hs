{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker (
  typeCheck, -- For individual expressions
  typeCheckProgram, -- For whole programs
  TypeError (..), -- Re-export
)
where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, gets, modify) -- Added evalState
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax (
  BinOp (..),
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

{- | Type checking state: primarily the type environment.
tsEffects is not used for global accumulation across definitions in typeCheckProgram.
It's used by inferType locally if needed, but inferType now returns effects directly.
-}
data TypeState = TypeState
  { tsEnv :: Map Text Type -- Type environment for variables (local and top-level)
  -- tsEffects :: Effects -- This was used for accumulating effects globally, now handled differently
  }

-- | Initial state for type checking
initialState :: TypeState
initialState = TypeState{tsEnv = Map.empty}

-- | Get the current type environment
getEnv :: TypeCheck (Map Text Type)
getEnv = gets tsEnv

-- | Add a binding to the type environment
addBinding :: Text -> Type -> TypeCheck ()
addBinding name typ = modify $ \st -> st{tsEnv = Map.insert name typ (tsEnv st)}

-- | Run a computation in a temporarily extended environment
withBinding :: Text -> Type -> TypeCheck a -> TypeCheck a
withBinding name typ action = do
  oldEnv <- getEnv
  addBinding name typ
  result <- action
  modify $ \st -> st{tsEnv = oldEnv} -- Restore original environment
  return result

{- | Effect operations mapping (name, (arg_type, return_type, effect_produced))
This remains useful for the 'Do' expression.
-}
effectOps :: [(Text, (Type, Type, Effect))]
effectOps = [("throw", (TNumber, TNumber, Throw))] -- Example, can be expanded

-- | Lookup an effect operation by its name
lookupEffectOp :: Text -> Maybe (Type, Type, Effect)
lookupEffectOp name = lookup name effectOps

{- | エフェクトのサブタイピングをチェック
   S1 ⊂ S2 のとき S1 サブタイプ S2
-}
isSubEffects :: Effects -> Effects -> Bool
isSubEffects e1 e2 = e1 `Set.isSubsetOf` e2

{- | Unify two types. Throws TypeMismatch if they are not equal.
   関数型の場合は引数と戻り値の型を再帰的に照合し、エフェクトについてはサブタイピングを適用する
   エフェクトのサブタイピング: S1 ⊂ S2 のとき T with S1 は T with S2 に代入可能
-}
unify :: Type -> Type -> TypeCheck ()
unify (TFunction argT1 retT1 eff1) (TFunction argT2 retT2 eff2) = do
  -- 引数の型と戻り値の型は通常通り単一化する
  unify argT1 argT2
  unify retT1 retT2

  -- エフェクトのサブタイピング: eff1 ⊆ eff2 であれば OK
  -- 実際の型のエフェクトが期待される型のサブセットであれば互換性あり
  unless (eff1 `isSubEffects` eff2) $
    throwError $
      EffectMismatch eff1 eff2
unify expected actual =
  when (expected /= actual) $
    throwError $
      TypeMismatch expected actual

{- | Type check a single expression (e.g., for testing or REPL)
This function is simplified as tsEffects is not part of TypeState for accumulation here.
-}
typeCheck :: Expr -> Either TypeError (Type, Effects)
typeCheck expr = evalState (runExceptT (inferType expr)) initialState

{- | Infer the type and effects of an expression.
Effects are accumulated and returned alongside the type.
-}
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

{- | Type check a whole program (a list of definitions).
Returns a map of definition names to their types if successful.
-}
typeCheckProgram :: Program -> Either TypeError (Map.Map Text Type)
typeCheckProgram (Program definitions) = evalState (runExceptT go) initialState
 where
  go :: TypeCheck (Map.Map Text Type) -- TypeCheck is ExceptT TypeError (State TypeState)
  go = do
    -- Pass 1: Populate environment with declared types of all definitions for mutual recursion.
    let declaredTypesEnv = Map.fromList $ map (\(ValDef name typ _) -> (name, typ)) definitions
    modify $ \st -> st{tsEnv = declaredTypesEnv}

    -- Pass 2: Type check each definition's body.
    checkedDefsData <- forM definitions $ \(ValDef name declType exprBody) -> do
      -- Infer the type and effects of the expression body.
      -- The environment (tsEnv) already contains all top-level declarations from Pass 1.
      (inferredBodyType, inferredBodyEffects) <- inferType exprBody

      -- Check 1: The inferred type of the body must match the declared type.
      -- 型の検証（エフェクトのサブタイピングも考慮）
      unify declType inferredBodyType

      -- Check 2: トップレベルではエフェクトは許可されません
      unless (Set.null inferredBodyEffects) $
        throwError $
          CustomErrorType $
            T.pack $
              "In definition '"
                <> T.unpack name
                <> "': effects are not allowed for top-level definitions"
                <> " but got "
                <> show inferredBodyEffects

      -- If both checks pass, return the name with its declared type.
      return (name, declType)

    return $ Map.fromList checkedDefsData
