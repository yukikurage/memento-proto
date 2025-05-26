{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker.Monad (
  TypeCheck,
  TypeState (..), -- Exporting fields for potential direct access if ever needed
  initialState,
  getEnv,
  addBinding,
  withBinding,
  withBindings,
  getAdtEnv,
  addAdtInfo,
  getEffectEnv,
  addEffectInfo,
  getOperatorEnv,
  addOperatorInfo,
  -- Exported functions moved from Types.hs
  resolveType,
  unify,
  unifyOnlyType,
  isSubEffects,
  extractConstructorType,
  extractHandlerType,
  buildConstructorType,
) where

import Control.Monad (forM_, unless, when) -- Added for resolveType, unify
import Control.Monad.Except (ExceptT, throwError) -- throwError used by moved functions
import Control.Monad.State (State, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set -- Added for isSubEffects, buildConstructorType
import Data.Text (Text)
import qualified Data.Text as T -- Added for resolveType, extractConstructorType etc.
import Language.Memento.Syntax (Effect (..), Effects, Type (..), TypeError (..))
import Language.Memento.TypeChecker.Types (AdtInfo, ConstructorSignature, EffectInfo, OperatorSignature)

-- | Type checking state
data TypeState = TypeState
  { tsEnv :: Map Text Type -- Type environment for variables and constructors
  , tsAdtEnv :: Map Text AdtInfo -- Environment for ADT definitions
  , tsEffectEnv :: Map Text EffectInfo -- Environment for Effect definitions
  , tsOperatorEnv :: Map Text OperatorSignature -- Environment for Operator definitions
  }

-- | Type checking monad: Error handling + State (for environment)
type TypeCheck a = ExceptT TypeError (State TypeState) a

-- | Initial state for type checking
initialState :: TypeState
initialState = TypeState{tsEnv = Map.empty, tsAdtEnv = Map.empty, tsEffectEnv = Map.empty, tsOperatorEnv = Map.empty}

-- | Get the current type environment for variables and constructors
getEnv :: TypeCheck (Map Text Type)
getEnv = gets tsEnv

-- | Add a binding to the variable/constructor type environment
addBinding :: Text -> Type -> TypeCheck ()
addBinding name typ = modify $ \st -> st{tsEnv = Map.insert name typ (tsEnv st)}

-- | Add an ADT definition to the ADT environment
addAdtInfo :: Text -> AdtInfo -> TypeCheck ()
addAdtInfo name adtInfo = modify $ \st -> st{tsAdtEnv = Map.insert name adtInfo (tsAdtEnv st)}

-- | Get the current ADT environment
getAdtEnv :: TypeCheck (Map Text AdtInfo)
getAdtEnv = gets tsAdtEnv

-- | Get the current effect environment
getEffectEnv :: TypeCheck (Map Text EffectInfo)
getEffectEnv = gets tsEffectEnv

-- | Add an Effect definition to the effect environment
addEffectInfo :: Text -> EffectInfo -> TypeCheck ()
addEffectInfo name effectInfo = modify $ \st -> st{tsEffectEnv = Map.insert name effectInfo (tsEffectEnv st)}

-- | Get the current operator environment
getOperatorEnv :: TypeCheck (Map Text OperatorSignature)
getOperatorEnv = gets tsOperatorEnv

-- | Add an operator definition to the operator environment
addOperatorInfo :: Text -> OperatorSignature -> TypeCheck ()
addOperatorInfo name operatorInfo = modify $ \st -> st{tsOperatorEnv = Map.insert name operatorInfo (tsOperatorEnv st)}

-- | Run a computation in a temporarily extended environment
withBinding :: Text -> Type -> TypeCheck a -> TypeCheck a
withBinding name typ action = do
  oldEnv <- getEnv
  addBinding name typ
  result <- action
  modify $ \st -> st{tsEnv = oldEnv} -- Restore original environment
  return result

-- | Run a computation in an environment temporarily extended with multiple bindings
withBindings :: [(Text, Type)] -> TypeCheck a -> TypeCheck a
withBindings bindings action = do
  oldEnv <- getEnv
  mapM_ (uncurry addBinding) bindings
  result <- action
  modify $ \st -> st{tsEnv = oldEnv} -- Restore original environment
  return result

-- Functions moved from Language.Memento.TypeChecker.Types

{- | Resolve a type to ensure it refers to known ADTs or primitive types.
currentAdtName is for allowing recursive definitions.
-}
resolveType :: Type -> Maybe Text -> Maybe Text -> TypeCheck ()
resolveType typ currentAdtName currentEffectName = do
  adtEnv <- getAdtEnv
  effectEnv <- getEffectEnv
  case typ of
    TNumber -> return ()
    TBool -> return ()
    THandler (argT, consumedEffects) (retT, generatedEffects) -> do
      forM_ consumedEffects $ \(Effect effect) -> do
        let isCurrent = Just effect == currentEffectName
        unless (isCurrent || Map.member effect effectEnv) $
          throwError $
            CustomErrorType $
              "Undefined effect referenced in function type: " <> T.pack (show effect)
      forM_ generatedEffects $ \(Effect effect) -> do
        let isCurrent = Just effect == currentEffectName
        unless (isCurrent || Map.member effect effectEnv) $
          throwError $
            CustomErrorType $
              "Undefined effect referenced in function type: " <> T.pack (show generatedEffects)
      resolveType argT currentAdtName currentEffectName
      resolveType retT currentAdtName currentEffectName
    TFunction argT (retT, generatedEffects) -> do
      forM_ generatedEffects $ \(Effect effect) -> do
        let isCurrent = Just effect == currentEffectName
        unless (isCurrent || Map.member effect effectEnv) $
          throwError $
            CustomErrorType $
              "Undefined effect referenced in function type: " <> T.pack (show generatedEffects)
      resolveType argT currentAdtName currentEffectName
      resolveType retT currentAdtName currentEffectName
    TAlgebraicData name -> do
      let isCurrent = Just name == currentAdtName
      unless (isCurrent || Map.member name adtEnv) $
        throwError $
          CustomErrorType $
            "Undefined algebraic data type: " <> name
    TTuple types -> do
      forM_ types $ \t -> resolveType t currentAdtName currentEffectName

-- | エフェクトのサブタイピングをチェック
isSubEffects :: Effects -> Effects -> Bool
isSubEffects e1 e2 = e1 `Set.isSubsetOf` e2

{- | Unify two types. Throws TypeMismatch if they are not equal.
   関数型の場合は引数と戻り値の型を再帰的に照合し、エフェクトについてはサブタイピングを適用する
   エフェクトのサブタイピング: S1 ⊂ S2 のとき T with S1 は T with S2 に代入可能
   (左辺) : 実際に宣言された型など、代入先の型
   (右辺) : 推論された型など
-}
unify :: Type -> Type -> TypeCheck ()
unify (THandler (argT1, eff1) (retT1, eff2)) (THandler (argT2, eff3) (retT2, eff4)) = do
  unify argT1 argT2
  unify retT1 retT2
  unless (eff4 `isSubEffects` eff2) $
    throwError $
      EffectMismatch eff4 eff2
  unless (eff1 `isSubEffects` eff3) $
    throwError $
      EffectMismatch eff1 eff3
unify (TFunction argT1 (retT1, eff1)) (TFunction argT2 (retT2, eff2)) = do
  unify argT1 argT2
  unify retT1 retT2
  unless (eff2 `isSubEffects` eff1) $
    throwError $
      EffectMismatch eff2 eff1 -- Corrected based on previous step's notes
unify expected actual =
  Control.Monad.when (expected /= actual) $ -- Explicitly using Control.Monad.when
    throwError $
      TypeMismatch expected actual

unifyOnlyType :: Type -> Type -> TypeCheck ()
unifyOnlyType (THandler (argT1, eff1) (retT1, eff2)) (THandler (argT2, eff3) (retT2, eff4)) = do
  unify argT1 argT2
  unify retT1 retT2
unifyOnlyType (TFunction argT1 (retT1, eff1)) (TFunction argT2 (retT2, eff2)) = do
  unify argT1 argT2
  unify retT1 retT2
unifyOnlyType expected actual =
  Control.Monad.when (expected /= actual) $ -- Explicitly using Control.Monad.when
    throwError $
      TypeMismatch expected actual

-- | Helper to build the functional type of a constructor
buildConstructorType :: Type -> Type -> Type
buildConstructorType argType resultType = TFunction argType (resultType, Set.empty)

{- | 逆に Type から引数の型の列と結果の型を取り出す
| 結果の型が分解不可能になるまで繰り返す
-}
extractConstructorType :: Type -> TypeCheck (Type, (Type, Effects))
extractConstructorType typ = case typ of
  TFunction argT (retT, retEffects) -> return (argT, (retT, retEffects))
  _ -> throwError $ CustomErrorType $ "Expected a function type but got: " <> T.pack (show typ)

extractHandlerType :: Type -> TypeCheck ((Type, Effects), (Type, Effects))
extractHandlerType typ = case typ of
  THandler (argT, argEffects) (retT, retEffects) -> return ((argT, argEffects), (retT, retEffects))
  _ -> throwError $ CustomErrorType $ "Expected a handler type but got: " <> T.pack (show typ)
