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
  addOperatorInfo
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (State, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Language.Memento.Syntax (Type, TypeError)
import Language.Memento.TypeChecker.Types (AdtInfo, EffectInfo, OperatorSignature)

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
