{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker.Monad where

{-
import Control.Monad (forM_, unless, when) -- Added for resolveType, unify
import Control.Monad.Except (ExceptT, throwError) -- throwError used by moved functions
import Control.Monad.State (State, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set -- Added for isSubEffects, buildConstructorType
import Data.Text (Text)
import qualified Data.Text as T -- Added for resolveType, extractConstructorType etc.
import Language.Memento.Syntax (Effect (..), Effects, Type (..), TypeVariable, TypeVariableWithMetadata, TypeWithMetadata, Variable)
import Language.Memento.TypeChecker.Solver (Constraints (Constraints, effectConstraints, typeConstraints), EffectConstraint, Preference, Preferences (Preferences, effectPreferences, typePreferences), SolvedType, TypeConstraint, UnsolvedEffectsVariable, UnsolvedType, UnsolvedTypeVariable)
import Language.Memento.TypeChecker.Types (AdtInfo, ConstructorSignature, EffectInfo, OperatorSignature)

data ConstructorInfo = ConstructorInfo
  { ciArgmentType :: TypeWithMetadata
  , ciReturnType :: TypeWithMetadata
  , ciDataType :: TypeVariable
  }

data OperatorInfo = OperatorInfo
  { oiArgumentType :: TypeWithMetadata
  , oiReturnType :: TypeWithMetadata
  , oiEffect :: TypeVariable
  }

-- | Type checking state
data ConstraintCollectorState = ConstraintCollectorState
  { csEnv :: Map Variable UnsolvedTypeVariable -- Type environment for variables and constructors, operators
  , csConstraints :: Constraints
  , csPreferences :: Preferences
  , csConstructors :: Map Variable ConstructorInfo
  , csOperators :: Map Variable OperatorInfo
  }

data ConstraintCollectorError
  = UnboundVariable Variable
  | UnboundConstructor Variable
  | UnboundOperator Variable
  | InvalidConstructorType TypeWithMetadata
  | InvalidOperatorType TypeWithMetadata
  deriving (Show, Eq)

-- | Type checking monad: Error handling + State (for environment)
type ConstraintCollectorM a = ExceptT ConstraintCollectorError (State ConstraintCollectorState) a

-- | Initial state for type checking
initialState :: ConstraintCollectorState
initialState =
  ConstraintCollectorState
    { csEnv = Map.empty
    , csConstraints = Constraints [] []
    , csPreferences = Preferences Map.empty Map.empty
    , csConstructors = Map.empty
    , csOperators = Map.empty
    }

-- | Get the current type environment for variables and constructors
getEnv :: ConstraintCollectorM (Map Variable UnsolvedTypeVariable)
getEnv = gets csEnv

withBinds :: Map Variable UnsolvedTypeVariable -> ConstraintCollectorM a -> ConstraintCollectorM a
withBinds binds action = do
  oldEnv <- gets csEnv
  modify $ \st -> st{csEnv = Map.union binds oldEnv}
  result <- action
  modify $ \st -> st{csEnv = oldEnv}
  return result

addEnv :: Map Variable UnsolvedTypeVariable -> ConstraintCollectorM ()
addEnv binds = modify $ \st -> st{csEnv = Map.union binds (csEnv st)}

addEnvSingleton :: Variable -> UnsolvedTypeVariable -> ConstraintCollectorM ()
addEnvSingleton var typ = modify $ \st -> st{csEnv = Map.insert var typ (csEnv st)}

-- | Add a type preference to the type environment
addTypePreference :: UnsolvedTypeVariable -> Preference -> ConstraintCollectorM ()
addTypePreference name pref = modify $ \st ->
  st
    { csPreferences =
        Preferences
          (Map.insert name pref (typePreferences $ csPreferences st))
          (effectPreferences $ csPreferences st)
    }

-- | Add an effect preference to the effect environment
addEffectPreference :: UnsolvedEffectsVariable -> Preference -> ConstraintCollectorM ()
addEffectPreference name pref = modify $ \st ->
  st
    { csPreferences =
        Preferences
          (typePreferences $ csPreferences st)
          (Map.insert name pref (effectPreferences $ csPreferences st))
    }

addCSEnv :: Variable -> UnsolvedTypeVariable -> ConstraintCollectorM ()
addCSEnv var typ = modify $ \st -> st{csEnv = Map.insert var typ (csEnv st)}

addCSTypeConstraints :: TypeConstraint -> ConstraintCollectorM ()
addCSTypeConstraints c = modify $ \st@ConstraintCollectorState{csConstraints = cs} -> st{csConstraints = cs{typeConstraints = c : typeConstraints cs}}

addCSEffectConstraints :: EffectConstraint -> ConstraintCollectorM ()
addCSEffectConstraints c = modify $ \st@ConstraintCollectorState{csConstraints = cs} -> st{csConstraints = cs{effectConstraints = c : effectConstraints cs}}

addCSConstructor :: Variable -> ConstructorInfo -> ConstraintCollectorM ()
addCSConstructor constructor dataTy = modify $ \st@ConstraintCollectorState{csConstructors = cs} -> st{csConstructors = Map.insert constructor dataTy (csConstructors st)}

addCSOperator :: Variable -> OperatorInfo -> ConstraintCollectorM ()
addCSOperator operator effect = modify $ \st@ConstraintCollectorState{csOperators = cs} -> st{csOperators = Map.insert operator effect (csOperators st)}

getCSConstructorInfo :: Variable -> ConstraintCollectorM (Maybe ConstructorInfo)
getCSConstructorInfo constructor = gets $ \st -> Map.lookup constructor (csConstructors st)

getCSOperatorInfo :: Variable -> ConstraintCollectorM (Maybe OperatorInfo)
getCSOperatorInfo operator = gets $ \st -> Map.lookup operator (csOperators st)

-}