{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.TypeChecker.TypeConversion (
  typeToTCType,
  tcTypeToType,
  effectKToEffects -- Exporting for potential direct use elsewhere
) where

import qualified Data.Set as Set
import Data.Text (Text, pack)
import Control.Monad.Except (throwError)

import Language.Memento.Syntax (Type(..), Effect(..), Effects, Pattern(..), TypeError(..))
import Language.Memento.TypeChecker.InferTypes (TCType(..), TypeVar(..), EffectK(..), Bound(..), EffectVar(..))
import Language.Memento.TypeChecker.Monad (TypeCheck) -- For tcTypeToType's error throwing

-- Convert Syntax.Type to InferTypes.TCType
typeToTCType :: Type -> TCType
typeToTCType TNumber = TCNum
typeToTCType TBool = TCBool
typeToTCType (TFunction argT (retT, synEffects)) =
  -- Syntax.Type's TFunction has concrete effects.
  -- We represent these as EffectsConcrete in TCType's EffectK.
  TCFun (typeToTCType argT) (typeToTCType retT) (EffectsConcrete synEffects)
typeToTCType (THandler (argT, consumedSynEffects) (retT, producedSynEffects)) =
  TCHandler (typeToTCType argT) (EffectsConcrete consumedSynEffects)
              (typeToTCType retT) (EffectsConcrete producedSynEffects)
typeToTCType (TAlgebraicData name) = TCAlgebraicData name [] -- Assuming TAlgebraicData in TCType doesn't take TCType args for now, or they are empty if not polymorphic.
                                                          -- This matches current TCType definition.
typeToTCType (TTuple types) = TCTuple (map typeToTCType types)


-- Convert InferTypes.TCType back to Syntax.Type
-- This runs in TypeCheck to allow for error throwing (e.g., for uninferrable TCVar)
tcTypeToType :: TCType -> TypeCheck Type
tcTypeToType TCNum = return TNumber
tcTypeToType TCBool = return TBool
tcTypeToType (TCVar (TV varName)) =
  throwError $ CustomErrorType $ "Cannot convert unresolved type variable '" <> varName <> "' to a concrete type."
tcTypeToType (TCFun argTcT retTcT effK) = do
  argSynT <- tcTypeToType argTcT
  retSynT <- tcTypeToType retTcT
  synEffects <- effectKToEffects effK
  return $ TFunction argSynT (retSynT, synEffects)
tcTypeToType (TCHandler argTcT consumedEffK retTcT producedEffK) = do
  argSynT <- tcTypeToType argTcT
  consumedSynEffects <- effectKToEffects consumedEffK
  retSynT <- tcTypeToType retTcT
  producedSynEffects <- effectKToEffects producedEffK
  return $ THandler (argSynT, consumedSynEffects) (retSynT, producedSynEffects)
tcTypeToType (TCAlgebraicData name []) = return $ TAlgebraicData name -- Assuming no type args for now
tcTypeToType (TCAlgebraicData name _args) =
  throwError $ CustomErrorType $ "Cannot convert parameterized ADT '" <> name <> "' to non-polymorphic Syntax.Type yet."
tcTypeToType (TCTuple tcTypes) = TTuple <$> mapM tcTypeToType tcTypes

-- Convert EffectK to Syntax.Effects (Set Effect)
effectKToEffects :: EffectK -> TypeCheck Effects
effectKToEffects (EffectsConcrete s) = return s
effectKToEffects (EffectsVar (EV varName)) =
  throwError $ CustomErrorType $ "Cannot convert unresolved effect variable '" <> varName <> "' to concrete effects."
effectKToEffects (EffectsBound lower upper) =
  -- For conversion back to Syntax.Type, we need a concrete set of effects.
  -- The issue: "エフェクトの範囲が被っていることが大事で、実際のエフェクトを同定する必要はありません"
  -- This implies that for unification, overlap is key. But for the final Type, we need specifics.
  -- Option 1: Use the lower bound if it's finite.
  -- Option 2: Error if not EffectsConcrete.
  -- Let's choose Option 1 for now if lower is finite, otherwise error.
  case lower of
    BoundFinite s -> return s
    BoundOmega    -> throwError $ CustomErrorType "Cannot convert effect bound with Omega lower bound to concrete effects for Syntax.Type."
