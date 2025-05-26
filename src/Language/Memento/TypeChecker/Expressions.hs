{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- May be needed for some existing logic, e.g. in Match or Handle
-- {-# LANGUAGE ScopedTypeVariables #-} -- Likely not needed anymore

module Language.Memento.TypeChecker.Expressions where

-- Removed imports:
-- import Control.Monad (foldM, unless, when, zipWithM)
-- import qualified Data.List as List (partition)
-- import qualified Data.Map as Map
-- import Data.Maybe (fromMaybe, isNothing)
-- import Debug.Trace (traceM)
-- import Language.Memento.TypeChecker.Patterns (checkPatternAndGetBindings, checkPatternCoverage)
-- import Language.Memento.TypeChecker.Types -- Assuming its contents are now covered by InferTypes/TypeConversion or Syntax

import qualified Data.Set as Set -- Kept for Effects type alias (Set Effect)
-- import Data.Text (Text) -- May remove if T.pack etc. are not used
-- import qualified Data.Text as T -- May remove if T.pack etc. are not used

import Language.Memento.Syntax (Type, Effects, Expr, TypeError(..), Effect(..)) -- Ensure these are available
-- Commented out as per instructions
-- import Language.Memento.TypeChecker.Handle (inferHandleType)
-- import Language.Memento.TypeChecker.Match (inferMatchType)
import Language.Memento.TypeChecker.Monad (TypeCheck, unify) -- unify is used
import Language.Memento.TypeChecker.InferTypes (inferHM, TCType(..), EffectK(..)) -- TCType(..), EffectK(..) might not be needed directly if conversions handle everything
import Language.Memento.TypeChecker.TypeConversion (tcTypeToType, effectKToEffects)


-- | Infer the type and effects of an expression, optionally checking against an expected type.
inferType :: Expr -> Maybe Type -> TypeCheck (Type, Effects)
inferType expr mExpectedType =
  -- New body for inferType:
  do
    -- 1. Perform Hindley-Milner inference using inferHM
    -- inferHM is expected to handle all expression forms via getUnifications
    (inferredTcType, inferredEffectK) <- inferHM expr

    -- 2. Convert results from TCType/EffectK back to Syntax.Type/Effects
    inferredSyntaxType <- tcTypeToType inferredTcType
    inferredSyntaxEffects <- effectKToEffects inferredEffectK

    -- 3. If an expected type was provided, unify the inferred Syntax.Type with it.
    -- The `unify` function from Monad.hs works on Syntax.Type.
    case mExpectedType of
      Just expectedSyntaxType -> do
        unify expectedSyntaxType inferredSyntaxType -- This throws if they don't match.
                                                    -- If unify succeeds, the types are compatible.
        -- We return the type derived from HM as it's the principal type.
        -- However, the original inferType returned expectedT if unification succeeded.
        -- For consistency with that behavior, we can return expectedSyntaxType.
        -- Or, stick to returning the principal type: inferredSyntaxType.
        -- The prompt for the new body implies returning inferredSyntaxType.
        return (inferredSyntaxType, inferredSyntaxEffects)
      Nothing ->
        return (inferredSyntaxType, inferredSyntaxEffects)
