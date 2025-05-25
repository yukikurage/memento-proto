{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.TypeChecker.Handle (
  inferHandleType,
  -- Exporting helpers if they are complex and might be reusable, though inferHandleType is the main entry point
  collectOpSigs,
  partitionHandlerClauses,
  processReturnClauses,
  processOpClauses,
) where

import Control.Monad (foldM, unless, when)
import Control.Monad.Except (throwError)
import qualified Data.List as List (partition)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Types

-- Forward declaration for inferType, will be passed as argument
-- import Language.Memento.TypeChecker.Expressions (inferType) -- This would be cyclic

-- Type of the inferType function passed as an argument
type InferTypeFunc = Expr -> Maybe Type -> TypeCheck (Type, Effects)

-- | Helper: Partition HandlerClauses into operator clauses and return clauses
partitionHandlerClauses :: [HandlerClause] -> ([HandlerClause], [HandlerClause])
partitionHandlerClauses = List.partition isOpClause
 where
  isOpClause HandlerClause{} = True
  isOpClause _ = False

-- | Helper: Process return clauses for a Handle expression
processReturnClauses :: InferTypeFunc -> Type -> [HandlerClause] -> Maybe Type -> TypeCheck (Type, Effects)
processReturnClauses inferTypeFunc exprType [HandlerReturnClause retVarName bodyExpr] mExpectedBodyType =
  withBinding retVarName exprType $ inferTypeFunc bodyExpr mExpectedBodyType
processReturnClauses _ _ _ _ =
  throwError $ CustomErrorType "Internal error: processReturnClauses called with invalid arguments. Expected exactly one return clause properly structured."

-- | Helper: Process operator clauses for a Handle expression
processOpClauses :: InferTypeFunc -> Map.Map Text OperatorSignature -> [HandlerClause] -> Type -> Effects -> Maybe Type -> TypeCheck ()
processOpClauses _ _ [] _ _ _ = return () -- No op clauses to process
processOpClauses inferTypeFunc operatorSigsMap clauses targetBodyType targetBodyEffects mExpectedOpBodyType = do
  mapM_ (processSingleOpClause mExpectedOpBodyType) clauses -- Pass mExpectedOpBodyType
 where
  processSingleOpClause expectedBodyType (HandlerClause opName argVarName contVarName bodyExpr) = do
    opSig <- case Map.lookup opName operatorSigsMap of
      Just sig -> return sig
      Nothing -> throwError $ CustomErrorType $ "Operator '" <> opName <> "' not defined for handled effects."

    let contType = TFunction (osRetType opSig) (targetBodyType, targetBodyEffects) -- targetBodyType is the type of the continuation's result
    (currentOpClauseBodyType, currentOpClauseBodyEffects) <-
      withBindings [(argVarName, osArgType opSig), (contVarName, contType)] $ inferTypeFunc bodyExpr expectedBodyType -- Pass expectation here
    unify targetBodyType currentOpClauseBodyType -- Op clause body must unify with return clause type
    unless (currentOpClauseBodyEffects `Set.isSubsetOf` targetBodyEffects) $
      throwError $
        EffectMismatch targetBodyEffects currentOpClauseBodyEffects
  processSingleOpClause _ (HandlerReturnClause _ _) =
    throwError $ CustomErrorType "Internal error: processOpClauses received a HandlerReturnClause."

-- | Helper: Collect operator signatures for handled effects
collectOpSigs :: Map.Map Text EffectInfo -> Map.Map Text OperatorSignature -> Text -> TypeCheck (Map.Map Text OperatorSignature)
collectOpSigs effectEnv accSigs effectName = do
  effectInfo <- case Map.lookup effectName effectEnv of
    Just ei -> return ei
    Nothing -> throwError $ CustomErrorType $ "Undefined effect referenced in handle: " <> effectName
  let currentEffectOps = eiOps effectInfo
  -- Check for duplicate operator names across different handled effects
  mapM_
    ( \opN ->
        when (Map.member opN accSigs) $
          throwError $
            CustomErrorType $
              "Duplicate operator name '" <> opN <> "' found across handled effects."
    )
    (Map.keys currentEffectOps)
  return $ Map.union accSigs currentEffectOps

{- | Infer the type and effects of a Handle expression.
Takes the main inferType function as an argument to break cycle.
-}
inferHandleType :: InferTypeFunc -> Type -> [HandlerClause] -> Maybe Type -> TypeCheck (Type, Effects)
inferHandleType inferTypeFunc handlerType handlerClauses mExpectedReturnType = do
  effectEnv <- getEffectEnv
  ((argTypeFromSig, argEffectsSetFromSig), (retTypeFromSig, retEffectsSetFromSig)) <- extractHandlerType handlerType
  ((mExpectedArgType, _), (mExpectedRetType, _)) <- case mExpectedReturnType of
    Just expectedReturnType -> do
      ((expectedArgType, _), (expectedRetType, _)) <- extractHandlerType expectedReturnType
      return ((Just expectedArgType, Set.empty), (Just expectedRetType, Set.empty))
    Nothing -> return ((Nothing, Set.empty), (Nothing, Set.empty)) -- Wildcard for arg and ret types
  let effectNamesToHandle = Set.map (\(Effect name) -> name) argEffectsSetFromSig

  operatorSigsMap :: Map.Map Text OperatorSignature <- foldM (collectOpSigs effectEnv) Map.empty effectNamesToHandle

  let (opClauses, returnClauses) = partitionHandlerClauses handlerClauses

  when (null returnClauses) $
    throwError $
      CustomErrorType "Handle expression must have at least one return clause."
  when (length returnClauses > 1) $
    throwError $
      CustomErrorType "Handle expression cannot have more than one return clause."

  -- The expected type for the return clause body is mExpectedReturnType (the overall expected type of the handle expression)
  (returnClauseBodyType, returnClauseBodyEffects) <- processReturnClauses inferTypeFunc argTypeFromSig returnClauses mExpectedRetType

  -- Operator clause bodies must also conform to this returnClauseBodyType and mExpectedReturnType.
  -- The effects of op clause bodies are checked against returnClauseBodyEffects.
  processOpClauses inferTypeFunc operatorSigsMap opClauses returnClauseBodyType returnClauseBodyEffects mExpectedRetType

  let declaredOps = Map.keysSet operatorSigsMap
  let providedOps = Set.fromList $ map (\(HandlerClause opN _ _ _) -> opN) opClauses
  unless (declaredOps `Set.isSubsetOf` providedOps) $
    throwError $
      CustomErrorType $
        "Handle expression is not exhaustive. Missing handlers for operators: "
          <> T.pack (show (Set.toList (declaredOps `Set.difference` providedOps)))

  -- Return handler type
  -- Run a computation with the operator HandleApply.

  return (THandler (argTypeFromSig, argEffectsSetFromSig) (retTypeFromSig, retEffectsSetFromSig), Set.empty)