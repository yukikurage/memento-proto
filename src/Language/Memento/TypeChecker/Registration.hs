{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} -- May be needed for some existing logic

module Language.Memento.TypeChecker.Registration (
  registerAdtsAndConstructors,
  registerEffects
) where

import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (modify) -- Added import for modify
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax (Definition (..), ConstructorDef (..), OperatorDef (..), Effect (..), Type (..), TypeError (..))
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Types

-- | Register ADTs and their constructors
registerAdtsAndConstructors :: [Definition] -> TypeCheck ()
registerAdtsAndConstructors definitions = do
  let dataDefs = [def | def@(DataDef _ _) <- definitions]

  -- Phase 1: Collect ADT names, check for duplicates, and pre-populate tsAdtEnv.
  currentAdtEnv <- getAdtEnv
  mapM_ (preRegisterAdtName currentAdtEnv) dataDefs

  -- Phase 2: Process constructors for each ADT.
  mapM_ processAdtDefinition dataDefs
 where
  preRegisterAdtName :: Map.Map Text AdtInfo -> Definition -> TypeCheck ()
  preRegisterAdtName initialAdtEnv (DataDef adtName _) = do
    when (Map.member adtName initialAdtEnv) $
      throwError $
        CustomErrorType $
          "Duplicate ADT definition: " <> adtName

    currentBatchAdtEnv <- getAdtEnv
    when (Map.member adtName currentBatchAdtEnv && not (Map.member adtName initialAdtEnv)) $
      throwError $
        CustomErrorType $
          "Duplicate ADT definition in current batch: " <> adtName

    addAdtInfo adtName (AdtInfo{adtName = adtName, adtConstructors = Map.empty}) -- Placeholder for recursive resolution
  preRegisterAdtName _ _ = return () -- Should not be called with ValDef

  processAdtDefinition :: Definition -> TypeCheck ()
  processAdtDefinition (DataDef adtName consDefs) = do
    finalConstructorMap <- foldM (buildAndRegisterConstructor adtName) Map.empty consDefs
    modify $ \st -> st{tsAdtEnv = Map.adjust (\info -> info{adtConstructors = finalConstructorMap}) adtName (tsAdtEnv st)}
  processAdtDefinition _ = return () -- Skip ValDefs

  buildAndRegisterConstructor :: Text -> Map.Map Text ConstructorSignature -> ConstructorDef -> TypeCheck (Map.Map Text ConstructorSignature)
  buildAndRegisterConstructor adtName accumulatedConstructors (ConstructorDef consNameText typ) = do
    env <- getEnv
    (argType, (resultType, resultEffects)) <- extractConstructorType typ

    resolveType resultType (Just adtName) Nothing

    unless (Set.null resultEffects) $
      throwError $
        CustomErrorType $
          "Constructor '" <> consNameText <> "' has effects: " <> T.pack (show resultEffects)

    when (Map.member consNameText env) $
      throwError $
        CustomErrorType $
          "Constructor name '" <> consNameText <> "' conflicts with an existing definition or another constructor."

    when (Map.member consNameText accumulatedConstructors) $
      throwError $
        CustomErrorType $
          "Duplicate constructor name '" <> consNameText <> "' in ADT '" <> adtName <> "'"

    resolveType argType (Just adtName) Nothing

    let constructorSig = ConstructorSignature{csArgType = argType, csResultType = resultType}
    let functionalType = buildConstructorType argType resultType

    addBinding consNameText functionalType
    return $ Map.insert consNameText constructorSig accumulatedConstructors

-- | Register Effect definitions
registerEffects :: [Definition] -> TypeCheck ()
registerEffects definitions = do
  let effectDefs = [def | def@(EffectDef _ _) <- definitions]
  mapM_ processEffectDefinition effectDefs
 where
  processEffectDefinition :: Definition -> TypeCheck ()
  processEffectDefinition (EffectDef effectName opDefs) = do
    currentEffectEnv <- getEffectEnv
    when (Map.member effectName currentEffectEnv) $
      throwError $
        CustomErrorType $
          "Duplicate effect definition: " <> effectName

    opSigMap <- foldM (buildAndRegisterOperator effectName) Map.empty opDefs
    addEffectInfo effectName (EffectInfo{eiName = effectName, eiOps = opSigMap})
  processEffectDefinition _ = return ()

  buildAndRegisterOperator :: Text -> Map.Map Text OperatorSignature -> OperatorDef -> TypeCheck (Map.Map Text OperatorSignature)
  buildAndRegisterOperator effectName accumulatedOpSigs (OperatorDef opName opType) = do
    resolveType opType Nothing (Just effectName)
    (argType, (finalRetType, finalRetEffects)) <- extractConstructorType opType

    unless (finalRetEffects == Set.singleton (Effect effectName)) $
      throwError $
        CustomErrorType $
          "Operator '" <> opName <> "' in effect '" <> effectName <> "' has effects: " <> T.pack (show finalRetEffects)
          <> ". Expected: " <> T.pack (show (Set.singleton (Effect effectName)))


    when (Map.member opName accumulatedOpSigs) $
      throwError $
        CustomErrorType $
          "Duplicate operator name '" <> opName <> "' in effect '" <> effectName <> "'"

    let opSig = OperatorSignature{osArgType = argType, osRetType = finalRetType, osEffectName = effectName}
    addOperatorInfo opName opSig
    return $ Map.insert opName opSig accumulatedOpSigs
