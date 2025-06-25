{-# LANGUAGE OverloadedStrings #-}

{- | Modular solver pipeline for the Memento type solver
Breaks down the solving process into composable, testable stages
-}
module Language.Memento.TypeSolver.SolverPipeline (
  SolverStage (..),
  SolverPipeline (..),
  SolverState (..),
  SolverResult (..),
  createDefaultPipeline,
  executePipeline,
  runStage,
  -- Individual stages
  normalizationStage,
  decompositionStage,
  boundsCalculationStage,
  substitutionStage,
  branchingStage,
  contradictionStage,
  propagationStage,
)
where

import Control.Monad.Error.Class (MonadError (catchError, throwError))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (traceM)
import Language.Memento.TypeSolver.Assumption (calculateGenericBounds, decomposeAssumptionAll)
import Language.Memento.TypeSolver.Normalize (normalizeConstraint)
import Language.Memento.TypeSolver.Solver (branchConstraints, calculatePropagationAll, checkContradictions, decomposeConstraintsAll, substInstancesAsPossible)
import Language.Memento.TypeSolver.Types

-- ============================================================================
-- Core Pipeline Types
-- ============================================================================

-- | Represents the current state of the solver
data SolverState = SolverState
  { ssConstraints :: ConstraintSet
  -- ^ Current constraints
  , ssAssumptions :: AssumptionSet
  -- ^ Current assumptions
  , ssGenericBounds :: GenericBoundsMap
  -- ^ Generic bounds map
  , ssVariances :: TypeConstructorVariances
  -- ^ Type constructor variances
  , ssStageHistory :: [T.Text]
  -- ^ History of stages executed (for debugging)
  }
  deriving (Show, Eq)

-- | Result of executing a solver stage
data SolverResult m
  = -- | Continue with updated state
    SolverContinue SolverState
  | -- | Branch into multiple paths
    SolverBranch [(SolverState, SolverPipeline m)]
  | -- | Solving completed successfully
    SolverSuccess
  | -- | Solving failed with error
    SolverFailure TypeError

-- | A single stage in the solver pipeline
data SolverStage m = SolverStage
  { stageName :: T.Text
  -- ^ Name for debugging
  , stageFunction :: SolverState -> m (SolverResult m)
  -- ^ Stage implementation
  }

instance Show (SolverStage m) where
  show stage = "SolverStage{stageName=" ++ T.unpack (stageName stage) ++ "}"

-- | Complete solver pipeline
data SolverPipeline m = SolverPipeline
  { pipelineStages :: [SolverStage m]
  -- ^ Ordered list of stages
  , pipelineName :: T.Text
  -- ^ Pipeline name for debugging
  }

instance Show (SolverPipeline m) where
  show pipeline =
    "SolverPipeline{pipelineName="
      ++ T.unpack (pipelineName pipeline)
      ++ ", stages=["
      ++ show (map stageName (pipelineStages pipeline))
      ++ "]}"

instance Show (SolverResult m) where
  show (SolverContinue state) = "SolverContinue " ++ show state
  show (SolverBranch branches) = "SolverBranch (" ++ show (length branches) ++ " branches)"
  show SolverSuccess = "SolverSuccess"
  show (SolverFailure err) = "SolverFailure " ++ T.unpack (formatTypeError err)

-- ============================================================================
-- Pipeline Execution
-- ============================================================================

-- | Execute a complete solver pipeline
executePipeline :: (MonadError TypeError m) => SolverPipeline m -> SolverState -> m ()
executePipeline pipeline initialState = do
  result <- runPipelineStages (pipelineStages pipeline) initialState
  case result of
    SolverSuccess -> return ()
    SolverFailure err -> throwError err
    SolverContinue _ -> throwError $ InternalTypeError "Pipeline ended without resolution"
    SolverBranch _ -> throwError $ InternalTypeError "Pipeline ended with unresolved branches"

-- | Run pipeline stages sequentially
runPipelineStages :: (MonadError TypeError m) => [SolverStage m] -> SolverState -> m (SolverResult m)
runPipelineStages [] state =
  return $ SolverSuccess -- No more stages, assume success
runPipelineStages (stage : restStages) state = do
  result <- runStage stage state
  case result of
    SolverContinue newState ->
      runPipelineStages restStages newState
    SolverBranch branches -> do
      -- Handle branching: try each branch until one succeeds
      branchResults <- mapM tryBranch branches
      case [r | Right r <- branchResults] of
        (result : _) -> return result -- First successful branch
        [] -> throwError $ InternalTypeError "All branches failed"
    SolverSuccess ->
      return SolverSuccess
    SolverFailure err ->
      return $ SolverFailure err
 where
  tryBranch :: (MonadError TypeError m) => (SolverState, SolverPipeline m) -> m (Either TypeError (SolverResult m))
  tryBranch (branchState, branchPipeline) =
    (Right <$> runPipelineStages (pipelineStages branchPipeline) branchState)
      `catchError` (return . Left)

-- | Execute a single stage
runStage :: (MonadError TypeError m) => SolverStage m -> SolverState -> m (SolverResult m)
runStage stage state = do
  let newState = state{ssStageHistory = stageName stage : ssStageHistory state}
  stageFunction stage newState

-- ============================================================================
-- Default Pipeline
-- ============================================================================

-- | Create the default solver pipeline
createDefaultPipeline :: (MonadError TypeError m) => SolverPipeline m
createDefaultPipeline =
  SolverPipeline
    { pipelineStages =
        [ normalizationStage
        , decompositionStage
        , boundsCalculationStage
        , substitutionStage
        , decompositionStage -- Second decomposition after substitution
        , boundsCalculationStage -- Recalculate bounds
        , branchingStage
        , contradictionStage
        , propagationStage
        ]
    , pipelineName = "DefaultSolverPipeline"
    }

-- ============================================================================
-- Individual Stages
-- ============================================================================

-- | Stage 1: Normalization of constraints and assumptions
normalizationStage :: (MonadError TypeError m) => SolverStage m
normalizationStage =
  SolverStage
    { stageName = "Normalization"
    , stageFunction = \state -> do
        normalizedConstraints <- Set.fromList <$> mapM normalizeConstraint (Set.toList $ ssConstraints state)
        normalizedAssumptions <- Set.fromList <$> mapM normalizeConstraint (Set.toList $ ssAssumptions state)
        return $
          SolverContinue
            state
              { ssConstraints = normalizedConstraints
              , ssAssumptions = normalizedAssumptions
              }
    }

-- | Stage 2: Decomposition of constraints and assumptions
decompositionStage :: (MonadError TypeError m) => SolverStage m
decompositionStage =
  SolverStage
    { stageName = "Decomposition"
    , stageFunction = \state -> do
        let decomposedAssumptions = decomposeAssumptionAll (ssVariances state) (ssAssumptions state)
        decomposedConstraints <- decomposeConstraintsAll (ssVariances state) (ssGenericBounds state) (ssConstraints state)
        return $
          SolverContinue
            state
              { ssConstraints = decomposedConstraints
              , ssAssumptions = decomposedAssumptions
              }
    }

-- | Stage 3: Generic bounds calculation
boundsCalculationStage :: (MonadError TypeError m) => SolverStage m
boundsCalculationStage =
  SolverStage
    { stageName = "BoundsCalculation"
    , stageFunction = \state -> do
        let newGenericBounds = calculateGenericBounds (ssVariances state) (ssAssumptions state)
        return $
          SolverContinue
            state
              { ssGenericBounds = newGenericBounds
              }
    }

-- | Stage 4: Instance substitution
substitutionStage :: (MonadError TypeError m) => SolverStage m
substitutionStage =
  SolverStage
    { stageName = "Substitution"
    , stageFunction = \state -> do
        let (substedAssumptions, substedConstraints) =
              substInstancesAsPossible (ssAssumptions state, ssConstraints state)
        return $
          SolverContinue
            state
              { ssConstraints = substedConstraints
              , ssAssumptions = substedAssumptions
              }
    }

-- | Stage 5: Branching for union/intersection types
branchingStage :: (MonadError TypeError m) => SolverStage m
branchingStage =
  SolverStage
    { stageName = "Branching"
    , stageFunction = \state -> do
        case branchConstraints (ssVariances state) (ssAssumptions state) (ssConstraints state) of
          Nothing ->
            -- No branching needed, continue to next stage
            return $ SolverContinue state
          Just branches -> do
            -- Create branch states and sub-pipelines
            let branchStates = map (createBranchState state) branches
                subPipeline = createPostBranchPipeline
            return $ SolverBranch [(s, subPipeline) | s <- branchStates]
    }
 where
  createBranchState :: SolverState -> (AssumptionSet, ConstraintSet) -> SolverState
  createBranchState originalState (branchAssumptions, branchConstraints) =
    SolverState
      { ssConstraints = branchConstraints
      , ssAssumptions = branchAssumptions
      , ssGenericBounds = Map.empty -- Will be recalculated
      , ssVariances = ssVariances originalState
      , ssStageHistory = "Branch" : ssStageHistory originalState
      }

  createPostBranchPipeline =
    SolverPipeline
      { pipelineStages = [contradictionStage, propagationStage]
      , pipelineName = "PostBranchPipeline"
      }

-- | Stage 6: Contradiction checking
contradictionStage :: (MonadError TypeError m) => SolverStage m
contradictionStage =
  SolverStage
    { stageName = "ContradictionCheck"
    , stageFunction = \state -> do
        let propagatedConstraints = calculatePropagationAll (ssConstraints state)
        checkContradictions (ssVariances state) (ssGenericBounds state) propagatedConstraints
        return $ SolverContinue state
    }

-- | Stage 7: Final propagation
propagationStage :: (MonadError TypeError m) => SolverStage m
propagationStage =
  SolverStage
    { stageName = "Propagation"
    , stageFunction = \state -> do
        let _propagatedConstraints = calculatePropagationAll (ssConstraints state)
        -- If we reach here without errors, solving is complete
        return SolverSuccess
    }

-- ============================================================================
-- Pipeline Builders
-- ============================================================================

-- | Create a custom pipeline with specific stages
createCustomPipeline :: T.Text -> [SolverStage m] -> SolverPipeline m
createCustomPipeline name stages =
  SolverPipeline
    { pipelineStages = stages
    , pipelineName = name
    }

-- | Create a pipeline for testing individual stages
createTestPipeline :: [SolverStage m] -> SolverPipeline m
createTestPipeline stages = createCustomPipeline "TestPipeline" stages

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Create initial solver state
createInitialState :: TypeConstructorVariances -> AssumptionSet -> ConstraintSet -> SolverState
createInitialState variances assumptions constraints =
  SolverState
    { ssConstraints = constraints
    , ssAssumptions = assumptions
    , ssGenericBounds = Map.empty
    , ssVariances = variances
    , ssStageHistory = []
    }

-- | Get pipeline execution trace for debugging
getPipelineTrace :: SolverState -> [T.Text]
getPipelineTrace = reverse . ssStageHistory
