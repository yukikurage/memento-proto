-- Main module for the type solver
module Language.Memento.TypeSolver (
  module Language.Memento.TypeSolver.Core.Types,
  module Language.Memento.TypeSolver.Constraints.SolverPipeline,
  module Language.Memento.TypeSolver.Constraints.Subtype,
  module Language.Memento.TypeSolver.Constraints.Normalize,
  module Language.Memento.TypeSolver.Core.TypeInference,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.TypeSolver.Core.TypeInference
import Language.Memento.TypeSolver.Constraints.Normalize
import Language.Memento.TypeSolver.Constraints.SolverPipeline
import Language.Memento.TypeSolver.Constraints.Subtype
import Language.Memento.TypeSolver.Core.Types
