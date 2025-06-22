-- Main module for the new type solver
module Language.Memento.TypeSolver (
  module Language.Memento.TypeSolver.Types,
  module Language.Memento.TypeSolver.Solver,
  module Language.Memento.TypeSolver.Subtype,
  module Language.Memento.TypeSolver.Normalize,
  module Language.Memento.TypeSolver.ConstraintGen,
  typeCheckAST,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.TypeSolver.ConstraintGen
import Language.Memento.TypeSolver.Normalize
import Language.Memento.TypeSolver.Solver
import Language.Memento.TypeSolver.Subtype
import Language.Memento.TypeSolver.Types
