-- Main module for the new type solver
module Language.Memento.TypeSolver
  ( module Language.Memento.TypeSolver.Types
  , module Language.Memento.TypeSolver.Solver
  , module Language.Memento.TypeSolver.Subtype
  , module Language.Memento.TypeSolver.Normalize
  , module Language.Memento.TypeSolver.ConstraintGen
  , typeCheckProgram
  , typeCheckAST
  ) where

import Language.Memento.TypeSolver.Types
import Language.Memento.TypeSolver.Solver
import Language.Memento.TypeSolver.Subtype
import Language.Memento.TypeSolver.Normalize
import Language.Memento.TypeSolver.ConstraintGen
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- Simple type checking function for testing
typeCheckProgram :: [T.Text] -> Either String (Map.Map T.Text Type)
typeCheckProgram variableNames =
  let -- Create simple constraints for testing
      vars = map TypeVar variableNames
      constraints = Set.fromList
        [ Subtype (TVar (TypeVar name)) TNumber | name <- variableNames ]
  in case solve Map.empty constraints of  -- Empty variance map for testing
    Success -> Right $ Map.fromList
      [(name, TVar (TypeVar name)) | name <- variableNames]  -- No substitution returned
    Contradiction msg -> Left $ "Type contradiction: " ++ msg
