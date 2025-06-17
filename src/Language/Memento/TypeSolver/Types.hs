{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Language.Memento.TypeSolver.Types where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)

-- Type representation
data Type
  = TTop
  | TBottom
  | TNumber
  | TBool
  | TString
  | TNever
  | TUnknown
  | TLiteral Literal
  | TVar TypeVar        -- Fresh type variables (can be unified)
  | TConstructor T.Text  -- Named constructors (distinct ground types)
  | TFunction Type Type
  | TUnion (Set.Set Type)
  | TIntersection (Set.Set Type)
  deriving (Eq, Ord, Show)

data Literal
  = LNumber Double
  | LBool Bool
  | LString Text
  deriving (Eq, Ord, Show)

newtype TypeVar = TypeVar Text
  deriving (Eq, Ord, Show)

-- Constraints
data Constraint = Subtype Type Type
  deriving (Eq, Ord, Show)

-- Constraint set for solving
type ConstraintSet = Set.Set Constraint

-- Substitution mapping type variables to types
type Substitution = Map.Map TypeVar Type

-- Result of type solving
data SolveResult
  = Success Substitution
  | Contradiction
  | Ambiguous [ConstraintSet] -- Multiple possible solutions
  deriving (Show)

-- Helper constructors
mkUnion :: [Type] -> Type
mkUnion [] = TBottom
mkUnion [t] = t
mkUnion ts = TUnion (Set.fromList ts)

mkIntersection :: [Type] -> Type
mkIntersection [] = TTop
mkIntersection [t] = t
mkIntersection ts = TIntersection (Set.fromList ts)

-- Check if type contains variables
containsVar :: Type -> Bool
containsVar TTop = False
containsVar TBottom = False
containsVar TNumber = False
containsVar TBool = False
containsVar TString = False
containsVar TNever = False
containsVar TUnknown = False
containsVar (TLiteral _) = False
containsVar (TVar _) = True
containsVar (TConstructor _) = False  -- Constructors are ground types
containsVar (TFunction t1 t2) = containsVar t1 || containsVar t2
containsVar (TUnion ts) = any containsVar (Set.toList ts)
containsVar (TIntersection ts) = any containsVar (Set.toList ts)

-- Get all type variables in a type
typeVars :: Type -> Set.Set TypeVar
typeVars TTop = Set.empty
typeVars TBottom = Set.empty
typeVars TNumber = Set.empty
typeVars TBool = Set.empty
typeVars TString = Set.empty
typeVars TNever = Set.empty
typeVars TUnknown = Set.empty
typeVars (TLiteral _) = Set.empty
typeVars (TVar v) = Set.singleton v
typeVars (TConstructor _) = Set.empty  -- Constructors have no type variables
typeVars (TFunction t1 t2) = typeVars t1 `Set.union` typeVars t2
typeVars (TUnion ts) = Set.unions (map typeVars (Set.toList ts))
typeVars (TIntersection ts) = Set.unions (map typeVars (Set.toList ts))

-- Apply substitution to a type
applySubst :: Substitution -> Type -> Type
applySubst _ TTop = TTop
applySubst _ TBottom = TBottom
applySubst _ TNumber = TNumber
applySubst _ TBool = TBool
applySubst _ TString = TString
applySubst _ TNever = TNever
applySubst _ TUnknown = TUnknown
applySubst _ lit@(TLiteral _) = lit
applySubst s (TVar v) = 
  case Map.lookup v s of
    Just t -> t
    Nothing -> TVar v
applySubst _ constructor@(TConstructor _) = constructor  -- Constructors unchanged
applySubst s (TFunction t1 t2) = TFunction (applySubst s t1) (applySubst s t2)
applySubst s (TUnion ts) = mkUnion (map (applySubst s) (Set.toList ts))
applySubst s (TIntersection ts) = mkIntersection (map (applySubst s) (Set.toList ts))

-- Apply substitution to a constraint
applySubstConstraint :: Substitution -> Constraint -> Constraint
applySubstConstraint s (Subtype t1 t2) = Subtype (applySubst s t1) (applySubst s t2)