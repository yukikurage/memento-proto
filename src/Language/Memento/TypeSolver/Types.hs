{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Language.Memento.TypeSolver.Types where

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (List)

-- Type representation
data Type
  = TNumber
  | TBool
  | TString
  | TNever
  | TUnknown
  | TLiteral Literal
  | TVar TypeVar -- Fresh type variables (can be unified)
  | TFunction (List Type) Type
  | TUnion (Set.Set Type)
  | TIntersection (Set.Set Type)
  | -- Polymorphism support
    TGeneric T.Text -- Generic type parameter (e.g., T, U)
  | TApplication Text (List Type) -- Type application for Constructor (e.g., Some<T>)
  deriving (Eq, Ord, Show)

data Literal
  = LNumber Double
  | LBool Bool
  | LString Text
  deriving (Eq, Ord, Show)

newtype TypeVar = TypeVar Text
  deriving (Eq, Ord, Show)

formatType :: Type -> String
formatType TNumber = "number"
formatType TBool = "boolean"
formatType TString = "string"
formatType TNever = "never"
formatType TUnknown = "unknown"
formatType (TLiteral (LNumber n)) = show n
formatType (TLiteral (LBool b)) = show b
formatType (TLiteral (LString s)) = "\"" ++ T.unpack s ++ "\""
formatType (TVar (TypeVar name)) = T.unpack name
formatType (TFunction args ret) =
  "(" ++ intercalate ", " (map formatType args) ++ " => " ++ formatType ret ++ ")"
formatType (TUnion ts) =
  "(" ++ intercalate " | " (map formatType (Set.toList ts)) ++ ")"
formatType (TIntersection ts) =
  "(" ++ intercalate " & " (map formatType (Set.toList ts)) ++ ")"
formatType (TGeneric name) = T.unpack name
formatType (TApplication base args) =
  T.unpack base ++ "<" ++ intercalate ", " (map formatType args) ++ ">"

-- Type schemes for polymorphic types (forall quantification)
data TypeScheme = TypeScheme [T.Text] Type -- forall [a1, a2, ...] . Type
  deriving (Eq, Ord, Show)

-- Monomorphic type scheme (no quantification)
monoType :: Type -> TypeScheme
monoType t = TypeScheme [] t

-- Variance analysis for type parameters
data Variance = Covariant | Contravariant | Invariant | Bivariant
  deriving (Eq, Ord, Show)

-- | Like :   Some |=> [Covariant],  or Either |=> [Covariant, Covariant],  Reader |=> [Contravariant, Covariant] ...
type TypeConstructorVariances = Map.Map T.Text [Variance]

-- ComposeVariance x y is variance y in variance x: e.g., for function type (X => Y) => Z, X is covariant because contravariant (X) in contravariant (X => Y)
composeVariance :: Variance -> Variance -> Variance
composeVariance Covariant x = x
composeVariance Contravariant x = flipVariance x
composeVariance Invariant x = Invariant
composeVariance Bivariant x = Bivariant

-- Combine variances when a type parameter appears in multiple positions
combineVariance :: Variance -> Variance -> Variance
combineVariance Covariant Covariant = Covariant
combineVariance Contravariant Contravariant = Contravariant
combineVariance _ _ = Invariant -- Mixed usage = invariant

-- Flip variance when entering contravariant position (like function arguments)
flipVariance :: Variance -> Variance
flipVariance Covariant = Contravariant
flipVariance Contravariant = Covariant
flipVariance Invariant = Invariant
flipVariance Bivariant = Bivariant

-- Analyze variance of type parameters in a type expression
-- Given a list of type parameter names and a type expression,
-- determine the variance of each parameter
analyzeVariance :: TypeConstructorVariances -> [T.Text] -> Type -> [Variance]
analyzeVariance varsMap paramNames typeExpr =
  map (\name -> analyzeParameterVariance varsMap name typeExpr) paramNames

-- Analyze variance of a specific type parameter in a type expression
analyzeParameterVariance :: TypeConstructorVariances -> T.Text -> Type -> Variance
analyzeParameterVariance varsMap paramName typeExpr = case typeExpr of
  -- Base cases - no variance contribution
  TNumber -> Bivariant
  TBool -> Bivariant
  TString -> Bivariant
  TNever -> Bivariant
  TUnknown -> Bivariant
  TLiteral _ -> Bivariant
  TVar _ -> Bivariant
  -- Generic type parameter - this is where we find our target!
  TGeneric name
    | name == paramName -> Covariant
    | otherwise -> Invariant
  -- Function type - argument is contravariant, return is covariant
  TFunction argsType retType ->
    let argVariances = map (flipVariance . analyzeParameterVariance varsMap paramName) argsType
        retVariance = analyzeParameterVariance varsMap paramName retType
     in foldl combineVariance retVariance argVariances
  -- Union type - maintains current variance for all members
  TUnion types ->
    let variances = map (analyzeParameterVariance varsMap paramName) (Set.toList types)
     in foldl combineVariance Bivariant variances
  -- Intersection type - maintains current variance for all members
  TIntersection types ->
    let variances = map (analyzeParameterVariance varsMap paramName) (Set.toList types)
     in foldl combineVariance Bivariant variances
  -- Type application - need to look up variance of the other types
  TApplication name argTypes
    | Just variances <- Map.lookup name varsMap ->
        let argVariances =
              zipWith
                ( \arg var ->
                    composeVariance var $
                      analyzeParameterVariance
                        varsMap
                        paramName
                        arg
                )
                argTypes
                variances
         in foldl combineVariance Bivariant argVariances
    | otherwise -> Invariant -- No variance info for this constructor

-- Constraints
data Constraint = Subtype Type Type
  deriving (Eq, Ord, Show)

-- Format type errors
formatConstraintSet :: ConstraintSet -> String
formatConstraintSet constraints =
  "All Constraints:\n" ++ unlines (map formatConstraint $ Set.toList constraints)
 where
  formatConstraint (Subtype t1 t2) =
    "  " ++ formatType t1 ++ " <: " ++ formatType t2

-- Constraint set for solving
type ConstraintSet = Set.Set Constraint

-- Substitution mapping type variables to types
type Substitution = Map.Map TypeVar Type

-- Result of type solving
data SolveResult
  = Success
  | Contradiction String
  deriving (Show, Eq)

-- Helper constructors
mkUnion :: [Type] -> Type
mkUnion [] = TNever
mkUnion [t] = t
mkUnion ts = TUnion (Set.fromList ts)

mkIntersection :: [Type] -> Type
mkIntersection [] = TUnknown
mkIntersection [t] = t
mkIntersection ts = TIntersection (Set.fromList ts)

-- Check if type contains variables
containsVar :: Type -> Bool
containsVar TNumber = False
containsVar TBool = False
containsVar TString = False
containsVar TNever = False
containsVar TUnknown = False
containsVar (TLiteral _) = False
containsVar (TVar _) = True
containsVar (TFunction args ret) = any containsVar args || containsVar ret
containsVar (TUnion ts) = any containsVar (Set.toList ts)
containsVar (TIntersection ts) = any containsVar (Set.toList ts)
containsVar (TGeneric _) = False -- Generic parameters are not unification variables
containsVar (TApplication _ args) = any containsVar args

containsGeneric :: Type -> Bool
containsGeneric TNumber = False
containsGeneric TBool = False
containsGeneric TString = False
containsGeneric TNever = False
containsGeneric TUnknown = False
containsGeneric (TLiteral _) = False
containsGeneric (TVar _) = False
containsGeneric (TFunction args ret) = any containsGeneric args || containsGeneric ret
containsGeneric (TUnion ts) = any containsGeneric (Set.toList ts)
containsGeneric (TIntersection ts) = any containsGeneric (Set.toList ts)
containsGeneric (TGeneric _) = True -- This is what we're looking for
containsGeneric (TApplication _ args) = any containsGeneric args

-- Get all type variables in a type
typeVars :: Type -> Set.Set TypeVar
typeVars TNumber = Set.empty
typeVars TBool = Set.empty
typeVars TString = Set.empty
typeVars TNever = Set.empty
typeVars TUnknown = Set.empty
typeVars (TLiteral _) = Set.empty
typeVars (TVar v) = Set.singleton v
typeVars (TFunction args ret) = Set.unions (map typeVars args) `Set.union` typeVars ret
typeVars (TUnion ts) = Set.unions (map typeVars (Set.toList ts))
typeVars (TIntersection ts) = Set.unions (map typeVars (Set.toList ts))
typeVars (TGeneric _) = Set.empty -- Generic parameters are not unification variables
typeVars (TApplication _ args) = Set.unions (map typeVars args)

-- Apply substitution to a type
applySubst :: Substitution -> Type -> Type
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
applySubst s (TFunction args ret) = TFunction (map (applySubst s) args) (applySubst s ret)
applySubst s (TUnion ts) = mkUnion (map (applySubst s) (Set.toList ts))
applySubst s (TIntersection ts) = mkIntersection (map (applySubst s) (Set.toList ts))
applySubst _ generic@(TGeneric _) = generic -- Generic parameters unchanged by unification
applySubst s (TApplication name args) = TApplication name (map (applySubst s) args)

-- Apply substitution to a constraint
applySubstConstraint :: Substitution -> Constraint -> Constraint
applySubstConstraint s (Subtype t1 t2) = Subtype (applySubst s t1) (applySubst s t2)

-- Polymorphism operations

-- Get free generic variables in a type
freeGenerics :: Type -> Set.Set T.Text
freeGenerics TNumber = Set.empty
freeGenerics TBool = Set.empty
freeGenerics TString = Set.empty
freeGenerics TNever = Set.empty
freeGenerics TUnknown = Set.empty
freeGenerics (TLiteral _) = Set.empty
freeGenerics (TVar _) = Set.empty
freeGenerics (TFunction args ret) = Set.unions (map freeGenerics args) `Set.union` freeGenerics ret
freeGenerics (TUnion ts) = Set.unions (map freeGenerics (Set.toList ts))
freeGenerics (TIntersection ts) = Set.unions (map freeGenerics (Set.toList ts))
freeGenerics (TGeneric name) = Set.singleton name
freeGenerics (TApplication base args) = Set.unions (map freeGenerics args)

-- Generalize a type to a type scheme (quantify over free generic variables)
generalize :: Type -> TypeScheme
generalize t = TypeScheme (Set.toList (freeGenerics t)) t

-- Instantiate a type scheme (type with generics) with fresh type variables
instantiate :: TypeScheme -> [TypeVar] -> Type
instantiate (TypeScheme quantified t) freshVars =
  let substitution = Map.fromList (zip quantified (map TVar freshVars))
   in substituteGenerics substitution t

-- Substitute generic type parameters with types
substituteGenerics :: Map.Map T.Text Type -> Type -> Type
substituteGenerics _ TNumber = TNumber
substituteGenerics _ TBool = TBool
substituteGenerics _ TString = TString
substituteGenerics _ TNever = TNever
substituteGenerics _ TUnknown = TUnknown
substituteGenerics _ lit@(TLiteral _) = lit
substituteGenerics _ var@(TVar _) = var
substituteGenerics s (TFunction args ret) = TFunction (map (substituteGenerics s) args) (substituteGenerics s ret)
substituteGenerics s (TUnion ts) = mkUnion (map (substituteGenerics s) (Set.toList ts))
substituteGenerics s (TIntersection ts) = mkIntersection (map (substituteGenerics s) (Set.toList ts))
substituteGenerics s (TGeneric name) =
  case Map.lookup name s of
    Just t -> t
    Nothing -> TGeneric name
substituteGenerics s (TApplication base args) =
  TApplication base (map (substituteGenerics s) args)
