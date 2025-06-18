{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Language.Memento.TypeSolver.Types where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Base (List)

-- Type representation
data Type
  = TNumber
  | TBool
  | TString
  | TNever
  | TUnknown
  | TLiteral Literal
  | TVar TypeVar        -- Fresh type variables (can be unified)
  | TConstructor T.Text  -- Named constructors (distinct ground types)
  | TFunction (List Type) Type
  | TUnion (Set.Set Type)
  | TIntersection (Set.Set Type)
  -- Polymorphism support
  | TGeneric T.Text                    -- Generic type parameter (e.g., T, U)
  | TApplication Type (List Type)        -- Type application (e.g., List<T>, Map<K,V>)
  deriving (Eq, Ord, Show)

data Literal
  = LNumber Double
  | LBool Bool
  | LString Text
  deriving (Eq, Ord, Show)

newtype TypeVar = TypeVar Text
  deriving (Eq, Ord, Show)

-- Type schemes for polymorphic types (forall quantification)
data TypeScheme = TypeScheme [T.Text] Type  -- forall [a1, a2, ...] . Type
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
combineVariance _ _ = Invariant  -- Mixed usage = invariant

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
  map (\name -> analyzeParameterVariance varsMap name typeExpr Covariant) paramNames

-- Analyze variance of a specific type parameter in a type expression
analyzeParameterVariance :: TypeConstructorVariances -> T.Text -> Type -> Variance -> Variance
analyzeParameterVariance varsMap paramName typeExpr currentVariance = case typeExpr of
  -- Base cases - no variance contribution
  TNumber -> Invariant
  TBool -> Invariant
  TString -> Invariant
  TNever -> Invariant
  TUnknown -> Invariant
  TLiteral _ -> Invariant
  TVar _ -> Invariant
  TConstructor _ -> Invariant

  -- Generic type parameter - this is where we find our target!
  TGeneric name
    | name == paramName -> currentVariance
    | otherwise -> Invariant

  -- Function type - argument is contravariant, return is covariant
  TFunction argsType retType ->
    let argVariances = map (\arg ->
          analyzeParameterVariance varsMap paramName arg (flipVariance currentVariance)) argsType
        retVariance = analyzeParameterVariance varsMap paramName retType currentVariance
    in foldl combineVariance retVariance argVariances

  -- Union type - maintains current variance for all members
  TUnion types ->
    let variances = map (\t -> analyzeParameterVariance varsMap paramName t currentVariance) (Set.toList types)
    in foldl combineVariance Invariant variances

  -- Intersection type - maintains current variance for all members
  TIntersection types ->
    let variances = map (\t -> analyzeParameterVariance varsMap paramName t currentVariance) (Set.toList types)
    in foldl combineVariance Invariant variances

  -- Type application - need to look up variance of the base type
  -- For now, assume all arguments are covariant (this needs the variance registry)
  TApplication baseType argTypes -> case baseType of
    TConstructor name
      | Just variances <- Map.lookup name varsMap ->
        let argVariances = zipWith (\arg var
              -> analyzeParameterVariance
                   varsMap paramName arg (composeVariance currentVariance var)) argTypes variances
        in foldl combineVariance Invariant argVariances
      | otherwise -> Invariant  -- No variance info for this constructor

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
containsVar (TConstructor _) = False  -- Constructors are ground types
containsVar (TFunction args ret) = any containsVar args || containsVar ret
containsVar (TUnion ts) = any containsVar (Set.toList ts)
containsVar (TIntersection ts) = any containsVar (Set.toList ts)
containsVar (TGeneric _) = False  -- Generic parameters are not unification variables
containsVar (TApplication base args) = containsVar base || any containsVar args

-- Get all type variables in a type
typeVars :: Type -> Set.Set TypeVar
typeVars TNumber = Set.empty
typeVars TBool = Set.empty
typeVars TString = Set.empty
typeVars TNever = Set.empty
typeVars TUnknown = Set.empty
typeVars (TLiteral _) = Set.empty
typeVars (TVar v) = Set.singleton v
typeVars (TConstructor _) = Set.empty  -- Constructors have no type variables
typeVars (TFunction args ret) = Set.unions (map typeVars args) `Set.union` typeVars ret
typeVars (TUnion ts) = Set.unions (map typeVars (Set.toList ts))
typeVars (TIntersection ts) = Set.unions (map typeVars (Set.toList ts))
typeVars (TGeneric _) = Set.empty  -- Generic parameters are not unification variables
typeVars (TApplication base args) = typeVars base `Set.union` Set.unions (map typeVars args)

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
applySubst _ constructor@(TConstructor _) = constructor  -- Constructors unchanged
applySubst s (TFunction args ret) = TFunction (map (applySubst s) args) (applySubst s ret)
applySubst s (TUnion ts) = mkUnion (map (applySubst s) (Set.toList ts))
applySubst s (TIntersection ts) = mkIntersection (map (applySubst s) (Set.toList ts))
applySubst _ generic@(TGeneric _) = generic  -- Generic parameters unchanged by unification
applySubst s (TApplication base args) = TApplication (applySubst s base) (map (applySubst s) args)

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
freeGenerics (TConstructor _) = Set.empty
freeGenerics (TFunction args ret) = Set.unions (map freeGenerics args) `Set.union` freeGenerics ret
freeGenerics (TUnion ts) = Set.unions (map freeGenerics (Set.toList ts))
freeGenerics (TIntersection ts) = Set.unions (map freeGenerics (Set.toList ts))
freeGenerics (TGeneric name) = Set.singleton name
freeGenerics (TApplication base args) = freeGenerics base `Set.union` Set.unions (map freeGenerics args)

-- Generalize a type to a type scheme (quantify over free generic variables)
generalize :: Type -> TypeScheme
generalize t = TypeScheme (Set.toList (freeGenerics t)) t

-- Instantiate a type scheme with fresh type variables
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
substituteGenerics _ constructor@(TConstructor _) = constructor
substituteGenerics s (TFunction args ret) = TFunction (map (substituteGenerics s) args) (substituteGenerics s ret)
substituteGenerics s (TUnion ts) = mkUnion (map (substituteGenerics s) (Set.toList ts))
substituteGenerics s (TIntersection ts) = mkIntersection (map (substituteGenerics s) (Set.toList ts))
substituteGenerics s (TGeneric name) =
  case Map.lookup name s of
    Just t -> t
    Nothing -> TGeneric name
substituteGenerics s (TApplication base args) =
  TApplication (substituteGenerics s base) (map (substituteGenerics s) args)
