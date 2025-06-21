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
import Data.Maybe (fromMaybe)

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

formatTypeScheme :: TypeScheme -> (String, String)
formatTypeScheme (TypeScheme vars t) =
  let varsStr = if null vars then "" else "<" ++ intercalate ", " (map T.unpack vars) ++ ">"
      typeStr = formatType t
   in (varsStr, typeStr)

formatTypeEnv :: Map.Map T.Text TypeScheme -> String
formatTypeEnv typeSchemeMap =
  intercalate "\n" $
    map
      ( \(k, v) ->
          let (generics, typeStr) = formatTypeScheme v
           in T.unpack k
                ++ " "
                ++ generics
                ++ " : "
                ++ typeStr
      )
      (Map.toList typeSchemeMap)

-- Monomorphic type scheme (no quantification)
monoType :: Type -> TypeScheme
monoType = TypeScheme []

type AssumptionSet = ConstraintSet

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

-- Generic bounds computed from assumptions
data GenericBounds = GenericBounds
  { gbLower :: Type -- Lower bound: gbLower <: Generic
  , gbUpper :: Type -- Upper bound: Generic <: gbUpper
  }
  deriving (Eq, Show)

type GenericBoundsMap = Map.Map T.Text GenericBounds

formatBoundsMap :: GenericBoundsMap -> String
formatBoundsMap boundsMap =
  "Generic Bounds:\n"
    ++ unlines
      [ " : " ++ formatType lower ++ " <: " ++ T.unpack name ++ " < " ++ formatType upper
      | (name, GenericBounds lower upper) <- Map.toList boundsMap
      ]

lookupGenericBounds :: T.Text -> GenericBoundsMap -> GenericBounds
lookupGenericBounds name boundsMap =
  fromMaybe (GenericBounds TNever TUnknown) (Map.lookup name boundsMap)

-- Compute generic bounds (bounds that generic type is possible to move) from constraints
-- It's safe to compute bounds that is more general than actual. e. g., actually a <: T <: b but computing as (never <: T <: b) is safe
-- We should handle nested generics which means,  bool <: (T | number) ~>   bool <: T <: unknown
computeGenericBounds :: AssumptionSet -> GenericBoundsMap
computeGenericBounds assumptions =
  -- Start with the most conservative bounds for all generics
  let constraintList = Set.toList assumptions
      allGenerics = collectAllGenerics constraintList
      -- Process constraints iteratively to refine bounds
      initialBounds = Map.fromList [(g, GenericBounds TNever TUnknown) | g <- Set.toList allGenerics]
      -- First pass: compute bounds without considering other generics in bounds
      simpleBounds = refineGenericBounds constraintList initialBounds
   in -- Second pass: remove circular references in bounds
      removeCircularReferences simpleBounds
 where
  -- Collect all generic type names from constraints
  collectAllGenerics :: [Constraint] -> Set.Set T.Text
  collectAllGenerics = Set.unions . map extractGenericsFromConstraint

  extractGenericsFromConstraint :: Constraint -> Set.Set T.Text
  extractGenericsFromConstraint (Subtype t1 t2) =
    Set.union (extractGenericsFromType t1) (extractGenericsFromType t2)

  extractGenericsFromType :: Type -> Set.Set T.Text
  extractGenericsFromType = go
   where
    go (TGeneric name) = Set.singleton name
    go (TFunction args ret) = Set.unions (map go args) `Set.union` go ret
    go (TUnion ts) = Set.unions (map go (Set.toList ts))
    go (TIntersection ts) = Set.unions (map go (Set.toList ts))
    go (TApplication _ args) = Set.unions (map go args)
    go _ = Set.empty

  -- Refine bounds by processing each constraint
  refineGenericBounds :: [Constraint] -> GenericBoundsMap -> GenericBoundsMap
  refineGenericBounds constraints bounds =
    foldl processConstraint bounds constraints

  -- Process a single constraint to refine bounds
  processConstraint :: GenericBoundsMap -> Constraint -> GenericBoundsMap
  processConstraint bounds (Subtype t1 t2) =
    -- For each generic in the constraint, update its bounds
    let genericsInConstraint = Set.union (extractGenericsFromType t1) (extractGenericsFromType t2)
     in foldl (\b g -> updateBoundsForGeneric g t1 t2 b) bounds (Set.toList genericsInConstraint)

  -- Update bounds for a specific generic based on a constraint
  updateBoundsForGeneric :: T.Text -> Type -> Type -> GenericBoundsMap -> GenericBoundsMap
  updateBoundsForGeneric targetGeneric t1 t2 boundsMap =
    case Map.lookup targetGeneric boundsMap of
      Just (GenericBounds currentLower currentUpper) ->
        let newLower = refineLowerBound targetGeneric t1 t2 currentLower
            newUpper = refineUpperBound targetGeneric t1 t2 currentUpper
         in Map.insert targetGeneric (GenericBounds newLower newUpper) boundsMap
      Nothing -> boundsMap -- Should not happen

  -- Refine lower bound based on constraint t1 <: t2
  refineLowerBound :: T.Text -> Type -> Type -> Type -> Type
  refineLowerBound target t1 t2 currentLower
    -- Direct case: t1 <: target
    | t2 == TGeneric target = mkUnion [currentLower, t1]
    -- Union case: t1 <: (target | other)
    -- For bool <: (T | number), we know bool must be subtype of T or number
    -- Since bool is not subtype of number, it must be subtype of T
    | TUnion ts <- t2
    , TGeneric target `Set.member` ts =
        let otherTypes = Set.filter (/= TGeneric target) ts
            -- Check if t1 is already a subtype of any other type in the union
            -- If not, then t1 must be a subtype of target
            isSubtypeOfOthers = any (\other -> isConservativeSubtype t1 other) (Set.toList otherTypes)
         in if isSubtypeOfOthers
              then currentLower -- t1 is already handled by other types
              else mkUnion [currentLower, t1] -- t1 must be subtype of target
              -- Intersection case: t1 <: (target & other)
    | TIntersection ts <- t2, TGeneric target `Set.member` ts = mkUnion [currentLower, t1]
    | otherwise = currentLower

  -- Conservative subtype check (without using bounds to avoid recursion)
  isConservativeSubtype :: Type -> Type -> Bool
  isConservativeSubtype TNever _ = True
  isConservativeSubtype _ TUnknown = True
  isConservativeSubtype t1 t2 | t1 == t2 = True
  isConservativeSubtype (TLiteral (LNumber _)) TNumber = True
  isConservativeSubtype (TLiteral (LBool _)) TBool = True
  isConservativeSubtype (TLiteral (LString _)) TString = True
  isConservativeSubtype _ _ = False

  -- Refine upper bound based on constraint t1 <: t2
  refineUpperBound :: T.Text -> Type -> Type -> Type -> Type
  refineUpperBound target t1 t2 currentUpper
    -- Direct case: target <: t2
    | t1 == TGeneric target = mkIntersection [currentUpper, t2]
    -- Union case: (target | other) <: t2
    | TUnion ts <- t1, TGeneric target `Set.member` ts = mkIntersection [currentUpper, t2]
    -- Intersection case: (target & other) <: t2
    -- Conservatively don't update
    | TIntersection ts <- t1, TGeneric target `Set.member` ts = currentUpper
    | otherwise = currentUpper
  
  -- Remove circular references in generic bounds
  removeCircularReferences :: GenericBoundsMap -> GenericBoundsMap
  removeCircularReferences boundsMap =
    let dependencies = computeDependencies boundsMap
        sccs = stronglyConnectedComponents dependencies
        -- For each SCC, if it has cycles, simplify the bounds
        simplifiedBounds = foldl simplifySCC boundsMap sccs
     in simplifiedBounds
   where
    -- Compute which generics depend on which other generics
    computeDependencies :: GenericBoundsMap -> Map.Map T.Text (Set.Set T.Text)
    computeDependencies bounds =
      Map.mapWithKey (\name (GenericBounds lower upper) ->
        Set.union (extractGenericsFromType lower) (extractGenericsFromType upper)
      ) bounds
    
    -- Find strongly connected components (cycles)
    stronglyConnectedComponents :: Map.Map T.Text (Set.Set T.Text) -> [[T.Text]]
    stronglyConnectedComponents deps =
      let nodes = Map.keys deps
          -- Simple SCC algorithm - just find direct cycles for now
          findCycles = [ [n1, n2] | 
                         n1 <- nodes,
                         n2 <- nodes, 
                         n1 /= n2,
                         n2 `Set.member` Map.findWithDefault Set.empty n1 deps,
                         n1 `Set.member` Map.findWithDefault Set.empty n2 deps ]
          -- Also include self-cycles
          selfCycles = [ [n] | 
                         n <- nodes,
                         n `Set.member` Map.findWithDefault Set.empty n deps ]
       in nub (selfCycles ++ findCycles)
    
    -- Simplify bounds for a strongly connected component
    simplifySCC :: GenericBoundsMap -> [T.Text] -> GenericBoundsMap
    simplifySCC bounds [] = bounds
    simplifySCC bounds [single] =
      -- Self-reference: remove self from bounds
      case Map.lookup single bounds of
        Nothing -> bounds
        Just (GenericBounds lower upper) ->
          let cleanLower = removeSelfReference single lower
              cleanUpper = removeSelfReference single upper
           in Map.insert single (GenericBounds cleanLower cleanUpper) bounds
    simplifySCC bounds cycle@(_:_:_) =
      -- Multi-node cycle: remove mutual references
      foldl (\b name -> 
        case Map.lookup name b of
          Nothing -> b
          Just (GenericBounds lower upper) ->
            let otherNodes = Set.fromList (filter (/= name) cycle)
                cleanLower = removeReferences otherNodes lower
                cleanUpper = removeReferences otherNodes upper
             in Map.insert name (GenericBounds cleanLower cleanUpper) b
      ) bounds cycle
    
    -- Remove self-reference from a type
    removeSelfReference :: T.Text -> Type -> Type
    removeSelfReference name = removeReferences (Set.singleton name)
    
    -- Remove references to specific generics from a type
    removeReferences :: Set.Set T.Text -> Type -> Type
    removeReferences refs = go
     where
      go (TGeneric n) | n `Set.member` refs = TNever -- Replace with bottom type
      go (TUnion ts) = 
        let cleaned = Set.map go ts
            -- Remove TNever from unions
            filtered = Set.filter (/= TNever) cleaned
         in if Set.null filtered then TNever else mkUnion (Set.toList filtered)
      go (TIntersection ts) = 
        let cleaned = Set.map go ts
            -- Remove TUnknown from intersections
            filtered = Set.filter (/= TUnknown) cleaned
         in if Set.null filtered then TUnknown else mkIntersection (Set.toList filtered)
      go (TFunction args ret) = TFunction (map go args) (go ret)
      go (TApplication name args) = TApplication name (map go args)
      go t = t
    
    -- Helper to remove duplicates
    nub :: Eq a => [a] -> [a]
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)
