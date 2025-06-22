module Language.Memento.TypeSolver.Subtype where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace)
import Language.Memento.TypeSolver.Types

-- Check if t1 <: t2 (only for types without variables)
isSubtype :: Type -> Type -> Bool
isSubtype t1 t2 | containsVar t1 || containsVar t2 = error "isSubtype called with type variables"
isSubtype t1 t2 = isSubtypeWithVariances Map.empty t1 t2

-- Check subtyping with variance information
isSubtypeWithVariances :: TypeConstructorVariances -> Type -> Type -> Bool
isSubtypeWithVariances variances t1 t2 | containsVar t1 || containsVar t2 = error "isSubtypeWithVariances called with type variables"
isSubtypeWithVariances variances t1 t2 = isSubtype' variances t1 t2

isSubtype' :: TypeConstructorVariances -> Type -> Type -> Bool
-- TUnknown is supertype of everything
isSubtype' _ _ TUnknown = True
-- TNever is subtype of everything
isSubtype' _ TNever _ = True
-- Reflexivity
isSubtype' _ t1 t2 | t1 == t2 = True
-- Base types
isSubtype' _ TNumber TNumber = True
isSubtype' _ TBool TBool = True
isSubtype' _ TString TString = True
-- Literal types
isSubtype' _ (TLiteral (LNumber _)) TNumber = True
isSubtype' _ (TLiteral (LBool _)) TBool = True
isSubtype' _ (TLiteral (LString _)) TString = True
-- Decomposition for Boolean
isSubtype' variances TBool t2 = isSubtype' variances (TLiteral $ LBool True) t2 && isSubtype' variances (TLiteral $ LBool False) t2
-- Function types (contravariant in argument, covariant in result)
isSubtype' variances (TFunction args1 r1) (TFunction args2 r2) =
  length args1 == length args2
    && all (uncurry (flip (isSubtype' variances))) (zip args1 args2)
    && isSubtype' variances r1 r2 -- contravariant in arguments
    -- covariant in result

-- Union types
isSubtype' variances (TUnion ts) t2 = all (\t -> isSubtype' variances t t2) (Set.toList ts)
isSubtype' variances t1 (TUnion ts) = any (isSubtype' variances t1) (Set.toList ts) -- NOTE: Possible to have false-positives here

-- Intersection types
isSubtype' variances t1 (TIntersection ts) = all (isSubtype' variances t1) (Set.toList ts)
isSubtype' variances (TIntersection ts) t2 = any (\t -> isSubtype' variances t t2) (Set.toList ts) -- NOTE: Possible to have false-positives here

-- Generic types (polymorphic type parameters)
isSubtype' _ (TGeneric name1) (TGeneric name2) = name1 == name2
isSubtype' _ (TGeneric _) t2 = t2 == TUnknown -- Generic types are subtypes of unknown (but not vice versa)
isSubtype' _ t1 (TGeneric _) = t1 == TNever -- Generic types are supertypes of never (but not vice versa)

-- Type application with variance-aware subtyping!
isSubtype' varMap (TApplication typeConsName1 args1) (TApplication typeConsName2 args2)
  | typeConsName1 /= typeConsName2 =
      -- Base types must match exactly for application subtyping
      False
  | otherwise =
      -- Must have same number of arguments
      length args1 == length args2
        &&
        -- Check arguments according to their variance
        checkArgumentVariances typeConsName1 args1 args2
 where
  checkArgumentVariances :: T.Text -> [Type] -> [Type] -> Bool
  checkArgumentVariances typeConsName args1 args2 =
    case Map.lookup typeConsName varMap of
      Nothing -> error $ "No variance information for type constructor: " ++ T.unpack typeConsName
      Just variances ->
        -- Use variance information to check arguments
        checkArgumentsWithVariance variances args1 args2

  -- Check arguments using variance information
  checkArgumentsWithVariance :: [Variance] -> [Type] -> [Type] -> Bool
  checkArgumentsWithVariance _ [] [] = True
  checkArgumentsWithVariance (variance : variances) (arg1 : args1) (arg2 : args2) =
    let
      -- Check this argument according to its variance
      argCheck = case variance of
        Covariant -> isSubtype' varMap arg1 arg2
        Contravariant -> isSubtype' varMap arg2 arg1 -- Flipped!
        Invariant -> isEquivalent' varMap Map.empty arg1 arg2
        Bivariant -> isSubtype' varMap arg1 arg2 || isSubtype' varMap arg2 arg1
     in
      argCheck && checkArgumentsWithVariance variances args1 args2

-- Default case
isSubtype' _ _ _ = False

-- Subtype checking with generic bounds and recursion  limiting
isSubtypeWithBounds' :: TypeConstructorVariances -> GenericBoundsMap -> Type -> Type -> Bool
--  limit reached, be more conservative:
-- If both sides are generics with circular bounds, assume true (equivalence)
-- Otherwise assume false to break cycles
-- TUnknown is supertype of everything
isSubtypeWithBounds' _ _ _ TUnknown = True
-- TNever is subtype of everything
isSubtypeWithBounds' _ _ TNever _ = True
-- Reflexivity
isSubtypeWithBounds' _ _ t1 t2 | t1 == t2 = True
-- Base types
isSubtypeWithBounds' _ _ TNumber TNumber = True
isSubtypeWithBounds' _ _ TBool TBool = True
isSubtypeWithBounds' _ _ TString TString = True
-- Literal types
isSubtypeWithBounds' _ _ (TLiteral (LNumber _)) TNumber = True
isSubtypeWithBounds' _ _ (TLiteral (LBool _)) TBool = True
isSubtypeWithBounds' _ _ (TLiteral (LString _)) TString = True
-- Decomposition for Boolean
isSubtypeWithBounds' variances bounds TBool t2 =
  isSubtypeWithBounds' variances bounds (TLiteral $ LBool True) t2
    && isSubtypeWithBounds' variances bounds (TLiteral $ LBool False) t2
-- Function types (contravariant in argument, covariant in result)
isSubtypeWithBounds' variances bounds (TFunction args1 r1) (TFunction args2 r2) =
  length args1 == length args2
    && all (uncurry (flip (isSubtypeWithBounds' variances bounds))) (zip args1 args2)
    && isSubtypeWithBounds' variances bounds r1 r2
-- Union types
isSubtypeWithBounds' variances bounds (TUnion ts) t2 =
  all (\t -> isSubtypeWithBounds' variances bounds t t2) (Set.toList ts)
isSubtypeWithBounds' variances bounds t1 (TUnion ts) =
  any (isSubtypeWithBounds' variances bounds t1) (Set.toList ts)
-- Intersection types
isSubtypeWithBounds' variances bounds t1 (TIntersection ts) = all (isSubtypeWithBounds' variances bounds t1) (Set.toList ts)
isSubtypeWithBounds' variances bounds (TIntersection ts) t2 =
  any (\t -> isSubtypeWithBounds' variances bounds t t2) (Set.toList ts)
-- Generic types with bounds
isSubtypeWithBounds' variances bounds (TGeneric name1) (TGeneric name2)
  | name1 == name2 = True
  | otherwise =
      let GenericBounds upperOf1 _ = lookupGenericBounds name1 bounds
          GenericBounds _ lowerOf2 = lookupGenericBounds name2 bounds
       in elem (TGeneric name2) upperOf1 || elem (TGeneric name1) lowerOf2
isSubtypeWithBounds' variances bounds (TGeneric name) t2 =
  let
    GenericBounds _ lowers = lookupGenericBounds name bounds
    lowersExceptGeneric = filter (not . containsGeneric) lowers
   in
    any (\lower -> isSubtypeWithBounds' variances bounds lower t2) lowersExceptGeneric
isSubtypeWithBounds' variances bounds t1 (TGeneric name) =
  let
    GenericBounds upperOfName lowers = lookupGenericBounds name bounds
    upperOfNameExceptGeneric = filter (not . containsGeneric) upperOfName
   in
    any (\upper -> isSubtypeWithBounds' variances bounds t1 upper) upperOfNameExceptGeneric
isSubtypeWithBounds' varMap bounds (TApplication typeConsName1 args1) (TApplication typeConsName2 args2)
  | typeConsName1 /= typeConsName2 = False
  | otherwise =
      length args1 == length args2 && checkArgumentVariances typeConsName1 args1 args2
 where
  checkArgumentVariances :: T.Text -> [Type] -> [Type] -> Bool
  checkArgumentVariances typeConsName args1 args2 =
    case Map.lookup typeConsName varMap of
      Nothing -> error $ "No variance information for type constructor: " ++ T.unpack typeConsName
      Just variances -> checkArgumentsWithVariance variances args1 args2
  checkArgumentsWithVariance :: [Variance] -> [Type] -> [Type] -> Bool
  checkArgumentsWithVariance _ [] [] = True
  checkArgumentsWithVariance (variance : variances) (arg1 : args1) (arg2 : args2) =
    let argCheck = case variance of
          Covariant -> isSubtypeWithBounds' varMap bounds arg1 arg2
          Contravariant -> isSubtypeWithBounds' varMap bounds arg2 arg1
          Invariant -> isSubtypeWithBounds' varMap bounds arg1 arg2 && isSubtypeWithBounds' varMap bounds arg2 arg1
          Bivariant -> isSubtypeWithBounds' varMap bounds arg1 arg2 || isSubtypeWithBounds' varMap bounds arg2 arg1
     in argCheck && checkArgumentsWithVariance variances args1 args2
-- Default case
isSubtypeWithBounds' _ _ _ _ = False

-- Check if two generics are equivalent based on circular bounds
-- If A's upper bound contains B and B's upper bound contains A,
-- and A's lower bound contains B and B's lower bound contains A,
-- then A and B are equivalent
-- areEquivalentGenerics :: T.Text -> T.Text -> GenericBoundsMap -> Bool
-- areEquivalentGenerics name1 name2 bounds =
--   let GenericBounds lower1 upper1 = lookupGenericBounds name1 bounds
--       GenericBounds lower2 upper2 = lookupGenericBounds name2 bounds
--       -- Check if name2 appears in name1's bounds and vice versa
--       name1InName2Bounds = containsGenericInType name1 lower2 || containsGenericInType name1 upper2
--       name2InName1Bounds = containsGenericInType name2 lower1 || containsGenericInType name2 upper1
--    in name1InName2Bounds && name2InName1Bounds

isEquivalent' :: TypeConstructorVariances -> GenericBoundsMap -> Type -> Type -> Bool
isEquivalent' variances bounds t1 t2
  | containsVar t1 || containsVar t2 = error "isEquivalent called with type variables"
  | otherwise = isSubtypeWithBounds' variances bounds t1 t2 && isSubtypeWithBounds' variances bounds t2 t1

-- Check if two types are equivalent (bidirectional subtyping)
