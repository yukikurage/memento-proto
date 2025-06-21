module Language.Memento.TypeSolver.Subtype where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.TypeSolver.Types

-- Check if t1 <: t2 (only for types without variables)
isSubtype :: Type -> Type -> Bool
isSubtype t1 t2 | containsVar t1 || containsVar t2 = error "isSubtype called with type variables"
isSubtype t1 t2 = isSubtypeWithVariances Map.empty t1 t2

-- Check if t1 <: t2 with assumptions
isSubtypeWithAssumptions :: AssumptionSet -> Type -> Type -> Bool
isSubtypeWithAssumptions assumptions t1 t2 = 
  let t1' = applyAssumptions assumptions t1
      t2' = applyAssumptions assumptions t2
  in if containsVar t1' || containsVar t2'
     then isSubtypeWithAssumptions' assumptions t1' t2'  -- Still has variables, use assumption-aware version
     else isSubtype t1' t2'  -- No variables, use standard version

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
        Invariant -> isEquivalent' varMap arg1 arg2
        Bivariant -> isSubtype' varMap arg1 arg2 || isSubtype' varMap arg2 arg1
     in
      argCheck && checkArgumentsWithVariance variances args1 args2

-- Default case
isSubtype' _ _ _ = False

-- Check if two types are equivalent
isEquivalent :: Type -> Type -> Bool
isEquivalent t1 t2 = isSubtype t1 t2 && isSubtype t2 t1

isEquivalent' :: TypeConstructorVariances -> Type -> Type -> Bool
isEquivalent' variances t1 t2 = isSubtype' variances t1 t2 && isSubtype' variances t2 t1

-- Assumption-aware subtype checking for types that may contain variables
isSubtypeWithAssumptions' :: AssumptionSet -> Type -> Type -> Bool
-- TUnknown is supertype of everything
isSubtypeWithAssumptions' _ _ TUnknown = True
-- TNever is subtype of everything
isSubtypeWithAssumptions' _ TNever _ = True
-- Reflexivity
isSubtypeWithAssumptions' _ t1 t2 | t1 == t2 = True
-- Type variables - check assumptions first
isSubtypeWithAssumptions' assumptions (TVar v1) t2 =
  case Map.lookup v1 assumptions of
    Just t1' -> isSubtypeWithAssumptions assumptions t1' t2
    Nothing -> False  -- Unresolved variable, cannot determine subtyping
isSubtypeWithAssumptions' assumptions t1 (TVar v2) =
  case Map.lookup v2 assumptions of
    Just t2' -> isSubtypeWithAssumptions assumptions t1 t2'
    Nothing -> False  -- Unresolved variable, cannot determine subtyping
-- Base types
isSubtypeWithAssumptions' _ TNumber TNumber = True
isSubtypeWithAssumptions' _ TBool TBool = True
isSubtypeWithAssumptions' _ TString TString = True
-- Literal types
isSubtypeWithAssumptions' _ (TLiteral (LNumber _)) TNumber = True
isSubtypeWithAssumptions' _ (TLiteral (LBool _)) TBool = True
isSubtypeWithAssumptions' _ (TLiteral (LString _)) TString = True
-- Decomposition for Boolean
isSubtypeWithAssumptions' assumptions TBool t2 = 
  isSubtypeWithAssumptions' assumptions (TLiteral $ LBool True) t2 && 
  isSubtypeWithAssumptions' assumptions (TLiteral $ LBool False) t2
-- Function types (contravariant in argument, covariant in result)
isSubtypeWithAssumptions' assumptions (TFunction args1 r1) (TFunction args2 r2) =
  length args1 == length args2
    && all (uncurry (flip (isSubtypeWithAssumptions' assumptions))) (zip args1 args2)
    && isSubtypeWithAssumptions' assumptions r1 r2
-- Union types
isSubtypeWithAssumptions' assumptions (TUnion ts) t2 = 
  all (\t -> isSubtypeWithAssumptions' assumptions t t2) (Set.toList ts)
isSubtypeWithAssumptions' assumptions t1 (TUnion ts) = 
  any (isSubtypeWithAssumptions' assumptions t1) (Set.toList ts)
-- Intersection types
isSubtypeWithAssumptions' assumptions t1 (TIntersection ts) = 
  all (isSubtypeWithAssumptions' assumptions t1) (Set.toList ts)
isSubtypeWithAssumptions' assumptions (TIntersection ts) t2 = 
  any (\t -> isSubtypeWithAssumptions' assumptions t t2) (Set.toList ts)
-- Generic types (polymorphic type parameters)
isSubtypeWithAssumptions' _ (TGeneric name1) (TGeneric name2) = name1 == name2
isSubtypeWithAssumptions' _ (TGeneric _) t2 = t2 == TUnknown
isSubtypeWithAssumptions' _ t1 (TGeneric _) = t1 == TNever
-- Type application - simplified version (no variance checking for now)
isSubtypeWithAssumptions' assumptions (TApplication name1 args1) (TApplication name2 args2)
  | name1 == name2 && length args1 == length args2 =
      all (uncurry (isSubtypeWithAssumptions' assumptions)) (zip args1 args2)
  | otherwise = False
-- Default case
isSubtypeWithAssumptions' _ _ _ = False
