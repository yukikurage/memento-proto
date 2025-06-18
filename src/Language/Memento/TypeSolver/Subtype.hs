module Language.Memento.TypeSolver.Subtype where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
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

-- Constructor types (only equal constructors are subtypes)
isSubtype' _ (TConstructor name1) (TConstructor name2) = name1 == name2

-- Literal types
isSubtype' _ (TLiteral (LNumber _)) TNumber = True
isSubtype' _ (TLiteral (LBool _)) TBool = True
isSubtype' _ (TLiteral (LString _)) TString = True

-- Function types (contravariant in argument, covariant in result)
isSubtype' variances (TFunction a1 r1) (TFunction a2 r2) = 
  isSubtype' variances a2 a1 && isSubtype' variances r1 r2

-- Union types
isSubtype' variances t1 (TUnion ts) = any (isSubtype' variances t1) (Set.toList ts)
isSubtype' variances (TUnion ts) t2 = all (\t -> isSubtype' variances t t2) (Set.toList ts)

-- Intersection types
isSubtype' variances t1 (TIntersection ts) = all (isSubtype' variances t1) (Set.toList ts)
isSubtype' variances (TIntersection ts) t2 = any (\t -> isSubtype' variances t t2) (Set.toList ts)

-- Generic types (polymorphic type parameters)
isSubtype' _ (TGeneric name1) (TGeneric name2) = name1 == name2

-- Type application with variance-aware subtyping!
isSubtype' variances (TApplication base1 args1) (TApplication base2 args2) =
  -- Base types must be subtypes
  isSubtype' variances base1 base2 && 
  -- Must have same number of arguments
  length args1 == length args2 && 
  -- Check arguments according to their variance
  checkArgumentVariances variances base1 args1 args2
  where
    checkArgumentVariances :: TypeConstructorVariances -> Type -> [Type] -> [Type] -> Bool
    checkArgumentVariances varMap baseType args1 args2 = 
      case extractConstructorName baseType of
        Nothing -> 
          -- No variance info available, fall back to invariant (conservative)
          all (uncurry isEquivalent') (zip args1 args2)
        Just constructorName ->
          case Map.lookup constructorName varMap of
            Nothing -> 
              -- No variance info for this constructor, fall back to invariant
              all (uncurry isEquivalent') (zip args1 args2)
            Just varianceMap ->
              -- Use variance information to check arguments
              checkArgumentsWithVariance varMap varianceMap args1 args2 0
    
    -- Helper to extract constructor name from type  
    extractConstructorName :: Type -> Maybe T.Text
    extractConstructorName (TConstructor name) = Just name
    extractConstructorName (TVar (TypeVar name)) = Just name  -- Type variables can represent constructors
    extractConstructorName _ = Nothing
    
    -- Check arguments using variance information
    checkArgumentsWithVariance :: TypeConstructorVariances -> VarianceMap -> [Type] -> [Type] -> Int -> Bool
    checkArgumentsWithVariance _ _ [] [] _ = True
    checkArgumentsWithVariance varMap variances (arg1:args1) (arg2:args2) index = 
      let -- Get variance for this argument position by index
          -- Convert variance map to list and get by position
          varianceList = Map.elems variances
          variance = if index < length varianceList 
                    then varianceList !! index 
                    else Invariant  -- Default to invariant if index out of bounds
          -- Check this argument according to its variance
          argCheck = case variance of
            Covariant -> isSubtype' varMap arg1 arg2
            Contravariant -> isSubtype' varMap arg2 arg1  -- Flipped!
            Invariant -> isEquivalent' arg1 arg2
      in argCheck && checkArgumentsWithVariance varMap variances args1 args2 (index + 1)
    
    -- Helper for type equivalence (both directions subtyping)
    isEquivalent' t1 t2 = isSubtype' variances t1 t2 && isSubtype' variances t2 t1

-- Default case
isSubtype' _ _ _ = False

-- Check if two types are equivalent
isEquivalent :: Type -> Type -> Bool
isEquivalent t1 t2 = isSubtype t1 t2 && isSubtype t2 t1