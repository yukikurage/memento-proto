module Language.Memento.TypeSolver.Subtype where

import qualified Data.Set as Set
import Language.Memento.TypeSolver.Types

-- Check if t1 <: t2 (only for types without variables)
isSubtype :: Type -> Type -> Bool
isSubtype t1 t2 | containsVar t1 || containsVar t2 = error "isSubtype called with type variables"
isSubtype t1 t2 = isSubtype' t1 t2

isSubtype' :: Type -> Type -> Bool
-- Top is supertype of everything
isSubtype' _ TTop = True
-- Bottom is subtype of everything
isSubtype' TBottom _ = True
-- Reflexivity
isSubtype' t1 t2 | t1 == t2 = True

-- Base types
isSubtype' TNumber TNumber = True
isSubtype' TBool TBool = True
isSubtype' TString TString = True
isSubtype' TNever _ = True
isSubtype' _ TUnknown = True

-- Constructor types (only equal constructors are subtypes)
isSubtype' (TConstructor name1) (TConstructor name2) = name1 == name2

-- Literal types
isSubtype' (TLiteral (LNumber _)) TNumber = True
isSubtype' (TLiteral (LBool _)) TBool = True
isSubtype' (TLiteral (LString _)) TString = True

-- Function types (contravariant in argument, covariant in result)
isSubtype' (TFunction a1 r1) (TFunction a2 r2) = 
  isSubtype' a2 a1 && isSubtype' r1 r2

-- Union types
isSubtype' t1 (TUnion ts) = any (isSubtype' t1) (Set.toList ts)
isSubtype' (TUnion ts) t2 = all (`isSubtype'` t2) (Set.toList ts)

-- Intersection types
isSubtype' t1 (TIntersection ts) = all (isSubtype' t1) (Set.toList ts)
isSubtype' (TIntersection ts) t2 = any (`isSubtype'` t2) (Set.toList ts)

-- Default case
isSubtype' _ _ = False

-- Check if two types are equivalent
isEquivalent :: Type -> Type -> Bool
isEquivalent t1 t2 = isSubtype t1 t2 && isSubtype t2 t1