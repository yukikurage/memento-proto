module Language.Memento.TypeSolver.Normalize where

import qualified Data.Set as Set
import Language.Memento.TypeSolver.Types
import Language.Memento.TypeSolver.Subtype

-- Normalize a type according to the rules in TYPE_SOLVER.md
normalize :: Type -> Type
normalize t = fixpoint normalizeStep t
  where
    fixpoint f x = 
      let x' = f x
      in if x == x' then x else fixpoint f x'

normalizeStep :: Type -> Type
normalizeStep TTop = TTop
normalizeStep TBottom = TBottom
normalizeStep TNumber = TNumber
normalizeStep TBool = TBool
normalizeStep TString = TString
normalizeStep TNever = TNever
normalizeStep TUnknown = TUnknown
normalizeStep lit@(TLiteral _) = lit
normalizeStep var@(TVar _) = var
normalizeStep constructor@(TConstructor _) = constructor
normalizeStep (TFunction t1 t2) = TFunction (normalize t1) (normalize t2)
normalizeStep (TUnion ts) = normalizeUnion (Set.map normalize ts)
normalizeStep (TIntersection ts) = normalizeIntersection (Set.map normalize ts)

-- Normalize union types
normalizeUnion :: Set.Set Type -> Type
normalizeUnion ts =
  let ts' = Set.toList ts
      -- Remove Bottom
      withoutBottom = filter (/= TBottom) ts'
      -- Check for Top
      hasTop = TTop `elem` ts'
      -- Remove duplicates (handled by Set)
      -- Flatten nested unions
      flattened = concatMap flattenUnion withoutBottom
      -- Remove subtypes if we can check without variables
      simplified = removeSubtypes flattened
  in case (hasTop, simplified) of
    (True, _) -> TTop
    (False, []) -> TBottom
    (False, [t]) -> t
    (False, ts'') -> TUnion (Set.fromList ts'')

-- Normalize intersection types  
normalizeIntersection :: Set.Set Type -> Type
normalizeIntersection ts =
  let ts' = Set.toList ts
      -- Remove Top
      withoutTop = filter (/= TTop) ts'
      -- Check for Bottom
      hasBottom = TBottom `elem` ts'
      -- Flatten nested intersections
      flattened = concatMap flattenIntersection withoutTop
      -- Remove supertypes if we can check without variables
      simplified = removeSupertypes flattened
  in case (hasBottom, simplified) of
    (True, _) -> TBottom
    (False, []) -> TTop
    (False, [t]) -> t
    (False, ts'') -> TIntersection (Set.fromList ts'')

-- Flatten union types
flattenUnion :: Type -> [Type]
flattenUnion (TUnion ts) = concatMap flattenUnion (Set.toList ts)
flattenUnion t = [t]

-- Flatten intersection types
flattenIntersection :: Type -> [Type]
flattenIntersection (TIntersection ts) = concatMap flattenIntersection (Set.toList ts)
flattenIntersection t = [t]

-- Remove subtypes from a list (only for types without variables)
removeSubtypes :: [Type] -> [Type]
removeSubtypes [] = []
removeSubtypes (t:ts) = 
  if any (\t' -> t /= t' && not (containsVar t || containsVar t') && isSubtype t t') ts
  then removeSubtypes ts
  else t : removeSubtypes (filter (\t' -> not (t /= t' && not (containsVar t || containsVar t') && isSubtype t' t)) ts)

-- Remove supertypes from a list (only for types without variables)
removeSupertypes :: [Type] -> [Type]
removeSupertypes [] = []
removeSupertypes (t:ts) = 
  if any (\t' -> t /= t' && not (containsVar t || containsVar t') && isSubtype t' t) ts
  then removeSupertypes ts
  else t : removeSupertypes (filter (\t' -> not (t /= t' && not (containsVar t || containsVar t') && isSubtype t t')) ts)

-- Special normalization for function types in unions/intersections
-- (A -> B) | (C -> D) = (A & C -> B | D)
normalizeFunctionUnion :: [Type] -> Maybe Type
normalizeFunctionUnion types =
  let functions = [(t1, t2) | TFunction t1 t2 <- types]
  in case functions of
    [] -> Nothing
    _ -> Just $ TFunction 
           (mkIntersection (map fst functions))
           (mkUnion (map snd functions))

-- (A -> B) & (C -> D) = (A | C -> B & D)
normalizeFunctionIntersection :: [Type] -> Maybe Type
normalizeFunctionIntersection types =
  let functions = [(t1, t2) | TFunction t1 t2 <- types]
  in case functions of
    [] -> Nothing
    _ -> Just $ TFunction 
           (mkUnion (map fst functions))
           (mkIntersection (map snd functions))