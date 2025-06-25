module Language.Memento.TypeSolver.Normalize where

import Control.Monad.Error.Class (MonadError)
import qualified Data.Set as Set
import Language.Memento.TypeSolver.Subtype
import Language.Memento.TypeSolver.Types

normalize ::
  forall m.
  (MonadError TypeError m) =>
  Type ->
  m Type
normalize t = case t of
  TNumber -> pure TNumber
  TBool -> pure TBool
  TString -> pure TString
  TNever -> pure TNever
  TUnknown -> pure TUnknown
  TLiteral lit -> pure $ TLiteral lit
  TVar name -> pure $ TVar name
  TFunction args t2 -> do
    normalizedArg <- mapM normalize args
    TFunction normalizedArg <$> normalize t2
  TUnion ts ->
    normalizeUnion . Set.fromList
      =<< mapM normalize (Set.toList ts) -- Union has special normalization
  TIntersection ts ->
    normalizeIntersection . Set.fromList
      =<< mapM normalize (Set.toList ts) -- Union has special normalization
  TGeneric name -> pure $ TGeneric name
  TApplication name args -> TApplication name <$> mapM normalize args

-- Normalize union types
normalizeUnion ::
  forall m.
  (MonadError TypeError m) =>
  Set.Set Type ->
  m Type
normalizeUnion ts = do
  let ts' = Set.toList ts
      flattened = concatMap flattenUnion ts'
      withoutNever = filter (/= TNever) flattened
      hasUnknown = TUnknown `elem` flattened
  if hasUnknown
    then
      pure TUnknown
    else
      -- Remove subtypes if we can check without variables
      -- removeSubtypes is heavy computation, so we avoid it for now
      -- simplified <- removeSubtypes withoutNever
      case withoutNever of
        [] -> pure TNever
        [t] -> pure t
        ts'' -> pure $ TUnion (Set.fromList ts'')

-- Normalize intersection types
normalizeIntersection ::
  forall m.
  (MonadError TypeError m) =>
  Set.Set Type ->
  m Type
normalizeIntersection ts = do
  let ts' = Set.toList ts
      flattened = concatMap flattenUnion ts'
      withoutUnknown = filter (/= TUnknown) flattened
      hasNever = TNever `elem` flattened
  if hasNever
    then
      pure TNever
    else
      -- Remove subtypes if we can check without variables
      -- simplified <- removeSupertypes withoutUnknown
      case withoutUnknown of
        [] -> pure TNever
        [t] -> pure t
        ts'' -> pure $ TUnion (Set.fromList ts'')

-- Flatten union types
flattenUnion :: Type -> [Type]
flattenUnion t = case t of
  (TUnion ts) -> concatMap flattenUnion (Set.toList ts)
  _ -> [t]

-- Flatten intersection types
flattenIntersection :: Type -> [Type]
flattenIntersection t = case t of
  (TIntersection ts) -> concatMap flattenIntersection (Set.toList ts)
  _ -> [t]

-- Remove subtypes from a list (only for types without variables)
-- removeSubtypes ::
--   forall m.
--   (MonadError TypeError m) =>
--   TypeConstructorVariances ->
--   GenericBoundsMap ->
--   [Type] ->
--   m [Type]
-- removeSubtypes [] = pure []
-- removeSubtypes (t : ts) =
--   if any (\t' -> andM [pure $ not (containsVar t || containsVar t'), isSubtype t t']) ts
--     then removeSubtypes ts
--     else t : removeSubtypes (filter (\t' -> not (t /= t' && not (containsVar t || containsVar t') && isSubtype t' t)) ts)
-- removeSubtypes ts = case ts of
--   [] -> pure []
--   (t : ts') -> do

-- Remove supertypes from a list (only for types without variables)
-- removeSupertypes ::
--   forall m.
--   (MonadError TypeError m) =>
--   [Type] ->
--   m [Type]
-- removeSupertypes [] = []
-- removeSupertypes (t : ts) =
--   if any (\t' -> t /= t' && not (containsVar t || containsVar t') && isSubtype t' t) ts
--     then removeSupertypes ts
--     else t : removeSupertypes (filter (\t' -> not (t /= t' && not (containsVar t || containsVar t') && isSubtype t t')) ts)

-- -- Special normalization for function types in unions/intersections
-- -- (A -> B) | (C -> D) = (A & C -> B | D)
-- normalizeFunctionUnion :: [Type] -> Maybe Type
-- normalizeFunctionUnion types =
--   let functions = [(t1, t2) | TFunction t1 t2 <- types]
--   in case functions of
--     [] -> Nothing
--     _ -> Just $ TFunction
--            (mkIntersection (map fst functions))
--            (mkUnion (map snd functions))

-- -- (A -> B) & (C -> D) = (A | C -> B & D)
-- normalizeFunctionIntersection :: [Type] -> Maybe Type
-- normalizeFunctionIntersection types =
--   let functions = [(t1, t2) | TFunction t1 t2 <- types]
--   in case functions of
--     [] -> Nothing
--     _ -> Just $ TFunction
--            (mkUnion (map fst functions))
--            (mkIntersection (map snd functions))
