{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Language.Memento.TypeSolver.Types where

import Control.Monad (forM, zipWithM)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (List)
import Language.Memento.Syntax.Metadata (Metadata)

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

newtype TypeVar = TypeVar T.Text
  deriving (Eq, Ord, Show)

formatType :: Type -> T.Text
formatType t = case t of
  TNumber -> "number"
  TBool -> "boolean"
  TString -> "string"
  TNever -> "never"
  TUnknown -> "unknown"
  TLiteral (LNumber n) -> T.pack $ show n
  TLiteral (LBool b) -> T.pack $ show b
  TLiteral (LString s) -> "\"" <> s <> "\""
  TVar (TypeVar name) -> "#" <> name
  TFunction args ret ->
    "(("
      <> T.intercalate ", " (map formatType args)
      <> ") => "
      <> formatType ret
      <> ")"
  TUnion ts ->
    "(" <> T.intercalate " | " (map formatType (Set.toList ts))
  TIntersection ts ->
    "(" <> T.intercalate " & " (map formatType (Set.toList ts))
  TGeneric name -> name
  TApplication base args ->
    base <> "<" <> T.intercalate ", " (map formatType args) <> ">"

-- Type schemes for polymorphic types (forall quantification)
data TypeScheme = TypeScheme [T.Text] Type -- forall [a1, a2, ...] . Type
  deriving (Eq, Ord, Show)

formatTypeScheme :: TypeScheme -> (T.Text, T.Text)
formatTypeScheme (TypeScheme vars t) =
  let varsStr = if null vars then "" else "<" <> T.intercalate ", " vars <> ">"
      typeStr = formatType t
   in (varsStr, typeStr)

formatTypeEnv :: Map.Map T.Text TypeScheme -> T.Text
formatTypeEnv typeSchemeMap =
  T.intercalate "\n" $
    map
      ( \(k, v) ->
          let (generics, typeStr) = formatTypeScheme v
           in k
                <> " "
                <> generics
                <> " : "
                <> typeStr
      )
      (Map.toList typeSchemeMap)

-- | Represent generics bounds
type Assumption = Constraint

type AssumptionSet = ConstraintSet

-- | Represents bounds of generics, pre computation for better performance
type GenericBoundsMap = Map.Map T.Text (Set.Set Type, Set.Set Type)

lookupGenericBounds :: T.Text -> GenericBoundsMap -> (Set.Set Type, Set.Set Type)
lookupGenericBounds name boundsMap =
  fromMaybe (Set.empty, Set.empty) $ Map.lookup name boundsMap
data TypeError
  = forall f a. UnboundVariable (Maybe (Metadata f a)) T.Text
  | forall f a. UnboundTypeVariable (Maybe (Metadata f a)) T.Text
  | forall f a. TypeNotSubtype (Maybe (Metadata f a)) Type Type
  | forall f a. TypeMismatch (Maybe (Metadata f a)) Type Type
  | SomeTypeError T.Text
  | InternalTypeError T.Text

formatTypeError :: TypeError -> T.Text
formatTypeError err = case err of
  UnboundVariable m name ->
    "Unbound variable: " <> name <> maybe "" (\meta -> " at " <> T.pack (show meta)) m
  UnboundTypeVariable m name ->
    "Unbound type variable: " <> name <> maybe "" (\meta -> " at " <> T.pack (show meta)) m
  TypeNotSubtype m t1 t2 ->
    "Type error: "
      <> formatType t1
      <> " is not a subtype of "
      <> formatType t2
      <> maybe "" (\meta -> " at " <> T.pack (show meta)) m
  TypeMismatch m t1 t2 ->
    "Type error: "
      <> formatType t1
      <> " does not match "
      <> formatType t2
      <> maybe "" (\meta -> " at " <> T.pack (show meta)) m
  SomeTypeError msg ->
    "Type error: " <> msg
  InternalTypeError msg ->
    "Internal type error: " <> msg

-- Variance analysis for type parameters
data Variance
  = Covariant
  | Contravariant
  | Invariant
  | Bivariant
  deriving (Eq, Ord, Show)

-- | Like :   Some |=> [Covariant],  or Either |=> [Covariant, Covariant],  Reader |=> [Contravariant, Covariant] ...
type TypeConstructorVariances = Map.Map T.Text [Variance]

-- ComposeVariance x y is variance y in variance x: e.g., for function type (X => Y) => Z, X is covariant because contravariant (X) in contravariant (X => Y)
composeVariance :: Variance -> Variance -> Variance
composeVariance x y
  | x == Bivariant || y == Bivariant = Bivariant
  | x == Covariant = y
  | y == Covariant = x
  | x == Contravariant = flipVariance y
  | y == Contravariant = flipVariance x
  | otherwise = Invariant -- If both are invariant, result is invariant

-- Operator for composeVariance
infixl 4 |*|

(|*|) :: Variance -> Variance -> Variance
(|*|) = composeVariance

productVariance :: forall f. (Foldable f) => f Variance -> Variance
productVariance = foldr composeVariance Covariant

-- Combine variances when a type parameter appears in multiple positions
combineVariance :: Variance -> Variance -> Variance
combineVariance x y
  | x == Bivariant = y
  | y == Bivariant = x
  | x == Covariant && y == Covariant = Covariant
  | x == Contravariant && y == Contravariant = Contravariant
  | otherwise = Invariant -- One side is Invariant, or permutation of [Covariant, Contravairant]

-- Operator for combineVariance
infixl 4 |+|

(|+|) :: Variance -> Variance -> Variance
(|+|) = combineVariance

sumVariance :: forall f. (Foldable f) => f Variance -> Variance
sumVariance = foldr combineVariance Bivariant

-- Flip variance when entering contravariant position (like function arguments)
flipVariance :: Variance -> Variance
flipVariance v = case v of
  Covariant -> Contravariant
  Contravariant -> Covariant
  Invariant -> Invariant
  Bivariant -> Bivariant

-- Analyze variance of a specific type parameter in a type expression
analyzeVariance ::
  forall m.
  (MonadError TypeError m) =>
  TypeConstructorVariances ->
  T.Text ->
  Type ->
  m Variance
analyzeVariance varsMap paramName typeExpr = case typeExpr of
  -- Base cases - no variance contribution
  TNumber -> pure Bivariant
  TBool -> pure Bivariant
  TString -> pure Bivariant
  TNever -> pure Bivariant
  TUnknown -> pure Bivariant
  TLiteral _ -> pure Bivariant
  TVar (TypeVar name) ->
    throwError $
      InternalTypeError $
        "analyzeVariance: " <> "analyzeVariance should not be called with type variables, but called with a type variable `" <> name <> "`"
  -- Generic type parameter - this is where we find our target!
  TGeneric name
    | name == paramName -> pure Covariant
    | otherwise -> pure Bivariant
  -- Function type - argument is contravariant, return is covariant
  TFunction argsType retType -> do
    argVariances <- forM argsType $ analyzeVariance varsMap paramName
    let argVarianceFlipped = sumVariance $ (Contravariant |*|) <$> argVariances
    retVariance <- analyzeVariance varsMap paramName retType
    pure $ argVarianceFlipped |+| retVariance
  -- Union type - maintains current variance for all members
  TUnion types -> do
    variances <- forM (Set.toList types) $ analyzeVariance varsMap paramName
    pure $ sumVariance variances
  -- Intersection type - maintains current variance for all members
  TIntersection types -> do
    variances <- forM (Set.toList types) $ analyzeVariance varsMap paramName
    pure $ sumVariance variances
  -- Type application - need to look up variance of the other types
  TApplication name argTypes
    | Just variances <- Map.lookup name varsMap -> do
        argVariances <-
          zipWithM
            ( \arg var -> (var |*|) <$> analyzeVariance varsMap paramName arg
            )
            argTypes
            variances
        pure $ sumVariance argVariances
    | otherwise -> throwError $ UnboundTypeVariable Nothing name

-- Constraints
data Constraint = Subtype Type Type
  deriving (Eq, Ord, Show)

-- Format type errors
formatConstraintSet :: ConstraintSet -> T.Text
formatConstraintSet constraints =
  "All Constraints:\n" <> T.unlines (map formatConstraint $ Set.toList constraints)
 where
  formatConstraint (Subtype t1 t2) =
    "  " <> formatType t1 <> " <: " <> formatType t2

-- Constraint set for solving
type ConstraintSet = Set.Set Constraint

-- Result of type solving
data SolveResult
  = Success
  | Contradiction T.Text
  deriving (Show, Eq)

-- Helper constructors
mkUnion :: [Type] -> Type
mkUnion ts = case ts of
  [] -> TNever
  [t] -> t
  _ -> TUnion (Set.fromList ts)

mkIntersection :: [Type] -> Type
mkIntersection ts = case ts of
  [] -> TUnknown
  [t] -> t
  _ -> TIntersection (Set.fromList ts)

-- Check if type contains variables
containsVar :: Type -> Bool
containsVar t = case t of
  TNumber -> False
  TBool -> False
  TString -> False
  TNever -> False
  TUnknown -> False
  TLiteral _ -> False
  TVar _ -> True
  TFunction args ret -> any containsVar args || containsVar ret
  TUnion ts -> any containsVar (Set.toList ts)
  TIntersection ts -> any containsVar (Set.toList ts)
  TGeneric _ -> False -- Generic parameters are not unification variables
  TApplication _ args -> any containsVar args

containsGeneric :: Type -> Bool
containsGeneric t = case t of
  TNumber -> False
  TBool -> False
  TString -> False
  TNever -> False
  TUnknown -> False
  TLiteral _ -> False
  TVar _ -> True -- NOTE: Potentialy contains generics
  TFunction args ret -> any containsGeneric args || containsGeneric ret
  TUnion ts -> any containsGeneric (Set.toList ts)
  TIntersection ts -> any containsGeneric (Set.toList ts)
  TGeneric _ -> True -- Generic parameters are what we're looking for
  TApplication _ args -> any containsGeneric args

-- Get all type variables in a type
typeVars :: Type -> Set.Set TypeVar
typeVars t = case t of
  TNumber -> Set.empty
  TBool -> Set.empty
  TString -> Set.empty
  TNever -> Set.empty
  TUnknown -> Set.empty
  TLiteral _ -> Set.empty
  TVar tvar -> Set.singleton tvar
  TFunction args ret -> Set.unions (typeVars <$> args) `Set.union` typeVars ret
  TUnion ts -> Set.unions (typeVars <$> Set.toList ts)
  TIntersection ts -> Set.unions (typeVars <$> Set.toList ts)
  TGeneric _ -> Set.empty
  TApplication _ args -> Set.unions (typeVars <$> args)

-- Apply substitution to a type
applySubst :: TypeVar -> Type -> Type -> Type
applySubst x t1 t2 = case t2 of
  TNumber -> TNumber
  TBool -> TBool
  TString -> TString
  TNever -> TNever
  TUnknown -> TUnknown
  TLiteral _ -> t2 -- Literals are unchanged
  TVar tvar | x == tvar -> t1
  TVar _ -> t2
  TFunction args ret -> TFunction (applySubst x t1 <$> args) (applySubst x t1 ret)
  TUnion ts -> TUnion $ Set.map (applySubst x t1) ts
  TIntersection ts -> TIntersection $ Set.map (applySubst x t1) ts
  TGeneric _ -> t2 -- Generic parameters are unchanged
  TApplication name args -> TApplication name (applySubst x t1 <$> args)

-- Apply substitution to a constraint
applySubstConstraint :: TypeVar -> Type -> Constraint -> Constraint
applySubstConstraint x t (Subtype t1 t2) = Subtype (applySubst x t t1) (applySubst x t t2)

applySubstConstraintSet :: TypeVar -> Type -> ConstraintSet -> ConstraintSet
applySubstConstraintSet x t = Set.map (applySubstConstraint x t)

-- Substitute generic type parameters with typesType -> Type
substGenerics ::
  forall m.
  (MonadError TypeError m) =>
  Map.Map T.Text Type ->
  Type ->
  m Type
substGenerics s t = case t of
  TNumber -> pure TNumber
  TBool -> pure TBool
  TString -> pure TString
  TNever -> pure TNever
  TUnknown -> pure TUnknown
  TLiteral _ -> pure t -- Literals are unchanged
  TVar _ -> throwError $ InternalTypeError ""
  TFunction args ret -> do
    args' <- forM args $ substGenerics s
    ret' <- substGenerics s ret
    pure $ TFunction args' ret'
  TUnion ts -> do
    ts' <- forM (Set.toList ts) $ substGenerics s
    pure $ TUnion $ Set.fromList ts'
  TIntersection ts -> do
    ts' <- forM (Set.toList ts) $ substGenerics s
    pure $ TIntersection $ Set.fromList ts'
  TGeneric name -> case Map.lookup name s of
    Just t' -> pure t'
    Nothing -> pure $ TGeneric name
  TApplication name args -> do
    args' <- forM args $ substGenerics s
    pure $ TApplication name args'
