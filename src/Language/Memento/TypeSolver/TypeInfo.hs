{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.TypeSolver.TypeInfo where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Language.Memento.Data.HCoproduct (HVoid, Injective (hProject), (:+:) (HInjL, HInjR))
import Language.Memento.Data.HFix
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Data.HProduct
import Language.Memento.Syntax
import Language.Memento.Syntax.Metadata
import Language.Memento.Syntax.Tag (KExpr, KLiteral, KType, KVariable)
import qualified Language.Memento.TypeSolver.Types as TSTypes

-- ============================================================================
-- Type Information for Different AST Node Kinds
-- ============================================================================

-- | Type information for variable nodes (KVariable)
data VariableTypeInfo (f :: Type -> Type) a where
  VariableTypeInfo ::
    { vtiInferredType :: TSTypes.Type
    -- ^ Inferred type for this variable
    , vtiTypeScheme :: Maybe TSTypes.TypeScheme
    -- ^ Type scheme if it's polymorphic
    } ->
    VariableTypeInfo f KVariable

deriving instance Show (VariableTypeInfo f a)
deriving instance Eq (VariableTypeInfo f a)

instance HFunctor VariableTypeInfo where
  hmap _ (VariableTypeInfo t s) = VariableTypeInfo t s

-- | Type information for expression nodes (KExpr)
data ExprTypeInfo (f :: Type -> Type) a where
  ExprTypeInfo ::
    { etiInferredType :: TSTypes.Type
    -- ^ Inferred type for this expression
    , etiLocalBindings :: Map.Map T.Text TSTypes.Type
    -- ^ Local variable bindings in scope
    } ->
    ExprTypeInfo f KExpr

deriving instance Show (ExprTypeInfo f a)
deriving instance Eq (ExprTypeInfo f a)

instance HFunctor ExprTypeInfo where
  hmap _ (ExprTypeInfo t b) = ExprTypeInfo t b

-- | Type information for literal nodes (KLiteral)
data LiteralTypeInfo (f :: Type -> Type) a where
  LiteralTypeInfo ::
    { ltiInferredType :: TSTypes.Type
    -- ^ Inferred type for this literal
    } ->
    LiteralTypeInfo f KLiteral

deriving instance Show (LiteralTypeInfo f a)
deriving instance Eq (LiteralTypeInfo f a)

instance HFunctor LiteralTypeInfo where
  hmap _ (LiteralTypeInfo t) = LiteralTypeInfo t

-- | Type information for type nodes (KType) - may be empty for now
data MTypeTypeInfo (f :: Type -> Type) a where
  MTypeTypeInfo ::
    { mttiResolvedType :: Maybe TSTypes.Type
    -- ^ Resolved type if available
    } ->
    MTypeTypeInfo f KType

deriving instance Show (MTypeTypeInfo f a)
deriving instance Eq (MTypeTypeInfo f a)

instance HFunctor MTypeTypeInfo where
  hmap _ (MTypeTypeInfo t) = MTypeTypeInfo t

-- ============================================================================
-- Type Information Coproduct
-- ============================================================================

-- | Combined type information for all node types
type TypeInfo =
  HVoid
    :+: VariableTypeInfo
    :+: ExprTypeInfo
    :+: LiteralTypeInfo
    :+: MTypeTypeInfo

-- | Typed AST that includes type information from the type solver
type TypedAST = HFix (HUnit :*: Metadata :*: Syntax :*: TypeInfo)

-- | Extract type information from TypedAST
extractTypeInfo :: (Extractive TypeInfo h) => HFix h a -> TypeInfo (HFix h) a
extractTypeInfo ast = unHFix ast & hExtract

-- ============================================================================
-- Extraction Functions
-- ============================================================================

-- | Extract variable type info (similar to unVariable)
unVariableTypeInfo :: TypeInfo f KVariable -> VariableTypeInfo f KVariable
unVariableTypeInfo typeInfo = fromJust $ hProject typeInfo

-- | Extract expression type info (similar to unExpr)
unExprTypeInfo :: TypeInfo f KExpr -> ExprTypeInfo f KExpr
unExprTypeInfo typeInfo = fromJust $ hProject typeInfo

-- | Extract literal type info (similar to unLiteral)
unLiteralTypeInfo :: TypeInfo f KLiteral -> LiteralTypeInfo f KLiteral
unLiteralTypeInfo typeInfo = fromJust $ hProject typeInfo

-- | Extract type type info (similar to unMType)
unMTypeTypeInfo :: TypeInfo f KType -> MTypeTypeInfo f KType
unMTypeTypeInfo typeInfo = fromJust $ hProject typeInfo
