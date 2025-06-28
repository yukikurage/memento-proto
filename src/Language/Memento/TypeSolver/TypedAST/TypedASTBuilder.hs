{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Compositional TypedAST construction
module Language.Memento.TypeSolver.TypedAST.TypedASTBuilder 
  ( -- * TypedAST construction functions
    buildTypedExpr,
    buildTypedVar,
    buildTypedLiteral,
    buildTypedType,
    buildTypedPattern,
    buildTypedLet,
    buildTypedDefinition,
    buildTypedProgram,
    
    -- * Helper functions  
    extractInferredType,
    extractVariableType,
    extractTypeScheme,
    getExpressionType,
    getVariableInfo,
  ) 
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.Memento.Data.HCoproduct ((:+:)(HInjL, HInjR), Injective(hInject))
import Language.Memento.Data.HFix (HFix (HFix))
import Language.Memento.Data.HProduct (HUnit(..), (:*:)((:*:)))
import Language.Memento.Syntax
import Language.Memento.Syntax.Metadata (Metadata(..))
import Language.Memento.Syntax.Tag (KDefinition, KExpr, KLet, KLiteral, KPattern, KProgram, KType, KVariable)
import Language.Memento.TypeSolver.TypedAST.TypeInfo
import qualified Language.Memento.TypeSolver.Core.Types as TSTypes
import Text.Megaparsec (SourcePos)

-- ============================================================================
-- TypedAST Construction Functions
-- ============================================================================

-- | Build a typed expression from raw components
buildTypedExpr :: Metadata TypedAST KExpr -> Syntax TypedAST KExpr -> TSTypes.Type -> Map.Map T.Text TSTypes.Type -> TypedAST KExpr
buildTypedExpr metadata syntax inferredType localBindings =
  let unit = HUnit
      typeInfo = hInject (ExprTypeInfo inferredType localBindings)
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- | Build a typed variable from raw components
buildTypedVar :: Metadata TypedAST KVariable -> Syntax TypedAST KVariable -> TSTypes.Type -> Maybe TSTypes.TypeScheme -> TypedAST KVariable
buildTypedVar metadata syntax inferredType typeScheme =
  let unit = HUnit
      typeInfo = hInject (VariableTypeInfo inferredType typeScheme)
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- | Build a typed literal from raw components
buildTypedLiteral :: Metadata TypedAST KLiteral -> Syntax TypedAST KLiteral -> TSTypes.Type -> TypedAST KLiteral
buildTypedLiteral metadata syntax inferredType =
  let unit = HUnit
      typeInfo = hInject (LiteralTypeInfo inferredType)
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- | Build a typed type node from raw components
buildTypedType :: Metadata TypedAST KType -> Syntax TypedAST KType -> Maybe TSTypes.Type -> TypedAST KType
buildTypedType metadata syntax resolvedType =
  let unit = HUnit
      typeInfo = hInject (MTypeTypeInfo resolvedType)
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- | Build a typed pattern from raw components (uses HVoid for no type info)
buildTypedPattern :: forall a. Metadata TypedAST a -> Syntax TypedAST a -> TypedAST a
buildTypedPattern metadata syntax =
  let unit = HUnit
      typeInfo = HInjL (HInjL (HInjL (HInjL undefined)))  -- HVoid in the coproduct chain
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- | Build a typed let from raw components (uses HVoid for no type info)
buildTypedLet :: forall a. Metadata TypedAST a -> Syntax TypedAST a -> TypedAST a
buildTypedLet metadata syntax =
  let unit = HUnit
      typeInfo = HInjL (HInjL (HInjL (HInjL undefined)))  -- HVoid in the coproduct chain
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- | Build a typed definition from raw components (uses HVoid for no type info)
buildTypedDefinition :: forall a. Metadata TypedAST a -> Syntax TypedAST a -> TypedAST a
buildTypedDefinition metadata syntax =
  let unit = HUnit
      typeInfo = HInjL (HInjL (HInjL (HInjL undefined)))  -- HVoid in the coproduct chain
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- | Build a typed program from raw components (uses HVoid for no type info)
buildTypedProgram :: forall a. Metadata TypedAST a -> Syntax TypedAST a -> TypedAST a
buildTypedProgram metadata syntax =
  let unit = HUnit
      typeInfo = HInjL (HInjL (HInjL (HInjL undefined)))  -- HVoid in the coproduct chain
  in HFix (unit :*: metadata :*: syntax :*: typeInfo)

-- ============================================================================
-- Convenience Functions for Creating Metadata and Syntax
-- ============================================================================

-- | Create metadata from source positions
createMetadata :: SourcePos -> SourcePos -> Metadata TypedAST a
createMetadata startPos endPos = Metadata startPos endPos

-- ============================================================================
-- Type Extraction Functions
-- ============================================================================

-- | Extract inferred type from a typed expression
extractInferredType :: TypedAST KExpr -> TSTypes.Type
extractInferredType typedExpr =
  let typeInfo = extractTypeInfo typedExpr
      exprInfo = unExprTypeInfo typeInfo
  in etiInferredType exprInfo

-- | Extract variable type from a typed variable
extractVariableType :: TypedAST KVariable -> TSTypes.Type
extractVariableType typedVar =
  let typeInfo = extractTypeInfo typedVar
      varInfo = unVariableTypeInfo typeInfo
  in vtiInferredType varInfo

-- | Extract type scheme from a typed variable (if polymorphic)
extractTypeScheme :: TypedAST KVariable -> Maybe TSTypes.TypeScheme
extractTypeScheme typedVar =
  let typeInfo = extractTypeInfo typedVar
      varInfo = unVariableTypeInfo typeInfo
  in vtiTypeScheme varInfo

-- | Get expression type information
getExpressionType :: TypedAST KExpr -> (TSTypes.Type, Map.Map T.Text TSTypes.Type)
getExpressionType typedExpr =
  let typeInfo = extractTypeInfo typedExpr
      exprInfo = unExprTypeInfo typeInfo
  in (etiInferredType exprInfo, etiLocalBindings exprInfo)

-- | Get variable type information
getVariableInfo :: TypedAST KVariable -> (TSTypes.Type, Maybe TSTypes.TypeScheme)
getVariableInfo typedVar =
  let typeInfo = extractTypeInfo typedVar
      varInfo = unVariableTypeInfo typeInfo
  in (vtiInferredType varInfo, vtiTypeScheme varInfo)