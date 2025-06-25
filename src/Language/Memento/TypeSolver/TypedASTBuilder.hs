{-# LANGUAGE OverloadedStrings #-}

-- | Build typed AST from constraint solving results
-- This module provides a practical interface for attaching type information to AST nodes
module Language.Memento.TypeSolver.TypedASTBuilder 
  ( TypedASTInfo (..),
    buildTypedASTInfo,
    extractVariableType,
    extractExpressionType,
    formatTypedASTInfo,
  ) 
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.Syntax.Tag (KExpr, KVariable)
import qualified Language.Memento.TypeSolver.Types as TSTypes

-- ============================================================================
-- Simplified TypedAST Information
-- ============================================================================

-- | Simplified typed AST information that can be easily used by downstream consumers
data TypedASTInfo = TypedASTInfo
  { taiVariableTypes :: Map.Map T.Text TSTypes.TypeScheme,  -- ^ Variable name to type scheme
    taiExpressionTypes :: Map.Map Int TSTypes.Type,         -- ^ Expression ID to inferred type (placeholder)
    taiLiteralTypes :: Map.Map Int TSTypes.Type,            -- ^ Literal ID to inferred type (placeholder)
    taiTypeResolutions :: Map.Map Int TSTypes.Type          -- ^ Type node ID to resolved type (placeholder)
  }
  deriving (Show, Eq)

-- | Build typed AST info from type checking results
buildTypedASTInfo :: Map.Map T.Text TSTypes.TypeScheme -> TypedASTInfo
buildTypedASTInfo typeEnv = TypedASTInfo
  { taiVariableTypes = typeEnv
  , taiExpressionTypes = Map.empty  -- Would be populated during constraint generation
  , taiLiteralTypes = Map.empty     -- Would be populated during constraint generation
  , taiTypeResolutions = Map.empty  -- Would be populated during type resolution
  }

-- ============================================================================
-- Helper Functions for Type Extraction
-- ============================================================================

-- | Extract variable type from typed AST info
extractVariableType :: T.Text -> TypedASTInfo -> Maybe TSTypes.TypeScheme
extractVariableType varName typedInfo = Map.lookup varName (taiVariableTypes typedInfo)

-- | Extract expression type from typed AST info (placeholder)
extractExpressionType :: Int -> TypedASTInfo -> Maybe TSTypes.Type
extractExpressionType exprId typedInfo = Map.lookup exprId (taiExpressionTypes typedInfo)

-- | Format typed AST info for display
formatTypedASTInfo :: TypedASTInfo -> T.Text
formatTypedASTInfo typedInfo = T.unlines
  [ "=== Typed AST Information ==="
  , "Variable Types:"
  , TSTypes.formatTypeEnv (taiVariableTypes typedInfo)
  , "Expression Types: " <> T.pack (show $ Map.size $ taiExpressionTypes typedInfo) <> " entries"
  , "Literal Types: " <> T.pack (show $ Map.size $ taiLiteralTypes typedInfo) <> " entries"
  , "Type Resolutions: " <> T.pack (show $ Map.size $ taiTypeResolutions typedInfo) <> " entries"
  ]