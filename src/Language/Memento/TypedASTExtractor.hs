{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

-- | Extract type information from TypedAST for enhanced code generation
module Language.Memento.TypedASTExtractor 
  ( TypedASTInfo(..)
  , extractTypedASTInfo
  , enhancedLowerToIR
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Language.Memento.IR as IR
import qualified Language.Memento.TypeSolver.Types as TST
import Language.Memento.TypeSolver.TypeInfo
import Language.Memento.TypeSolver.Types (TypeScheme(..))
import Language.Memento.LowerToIR
import Language.Memento.Syntax
import Language.Memento.Syntax.Tag

-- Type information extracted from TypedAST for enhanced code generation
data TypedASTInfo = TypedASTInfo
  { taiVariableTypes :: Map.Map Text TST.Type
  , taiExpressionTypes :: Map.Map Text TST.Type  -- Keyed by expression identifier or context
  , taiTypeSchemes :: Map.Map Text TypeScheme
  , taiLocalBindings :: Map.Map Text (Map.Map Text TST.Type)
  } deriving (Show, Eq)

-- Extract comprehensive type information from TypedAST
extractTypedASTInfo :: TypedAST KProgram -> TypedASTInfo
extractTypedASTInfo typedProgram = 
  TypedASTInfo
    { taiVariableTypes = Map.empty  -- Would be populated by traversing TypedAST
    , taiExpressionTypes = Map.empty
    , taiTypeSchemes = Map.empty
    , taiLocalBindings = Map.empty
    }

-- Enhanced IR generation that uses both AST and TypedAST information
enhancedLowerToIR :: AST KProgram -> TypedASTInfo -> IR.IRProgram
enhancedLowerToIR astProgram typedInfo = 
  -- Use existing LowerToIR with type environment from TypedAST
  let typeEnv = buildTypeEnvFromTypedInfo typedInfo
  in lowerProgramWithTypes astProgram typeEnv

-- Build type environment from extracted TypedAST information
buildTypeEnvFromTypedInfo :: TypedASTInfo -> Map.Map Text TypeScheme
buildTypeEnvFromTypedInfo typedInfo = 
  let -- Convert variable types to type schemes
      varSchemes = Map.map (\t -> TypeScheme [] t) (taiVariableTypes typedInfo)
      -- Merge with existing type schemes
      allSchemes = Map.union (taiTypeSchemes typedInfo) varSchemes
  in allSchemes