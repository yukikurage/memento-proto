{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM.Control where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Memento.CodegenWASM.Types
import Language.Memento.CodegenWASM.Expressions
import qualified Language.Memento.IR as IR

-- Control flow compilation context
data ControlContext = ControlContext
  { ccLabelCounter :: Int
  , ccBreakTargets :: Map Text Text  -- Break label mapping
  , ccContinueTargets :: Map Text Text  -- Continue label mapping
  } deriving (Show)

emptyControlContext :: ControlContext
emptyControlContext = ControlContext 0 Map.empty Map.empty

-- Generate unique label
generateLabel :: Text -> ControlContext -> (Text, ControlContext)
generateLabel prefix ctx =
  let label = prefix <> "_" <> T.pack (show (ccLabelCounter ctx))
      newCtx = ctx { ccLabelCounter = ccLabelCounter ctx + 1 }
  in (label, newCtx)

-- Compile if expression with proper control flow
compileIfExpression :: CompilationContext -> ControlContext -> IR.IRExpr -> IR.IRExpr -> IR.IRExpr -> ([WASMInstruction], ControlContext)
compileIfExpression ctx ctrlCtx cond thenExpr elseExpr =
  let condInstr = compileExpr ctx cond
      thenInstr = compileExpr ctx thenExpr
      elseInstr = compileExpr ctx elseExpr
      
      -- Use block structure for if-then-else
      (thenLabel, ctrlCtx1) = generateLabel "then" ctrlCtx
      (elseLabel, ctrlCtx2) = generateLabel "else" ctrlCtx1
      (endLabel, ctrlCtx3) = generateLabel "end" ctrlCtx2
      
      instructions = 
        condInstr ++
        [ If F64 thenInstr elseInstr ]
  in (instructions, ctrlCtx3)

-- Note: All pattern matching and switch/case compilation functions removed for primitive IR
-- Primitive IR only supports basic control flow: if-then-else

-- Loop compilation (for potential future use)
compileLoop :: CompilationContext -> ControlContext -> [WASMInstruction] -> ([WASMInstruction], ControlContext)
compileLoop _ctx ctrlCtx body =
  let (loopLabel, ctrlCtx1) = generateLabel "loop" ctrlCtx
      instructions = [Loop loopLabel F64 body]
  in (instructions, ctrlCtx1)

-- Block compilation with labels
compileBlock :: CompilationContext -> ControlContext -> [WASMInstruction] -> ([WASMInstruction], ControlContext)
compileBlock _ctx ctrlCtx body =
  let (blockLabel, ctrlCtx1) = generateLabel "block" ctrlCtx
      instructions = [Block blockLabel F64 body]
  in (instructions, ctrlCtx1)