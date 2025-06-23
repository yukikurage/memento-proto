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

-- Compile switch expression (pattern matching)
compileSwitchExpression :: CompilationContext -> ControlContext -> IR.IRExpr -> [IR.IRCase] -> ([WASMInstruction], ControlContext)
compileSwitchExpression ctx ctrlCtx expr cases =
  let exprInstr = compileExpr ctx expr
      (switchInstr, finalCtrlCtx) = compileSwitchCases ctx ctrlCtx "match_value" cases
  in (exprInstr ++ [LocalTee "match_value"] ++ switchInstr, finalCtrlCtx)

-- Compile switch cases with jump table optimization
compileSwitchCases :: CompilationContext -> ControlContext -> Text -> [IR.IRCase] -> ([WASMInstruction], ControlContext)
compileSwitchCases ctx ctrlCtx objVar cases =
  case detectJumpTablePattern cases of
    Just jumpTable -> compileJumpTable ctx ctrlCtx objVar jumpTable
    Nothing -> compileLinearSwitch ctx ctrlCtx objVar cases

-- Detect if cases can use jump table (all constructor patterns with consecutive tags)
detectJumpTablePattern :: [IR.IRCase] -> Maybe [(Int, IR.IRExpr)]
detectJumpTablePattern cases = 
  let extractConstructorCase (IR.IRCase (IR.IRPatConstructor _ _) body) = Just body
      extractConstructorCase _ = Nothing
      constructorBodies = map extractConstructorCase cases
  in if all (not . null) constructorBodies
     then Just $ zip [0..] (map (\(Just b) -> b) constructorBodies)
     else Nothing

-- Compile using jump table for efficient pattern matching
compileJumpTable :: CompilationContext -> ControlContext -> Text -> [(Int, IR.IRExpr)] -> ([WASMInstruction], ControlContext)
compileJumpTable ctx ctrlCtx objVar jumpTable =
  let (tableLabel, ctrlCtx1) = generateLabel "jump_table" ctrlCtx
      caseLabels = map (\(i, _) -> "case_" <> T.pack (show i)) jumpTable
      (defaultLabel, ctrlCtx2) = generateLabel "default" ctrlCtx1
      
      -- Load object tag for jump table
      loadTagInstr = 
        [ LocalGet objVar
        , I32Load 4 0  -- Load tag from object header
        ]
      
      -- Generate jump table
      jumpInstr = 
        [ BrTable caseLabels defaultLabel ]
      
      -- Generate case blocks
      (caseBlocks, ctrlCtx3) = compileCaseBlocks ctx ctrlCtx2 jumpTable
      
      instructions = 
        [ Block tableLabel F64 
          (loadTagInstr ++ jumpInstr ++ caseBlocks ++
           [ Block defaultLabel F64 [Unreachable] ])
        ]
  in (instructions, ctrlCtx3)

-- Compile case blocks for jump table
compileCaseBlocks :: CompilationContext -> ControlContext -> [(Int, IR.IRExpr)] -> ([WASMInstruction], ControlContext)
compileCaseBlocks ctx ctrlCtx [] = ([], ctrlCtx)
compileCaseBlocks ctx ctrlCtx ((tag, body):rest) =
  let (caseLabel, ctrlCtx1) = generateLabel ("case_" <> T.pack (show tag)) ctrlCtx
      bodyInstr = compileExpr ctx body
      (restInstr, ctrlCtx2) = compileCaseBlocks ctx ctrlCtx1 rest
      
      caseBlock = 
        [ Block caseLabel F64 (bodyInstr ++ [Return]) ]
  in (caseBlock ++ restInstr, ctrlCtx2)

-- Compile linear switch (fallback for complex patterns)
compileLinearSwitch :: CompilationContext -> ControlContext -> Text -> [IR.IRCase] -> ([WASMInstruction], ControlContext)
compileLinearSwitch ctx ctrlCtx objVar cases =
  compileLinearSwitchCases ctx ctrlCtx objVar cases

compileLinearSwitchCases :: CompilationContext -> ControlContext -> Text -> [IR.IRCase] -> ([WASMInstruction], ControlContext)
compileLinearSwitchCases ctx ctrlCtx objVar [] = 
  -- No cases left - should be unreachable in exhaustive match
  ([Unreachable], ctrlCtx)
compileLinearSwitchCases ctx ctrlCtx objVar [IR.IRCase pattern body] =
  -- Last case - no need for condition (should always match in exhaustive switch)
  let bindingInstr = compilePatternBinding ctx objVar pattern
      bodyInstr = compileExpr ctx body
  in (bindingInstr ++ bodyInstr, ctrlCtx)
compileLinearSwitchCases ctx ctrlCtx objVar (IR.IRCase pattern body : rest) =
  let (conditionInstr, bindingInstr) = compilePatternCondition ctx objVar pattern
      bodyInstr = compileExpr ctx body
      (restInstr, ctrlCtx1) = compileLinearSwitchCases ctx ctrlCtx objVar rest
      (skipLabel, ctrlCtx2) = generateLabel "skip_case" ctrlCtx1
      
      caseInstr = 
        [ Block skipLabel F64 
          (conditionInstr ++  -- Check if pattern matches
           [BrIf skipLabel] ++  -- Skip if doesn't match
           bindingInstr ++     -- Bind pattern variables
           bodyInstr ++        -- Execute case body
           [Return]            -- Exit switch
          )
        ] ++ restInstr
  in (caseInstr, ctrlCtx2)

-- Compile loops (for recursive functions and iteration)
compileLoop :: CompilationContext -> ControlContext -> Text -> [WASMInstruction] -> ([WASMInstruction], ControlContext)
compileLoop ctx ctrlCtx loopName bodyInstr =
  let (loopLabel, ctrlCtx1) = generateLabel loopName ctrlCtx
      (breakLabel, ctrlCtx2) = generateLabel "break" ctrlCtx1
      
      -- Add loop to context for break/continue
      newBreakTargets = Map.insert loopName breakLabel (ccBreakTargets ctrlCtx2)
      newContinueTargets = Map.insert loopName loopLabel (ccContinueTargets ctrlCtx2)
      updatedCtx = ctrlCtx2 
        { ccBreakTargets = newBreakTargets
        , ccContinueTargets = newContinueTargets
        }
      
      loopInstr = 
        [ Block breakLabel F64
          [ Loop loopLabel F64 bodyInstr ]
        ]
  in (loopInstr, updatedCtx)

-- Compile break statement
compileBreak :: ControlContext -> Text -> [WASMInstruction]
compileBreak ctrlCtx loopName =
  case Map.lookup loopName (ccBreakTargets ctrlCtx) of
    Just target -> [Br target]
    Nothing -> [Unreachable]  -- Error: break outside loop

-- Compile continue statement
compileContinue :: ControlContext -> Text -> [WASMInstruction]
compileContinue ctrlCtx loopName =
  case Map.lookup loopName (ccContinueTargets ctrlCtx) of
    Just target -> [Br target]
    Nothing -> [Unreachable]  -- Error: continue outside loop

-- Tail recursion optimization
optimizeTailRecursion :: Text -> [WASMInstruction] -> [WASMInstruction]
optimizeTailRecursion funcName instructions = 
  -- Look for tail recursive calls and convert to loops
  case reverse instructions of
    (Return : Call name : setupArgs) | name == funcName ->
      -- Convert tail call to loop
      let (loopBody, paramUpdates) = extractTailCallSetup (reverse setupArgs)
      in [ Loop "tail_recursion" F64 
           (loopBody ++ paramUpdates ++ [Br "tail_recursion"])
         ]
    _ -> instructions

-- Extract parameter setup for tail call
extractTailCallSetup :: [WASMInstruction] -> ([WASMInstruction], [WASMInstruction])
extractTailCallSetup instructions = 
  -- Simplified: assume all instructions before call are parameter setup
  let paramUpdates = map convertToParamUpdate instructions
  in ([], paramUpdates)

-- Convert instruction to parameter update
convertToParamUpdate :: WASMInstruction -> WASMInstruction
convertToParamUpdate (LocalGet name) = LocalSet name
convertToParamUpdate instr = instr  -- Fallback

-- Exception handling support (for error types)
compileThrow :: CompilationContext -> IR.IRExpr -> [WASMInstruction]
compileThrow ctx errorValue =
  -- In WASM, we can simulate exceptions with unwinding
  compileExpr ctx errorValue ++
  [Unreachable]  -- Simplified: just trap

compileTryCatch :: CompilationContext -> ControlContext -> IR.IRExpr -> [(IR.IRPattern, IR.IRExpr)] -> ([WASMInstruction], ControlContext)
compileTryCatch ctx ctrlCtx tryExpr catchCases =
  -- Simplified try-catch using result types
  let (tryLabel, ctrlCtx1) = generateLabel "try" ctrlCtx
      (catchLabel, ctrlCtx2) = generateLabel "catch" ctrlCtx1
      
      tryInstr = compileExpr ctx tryExpr
      (catchInstr, ctrlCtx3) = compileCatchCases ctx ctrlCtx2 catchCases
      
      instructions = 
        [ Block tryLabel F64
          (tryInstr ++
           [Br tryLabel] ++  -- Success - skip catch
           catchInstr
          )
        ]
  in (instructions, ctrlCtx3)

-- Compile catch cases
compileCatchCases :: CompilationContext -> ControlContext -> [(IR.IRPattern, IR.IRExpr)] -> ([WASMInstruction], ControlContext)
compileCatchCases ctx ctrlCtx [] = ([Unreachable], ctrlCtx)  -- No catch handlers
compileCatchCases ctx ctrlCtx ((pattern, handler):rest) =
  let handlerInstr = compileExpr ctx handler
      (restInstr, ctrlCtx1) = compileCatchCases ctx ctrlCtx rest
      
      -- Pattern matching on error value
      (condInstr, bindInstr) = compilePatternCondition ctx "error_value" pattern
      (skipLabel, ctrlCtx2) = generateLabel "skip_catch" ctrlCtx1
      
      catchInstr = 
        [ Block skipLabel F64
          (condInstr ++
           [BrIf skipLabel] ++
           bindInstr ++
           handlerInstr ++
           [Return]
          )
        ] ++ restInstr
  in (catchInstr, ctrlCtx2)