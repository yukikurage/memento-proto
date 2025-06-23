{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM.Expressions where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Memento.CodegenWASM.Types
import Language.Memento.CodegenWASM.Memory
import qualified Language.Memento.IR as IR

-- Compilation context
data CompilationContext = CompilationContext
  { ctxLocals :: Set Text          -- Local variables and parameters
  , ctxGlobals :: Set Text         -- Global functions and values
  , ctxFunctions :: Set Text       -- Available function names
  , ctxClosureVars :: [Text]       -- Variables captured in closures
  , ctxBlockDepth :: Int           -- Nesting depth for block labels
  } deriving (Show)

emptyContext :: CompilationContext
emptyContext = CompilationContext Set.empty Set.empty Set.empty [] 0

-- Add local variable to context
addLocal :: Text -> CompilationContext -> CompilationContext
addLocal name ctx = ctx { ctxLocals = Set.insert name (ctxLocals ctx) }

-- Add multiple locals
addLocals :: [Text] -> CompilationContext -> CompilationContext
addLocals names ctx = foldr addLocal ctx names

-- Check if variable is local
isLocal :: Text -> CompilationContext -> Bool
isLocal name ctx = Set.member name (ctxLocals ctx)

-- Generate unique block label
generateBlockLabel :: CompilationContext -> (Text, CompilationContext)
generateBlockLabel ctx = 
  let label = "block" <> T.pack (show (ctxBlockDepth ctx))
      newCtx = ctx { ctxBlockDepth = ctxBlockDepth ctx + 1 }
  in (label, newCtx)

-- Compile IR expression to WASM instructions
compileExpr :: CompilationContext -> IR.IRExpr -> [WASMInstruction]
compileExpr ctx expr = case expr of
  IR.IRVar name -> compileVariable ctx name
  IR.IRLiteral lit -> compileLiteral lit
  IR.IRBinOp op left right -> compileBinOp ctx op left right
  IR.IRCall func args -> compileCall ctx func args
  IR.IRLambda params _returnType body -> compileLambda ctx params body
  IR.IRIf cond thenExpr elseExpr -> compileIf ctx cond thenExpr elseExpr
  IR.IRLet varName _varType varExpr bodyExpr -> compileLet ctx varName varExpr bodyExpr
  IR.IRBlock exprs -> compileBlock ctx exprs
  IR.IRCtorApp ctorName args -> compileConstructor ctx ctorName args
  IR.IRMatch expr cases -> compileMatch ctx expr cases

-- Compile variable reference
compileVariable :: CompilationContext -> Text -> [WASMInstruction]
compileVariable ctx name
  | isLocal name ctx = [LocalGet name]
  | Set.member name (ctxGlobals ctx) = [Call name]  -- Zero-argument function call
  | Set.member name (ctxFunctions ctx) = [Call name]  -- Zero-argument function call  
  | otherwise = [Call name]  -- Assume it's a global function

-- Compile literals
compileLiteral :: IR.IRLiteral -> [WASMInstruction]
compileLiteral (IR.IRNumberLit n) = [I32Const (round n)]  -- Convert f64 to i32 for MkBox
compileLiteral (IR.IRIntLit i) = [I32Const i]
compileLiteral (IR.IRBoolLit True) = [I32Const 1]
compileLiteral (IR.IRBoolLit False) = [I32Const 0]
compileLiteral (IR.IRStringLit str) = generateStringAllocation str

-- Compile binary operations
compileBinOp :: CompilationContext -> IR.IRBinOp -> IR.IRExpr -> IR.IRExpr -> [WASMInstruction]
compileBinOp ctx op left right =
  compileExpr ctx left ++
  compileExpr ctx right ++
  [compileBinOpInstr op]

compileBinOpInstr :: IR.IRBinOp -> WASMInstruction
compileBinOpInstr IR.IRAdd = F64Add
compileBinOpInstr IR.IRSub = F64Sub
compileBinOpInstr IR.IRMul = F64Mul
compileBinOpInstr IR.IRDiv = F64Div
compileBinOpInstr IR.IREq = F64Eq
compileBinOpInstr IR.IRLt = F64Lt
compileBinOpInstr IR.IRGt = F64Gt

-- Compile function calls
compileCall :: CompilationContext -> IR.IRExpr -> [IR.IRExpr] -> [WASMInstruction]
compileCall ctx func args = case func of
  IR.IRVar funcName -> 
    -- Direct function call
    concatMap (compileExpr ctx) args ++
    [Call funcName]
  _ -> 
    -- Higher-order function call (closure)
    compileExpr ctx func ++
    concatMap (compileExpr ctx) args ++
    generateClosureCall (map (const "arg") args)

-- Compile lambda expressions (create closures)
compileLambda :: CompilationContext -> [(Text, IR.IRType)] -> IR.IRExpr -> [WASMInstruction]
compileLambda ctx params body =
  -- For now, generate a simple closure
  -- TODO: Implement proper lambda lifting and closure creation
  let capturedVars = findFreeVariables ctx body
      funcIndex = 0  -- TODO: Generate unique function index
  in generateClosureAllocation funcIndex capturedVars

-- Find free variables in expression (for closure capture)
findFreeVariables :: CompilationContext -> IR.IRExpr -> [Text]
findFreeVariables ctx expr = case expr of
  IR.IRVar name | not (isLocal name ctx) && not (Set.member name (ctxGlobals ctx)) -> [name]
  IR.IRVar _ -> []
  IR.IRLiteral _ -> []
  IR.IRBinOp _ left right -> findFreeVariables ctx left ++ findFreeVariables ctx right
  IR.IRCall func args -> findFreeVariables ctx func ++ concatMap (findFreeVariables ctx) args
  IR.IRLambda params _ body -> 
    let newCtx = addLocals (map fst params) ctx
    in findFreeVariables newCtx body
  IR.IRIf cond thenExpr elseExpr -> 
    findFreeVariables ctx cond ++ 
    findFreeVariables ctx thenExpr ++ 
    findFreeVariables ctx elseExpr
  IR.IRLet varName _ varExpr bodyExpr ->
    findFreeVariables ctx varExpr ++
    findFreeVariables (addLocal varName ctx) bodyExpr
  IR.IRBlock exprs -> concatMap (findFreeVariables ctx) exprs
  IR.IRCtorApp _ args -> concatMap (findFreeVariables ctx) args
  IR.IRMatch expr cases -> 
    findFreeVariables ctx expr ++
    concatMap (\(IR.IRCase _ body) -> findFreeVariables ctx body) cases

-- Compile if expressions
compileIf :: CompilationContext -> IR.IRExpr -> IR.IRExpr -> IR.IRExpr -> [WASMInstruction]
compileIf ctx cond thenExpr elseExpr =
  compileExpr ctx cond ++
  [If F64 
    (compileExpr ctx thenExpr)
    (compileExpr ctx elseExpr)
  ]

-- Compile let bindings with proper scoping
compileLet :: CompilationContext -> Text -> IR.IRExpr -> IR.IRExpr -> [WASMInstruction]
compileLet ctx varName varExpr bodyExpr =
  -- Evaluate the variable expression
  compileExpr ctx varExpr ++
  -- Store in local variable
  [LocalTee varName] ++
  -- Compile body with variable in scope
  compileExpr (addLocal varName ctx) bodyExpr

-- Compile block expressions
compileBlock :: CompilationContext -> [IR.IRExpr] -> [WASMInstruction]
compileBlock ctx exprs = case exprs of
  [] -> [F64Const 0]  -- Empty block returns 0
  [expr] -> compileExpr ctx expr
  (expr:rest) -> 
    compileExpr ctx expr ++
    [Drop] ++  -- Discard intermediate results
    compileBlock ctx rest

-- Compile constructor applications
compileConstructor :: CompilationContext -> Text -> [IR.IRExpr] -> [WASMInstruction]
compileConstructor ctx ctorName args =
  -- Evaluate arguments
  concatMap (compileExpr ctx) args ++
  -- Call constructor function
  [Call ctorName]

-- Compile pattern matching
compileMatch :: CompilationContext -> IR.IRExpr -> [IR.IRCase] -> [WASMInstruction]
compileMatch ctx expr cases =
  -- Evaluate the expression to match (should be i32 pointer for objects)
  compileExpr ctx expr ++
  [LocalTee "obj_ptr"] ++  -- Store object pointer in obj_ptr and use for matching
  -- Generate branching logic for each case
  compileMatchCases ctx "obj_ptr" cases

compileMatchCases :: CompilationContext -> Text -> [IR.IRCase] -> [WASMInstruction]
compileMatchCases ctx objVar cases = case cases of
  [] -> [Unreachable]  -- Should never reach here if exhaustive
  [IR.IRCase pattern body] -> 
    -- Last case - no need for condition
    let patternVars = extractPatternVars pattern
        extendedCtx = addLocals patternVars ctx
    in compilePatternBinding extendedCtx objVar pattern ++
       compileExpr extendedCtx body
  (IR.IRCase pattern body : rest) ->
    let (conditionCode, bindingCode) = compilePatternCondition ctx objVar pattern
        patternVars = extractPatternVars pattern
        extendedCtx = addLocals patternVars ctx
        (blockLabel, newCtx) = generateBlockLabel extendedCtx
    in [Block blockLabel F64 
         (conditionCode ++
          [BrIf blockLabel] ++  -- Skip this case if pattern doesn't match
          bindingCode ++
          compileExpr newCtx body ++
          [Return]
         )
       ] ++
       compileMatchCases newCtx objVar rest

-- Compile pattern matching condition and binding
compilePatternCondition :: CompilationContext -> Text -> IR.IRPattern -> ([WASMInstruction], [WASMInstruction])
compilePatternCondition ctx objVar pattern = case pattern of
  IR.IRPatVar varName _type ->
    -- Variable pattern always matches
    ([], [LocalGet objVar, LocalSet varName])
  IR.IRPatWildcard _type ->
    -- Wildcard always matches
    ([], [])
  IR.IRPatLiteral lit ->
    -- Literal pattern - compare values (both as i32)
    let litInstructions = compileLiteral lit
        conditionCode = [LocalGet objVar] ++ 
                        litInstructions ++ 
                        [I32Ne]
    in (conditionCode, [])
  IR.IRPatConstructor ctorName argPatterns ->
    -- Constructor pattern - check tag and extract fields
    let tag = 0  -- TODO: Get actual constructor tag
        conditionCode = generateTagCheck objVar tag ++ [I32Eqz]  -- Check if NOT equal
        bindingCode = concatMap (\(i, argPat) ->
          case argPat of
            IR.IRPatVar varName _type -> generateFieldExtraction objVar i varName
            _ -> []  -- TODO: Handle nested patterns
          ) (zip [0..] argPatterns)
    in (conditionCode, bindingCode)

-- Extract pattern variables from a pattern
extractPatternVars :: IR.IRPattern -> [Text]
extractPatternVars pattern = case pattern of
  IR.IRPatVar varName _ -> [varName]
  IR.IRPatWildcard _ -> []
  IR.IRPatLiteral _ -> []
  IR.IRPatConstructor _ argPatterns -> concatMap extractPatternVars argPatterns

-- Helper for pattern binding without condition
compilePatternBinding :: CompilationContext -> Text -> IR.IRPattern -> [WASMInstruction]
compilePatternBinding ctx objVar pattern = case pattern of
  IR.IRPatVar varName _type -> [LocalGet objVar, F64ConvertI32S, LocalSet varName]
  IR.IRPatWildcard _type -> []
  IR.IRPatLiteral _ -> []  -- No binding needed for literals
  IR.IRPatConstructor _ argPatterns ->
    concatMap (\(i, argPat) ->
      case argPat of
        IR.IRPatVar varName _type -> generateFieldExtraction objVar i varName
        _ -> []  -- TODO: Handle nested patterns
      ) (zip [0..] argPatterns)