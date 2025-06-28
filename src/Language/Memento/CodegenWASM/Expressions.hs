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
  -- Note: Constructor applications and pattern matching not supported in primitive IR
  -- These should not appear in primitive IR expressions

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
  -- Implement lambda lifting by creating a top-level function
  let capturedVars = findFreeVariables ctx body
      funcIndex = getNextFunctionIndex ctx
      liftedFuncName = "lambda_" <> T.pack (show funcIndex)
      
      -- Create closure that captures free variables
      closureData = if null capturedVars
        then [I32Const (fromIntegral funcIndex)]  -- Simple function pointer for closures without captures
        else generateClosureWithCaptures funcIndex capturedVars
  in 
    -- Store the generated function in context for later emission
    -- For now, generate closure allocation
    closureData ++ generateClosureAllocation funcIndex capturedVars

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
  -- Note: Constructor applications and pattern matching not supported in primitive IR

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

-- Note: Constructor applications and pattern matching functions removed for primitive IR
-- Primitive IR only supports basic types and functions, no user-defined data types or pattern matching

-- Get next available function index for lambda lifting
getNextFunctionIndex :: CompilationContext -> Int
getNextFunctionIndex ctx = 
  -- Simple counter based on number of existing functions
  length (ctxFunctions ctx) + 1000  -- Start lambda functions at index 1000

-- Generate closure with captured variables
generateClosureWithCaptures :: Int -> [Text] -> [WASMInstruction]
generateClosureWithCaptures funcIndex capturedVars =
  -- Allocate memory for closure: [function_index, captured_var1, captured_var2, ...]
  let closureSize = 1 + length capturedVars  -- Function index + captured variables
      allocSize = closureSize * 4  -- Each value is 4 bytes (i32 or f64)
  in
    [ I32Const (fromIntegral allocSize)
    , Call "malloc"  -- Allocate memory for closure
    -- Store function index at offset 0
    , LocalTee "closure_ptr"  -- Keep pointer on stack and in local
    , I32Const (fromIntegral funcIndex)
    , I32Store 0 0  -- Store function index at offset 0
    ] ++
    -- Store each captured variable
    concatMap (\(idx, varName) ->
      [ LocalGet "closure_ptr"
      , LocalGet varName  -- Get captured variable value
      , I32Store 0 (fromIntegral ((idx + 1) * 4))  -- Store at appropriate offset
      ]) (zip [0..] capturedVars) ++
    [ LocalGet "closure_ptr"  -- Return closure pointer
    ]