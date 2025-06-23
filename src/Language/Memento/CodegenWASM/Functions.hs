{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM.Functions where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Memento.CodegenWASM.Types
import Language.Memento.CodegenWASM.Memory
import Language.Memento.CodegenWASM.Expressions
import qualified Language.Memento.IR as IR

-- Function compilation context
data FunctionContext = FunctionContext
  { fcLiftedFunctions :: Map Text WASMFunction  -- Lambda-lifted functions
  , fcFunctionIndex :: Int                      -- Next function index
  , fcTypeIndex :: Map [WASMType] Int          -- Type signature to index mapping
  , fcNextTypeIndex :: Int                     -- Next type index
  } deriving (Show)

emptyFunctionContext :: FunctionContext
emptyFunctionContext = FunctionContext Map.empty 0 Map.empty 0

-- Result of function compilation
data FunctionCompilationResult = FunctionCompilationResult
  { fcrMainFunction :: Maybe WASMFunction
  , fcrLiftedFunctions :: [WASMFunction]
  , fcrTypes :: [WASMFuncType]
  } deriving (Show)

-- Compile IR function to WASM function
compileFunctionDef :: IR.IRFunction -> WASMFunction
compileFunctionDef (IR.IRFunction name params returnType body) =
  let wasmParams = map (\(pName, pType) -> (pName, irTypeToWASM pType)) params
      paramNames = map fst params
      compilationCtx = emptyContext 
        { ctxLocals = Set.fromList paramNames
        , ctxGlobals = Set.empty  -- Will be filled by caller
        }
      bodyInstructions = compileExpr compilationCtx body
      -- Extract all local variables needed by analyzing the expression
      neededLocals = extractPatternVariables body
      -- Add common local variables that might be needed
      commonLocals = [("match_value", F64), ("obj_ptr", I32), ("string_ptr", I32)] ++ 
                     map (\name -> (name, F64)) neededLocals  -- Default to F64 for pattern variables
  in WASMFunction
     { funcName = name
     , funcTypeIndex = 0  -- Will be assigned later
     , funcParams = wasmParams      -- Only function parameters
     , funcLocals = commonLocals    -- Only local variables
     , funcReturnType = irTypeToWASM returnType  -- Convert IR return type
     , funcBody = bodyInstructions ++ [Return]
     }

-- Compile global value (convert to nullary function)
compileGlobalValue :: IR.IRGlobal -> WASMFunction
compileGlobalValue (IR.IRGlobal name _ty expr) =
  let compilationCtx = emptyContext { ctxGlobals = Set.empty }
      bodyInstructions = compileExpr compilationCtx expr
      -- Extract all local variables needed by analyzing the expression
      neededLocals = extractPatternVariables expr
      -- Add common local variables that might be needed
      commonLocals = [("match_value", F64), ("obj_ptr", I32), ("string_ptr", I32)] ++ 
                     map (\varName -> (varName, F64)) neededLocals  -- Default to F64 for pattern variables
  in WASMFunction
     { funcName = name
     , funcTypeIndex = 0  -- Will be assigned later
     , funcParams = []               -- No parameters for global values
     , funcLocals = commonLocals     -- Add common local variables
     , funcReturnType = irTypeToWASM _ty  -- Convert IR type to WASM type
     , funcBody = bodyInstructions ++ [Return]
     }

-- Lambda lifting: convert nested lambdas to top-level functions
liftLambdas :: IR.IRExpr -> FunctionContext -> (IR.IRExpr, FunctionContext)
liftLambdas expr ctx = case expr of
  IR.IRLambda params returnType body ->
    -- Generate unique function name
    let funcName = "lambda_" <> T.pack (show (fcFunctionIndex ctx))
        newCtx = ctx { fcFunctionIndex = fcFunctionIndex ctx + 1 }
        
        -- Lift nested lambdas in body
        (liftedBody, finalCtx) = liftLambdas body newCtx
        
        -- Create lifted function
        liftedFunc = WASMFunction
          { funcName = funcName
          , funcTypeIndex = 0  -- Will be assigned later
          , funcParams = map (\(pName, pType) -> (pName, irTypeToWASM pType)) params
          , funcLocals = []  -- No additional locals for now
          , funcReturnType = irTypeToWASM returnType  -- Convert return type
          , funcBody = compileExpr (emptyContext { ctxLocals = Set.fromList (map fst params) }) liftedBody ++ [Return]
          }
        
        -- Add to context
        updatedCtx = finalCtx { fcLiftedFunctions = Map.insert funcName liftedFunc (fcLiftedFunctions finalCtx) }
        
        -- Replace lambda with function reference
        functionRef = IR.IRVar funcName
    in (functionRef, updatedCtx)
    
  IR.IRBinOp op left right ->
    let (liftedLeft, ctx1) = liftLambdas left ctx
        (liftedRight, ctx2) = liftLambdas right ctx1
    in (IR.IRBinOp op liftedLeft liftedRight, ctx2)
    
  IR.IRCall func args ->
    let (liftedFunc, ctx1) = liftLambdas func ctx
        (liftedArgs, ctx2) = liftLambdasList args ctx1
    in (IR.IRCall liftedFunc liftedArgs, ctx2)
    
  IR.IRIf cond thenExpr elseExpr ->
    let (liftedCond, ctx1) = liftLambdas cond ctx
        (liftedThen, ctx2) = liftLambdas thenExpr ctx1
        (liftedElse, ctx3) = liftLambdas elseExpr ctx2
    in (IR.IRIf liftedCond liftedThen liftedElse, ctx3)
    
  IR.IRLet varName varType varExpr bodyExpr ->
    let (liftedVarExpr, ctx1) = liftLambdas varExpr ctx
        (liftedBodyExpr, ctx2) = liftLambdas bodyExpr ctx1
    in (IR.IRLet varName varType liftedVarExpr liftedBodyExpr, ctx2)
    
  IR.IRBlock exprs ->
    let (liftedExprs, finalCtx) = liftLambdasList exprs ctx
    in (IR.IRBlock liftedExprs, finalCtx)
    
  IR.IRCtorApp ctorName args ->
    let (liftedArgs, finalCtx) = liftLambdasList args ctx
    in (IR.IRCtorApp ctorName liftedArgs, finalCtx)
    
  IR.IRMatch expr cases ->
    let (liftedExpr, ctx1) = liftLambdas expr ctx
        (liftedCases, ctx2) = liftLambdasCases cases ctx1
    in (IR.IRMatch liftedExpr liftedCases, ctx2)
    
  -- Base cases
  IR.IRVar _ -> (expr, ctx)
  IR.IRLiteral _ -> (expr, ctx)

-- Helper for lifting lambdas in lists
liftLambdasList :: [IR.IRExpr] -> FunctionContext -> ([IR.IRExpr], FunctionContext)
liftLambdasList [] ctx = ([], ctx)
liftLambdasList (e:es) ctx =
  let (liftedE, ctx1) = liftLambdas e ctx
      (liftedEs, ctx2) = liftLambdasList es ctx1
  in (liftedE:liftedEs, ctx2)

-- Helper for lifting lambdas in pattern match cases
liftLambdasCases :: [IR.IRCase] -> FunctionContext -> ([IR.IRCase], FunctionContext)
liftLambdasCases [] ctx = ([], ctx)
liftLambdasCases (IR.IRCase pattern body : cases) ctx =
  let (liftedBody, ctx1) = liftLambdas body ctx
      (liftedCases, ctx2) = liftLambdasCases cases ctx1
  in (IR.IRCase pattern liftedBody : liftedCases, ctx2)

-- Generate function type signature
generateFunctionType :: [WASMType] -> [WASMType] -> WASMFuncType
generateFunctionType params results = WASMFuncType params results

-- Assign type indices to functions
assignTypeIndices :: [WASMFunction] -> ([WASMFunction], [WASMFuncType])
assignTypeIndices functions =
  let typeMap = buildTypeMap functions
      typedFunctions = map (assignTypeIndex typeMap) functions
      types = Map.elems $ Map.fromList $ map (\(sig, idx) -> (idx, sig)) $ Map.toList typeMap
  in (typedFunctions, types)

-- Build mapping from function signatures to type indices
buildTypeMap :: [WASMFunction] -> Map WASMFuncType Int
buildTypeMap functions =
  let signatures = map extractSignature functions
      uniqueSignatures = Set.toList $ Set.fromList signatures
  in Map.fromList $ zip uniqueSignatures [0..]

-- Extract function signature (only parameters, not all locals)
extractSignature :: WASMFunction -> WASMFuncType
extractSignature func =
  let paramTypes = map snd (funcParams func)  -- Only use actual parameters
      resultType = funcReturnType func        -- Use actual return type
  in WASMFuncType paramTypes [resultType]

-- Assign type index to function
assignTypeIndex :: Map WASMFuncType Int -> WASMFunction -> WASMFunction
assignTypeIndex typeMap func =
  let signature = extractSignature func
      typeIndex = Map.findWithDefault 0 signature typeMap
  in func { funcTypeIndex = typeIndex }

-- Extract pattern variables from expressions
extractPatternVariables :: IR.IRExpr -> [Text]
extractPatternVariables expr = case expr of
  IR.IRVar _ -> []
  IR.IRLiteral _ -> []
  IR.IRBinOp _ left right -> extractPatternVariables left ++ extractPatternVariables right
  IR.IRCall func args -> extractPatternVariables func ++ concatMap extractPatternVariables args
  IR.IRLambda _ _ body -> extractPatternVariables body
  IR.IRIf cond thenExpr elseExpr -> 
    extractPatternVariables cond ++ extractPatternVariables thenExpr ++ extractPatternVariables elseExpr
  IR.IRLet _ _ varExpr bodyExpr -> extractPatternVariables varExpr ++ extractPatternVariables bodyExpr
  IR.IRBlock exprs -> concatMap extractPatternVariables exprs
  IR.IRCtorApp _ args -> concatMap extractPatternVariables args
  IR.IRMatch expr cases -> 
    extractPatternVariables expr ++ concatMap extractFromCase cases
  where
    extractFromCase (IR.IRCase pattern body) = 
      extractFromPattern pattern ++ extractPatternVariables body
      
    extractFromPattern pattern = case pattern of
      IR.IRPatVar varName _ -> [varName]
      IR.IRPatWildcard _ -> []
      IR.IRPatLiteral _ -> []
      IR.IRPatConstructor _ argPatterns -> concatMap extractFromPattern argPatterns

-- Compile data constructor to WASM function (moved to Data.hs)
-- This is now handled by Data.compileDataConstructor

-- Higher-order function support
generateFunctionTable :: [WASMFunction] -> WASMTable
generateFunctionTable functions =
  WASMTable
  { tableMin = length functions
  , tableMax = Just (length functions * 2)  -- Allow some growth
  , tableElementType = "funcref"
  }

-- Generate function references for higher-order usage
generateFunctionRefs :: [WASMFunction] -> [WASMInstruction]
generateFunctionRefs functions =
  -- Initialize function table with function references
  concatMap (\(i, _func) -> 
    [ I32Const i
    , I32Const i  -- Function index same as table index for simplicity
    -- TODO: elem.drop or table.set instruction would go here
    ]) (zip [0..] functions)

-- Closure compilation
compileClosure :: IR.IRExpr -> [Text] -> FunctionContext -> (WASMFunction, FunctionContext)
compileClosure body capturedVars ctx =
  let funcName = "closure_" <> T.pack (show (fcFunctionIndex ctx))
      newCtx = ctx { fcFunctionIndex = fcFunctionIndex ctx + 1 }
      
      -- First parameter is always the closure object
      closureParam = ("closure", I32)
      -- Extract captured variables from closure
      extractInstructions = concatMap (\(i, varName) ->
        [ LocalGet "closure"
        , I32Const (closureHeaderSize + i * 4)
        , I32Add
        , I32Load 4 0
        , LocalSet varName
        ]) (zip [0..] capturedVars)
      
      -- Compile body with captured variables in scope
      compilationCtx = emptyContext { ctxLocals = Set.fromList ("closure" : capturedVars) }
      bodyInstructions = compileExpr compilationCtx body
      
      closureFunc = WASMFunction
        { funcName = funcName
        , funcTypeIndex = 0  -- Will be assigned later
        , funcParams = [closureParam]   -- Only closure parameter
        , funcLocals = map (\v -> (v, I32)) capturedVars  -- Captured variables as locals
        , funcReturnType = F64  -- Default return type for closures
        , funcBody = extractInstructions ++ bodyInstructions ++ [Return]
        }
        
      finalCtx = newCtx { fcLiftedFunctions = Map.insert funcName closureFunc (fcLiftedFunctions newCtx) }
  in (closureFunc, finalCtx)

-- Function call optimization
optimizeTailCalls :: [WASMInstruction] -> [WASMInstruction]
optimizeTailCalls instructions = case reverse instructions of
  (Return : Call name : rest) -> 
    -- Convert tail call to optimized tail call
    reverse rest ++ [Call name, Return]  -- Simplified - real tail call optimization is more complex
  _ -> instructions