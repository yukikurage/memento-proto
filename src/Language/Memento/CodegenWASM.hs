{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM 
  ( generateWASM
  , generateWASMFromTypedAST
  , WASMModule(..)
  , WASMFunction(..)
  , WASMType(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Language.Memento.IR as IR
import Language.Memento.CodegenWASM.Types
import Language.Memento.CodegenWASM.Memory
import qualified Language.Memento.CodegenWASM.Functions as Func
import Language.Memento.CodegenWASM.Expressions
import Language.Memento.CodegenWASM.Control
import qualified Language.Memento.CodegenWASM.Data as Data
import Language.Memento.TypedASTExtractor
import Language.Memento.TypeSolver.TypedAST.TypeInfo (TypedAST)
import Language.Memento.Syntax.Tag (KProgram)
import Language.Memento.Syntax (AST)

-- Main entry point for WASM generation
generateWASM :: IR.IRProgram -> Text
generateWASM irProgram = 
  let wasmModule = compileProgram irProgram
  in generateWAT wasmModule

-- Enhanced entry point for WASM generation from TypedAST and AST
generateWASMFromTypedAST :: AST KProgram -> TypedAST KProgram -> Text
generateWASMFromTypedAST astProgram typedAST =
  let -- Extract type information from TypedAST
      typedInfo = extractTypedASTInfo typedAST
      -- Use enhanced IR generation with type information
      irProgram = enhancedLowerToIR astProgram typedInfo
      -- Generate WASM using existing pipeline
      wasmModule = compileProgram irProgram
  in generateWAT wasmModule

-- Compile entire IR program to WASM module
compileProgram :: IR.IRProgram -> WASMModule
compileProgram (IR.IRProgram irFunctions irGlobals mainFunc) = 
  let -- Lift lambdas to top-level functions
      (liftedFunctions, liftedGlobals) = liftProgramLambdas irFunctions irGlobals
      
      -- Compile all functions (no data constructors in primitive IR)
      globalFunctions = map Func.compileGlobalValue liftedGlobals
      realFunctions = map Func.compileFunctionDef liftedFunctions
      allFunctions = globalFunctions ++ realFunctions
      
      -- Assign type indices
      (typedFunctions, funcTypes) = Func.assignTypeIndices allFunctions
      
      -- Generate exports
      exports = generateExports mainFunc typedFunctions
      
      -- Generate memory and globals
      memory = WASMMemory 1 Nothing  -- 64KB initial memory
      globals = heapGlobals  -- Heap management globals
      
      -- Generate function table for higher-order functions
      table = Func.generateFunctionTable typedFunctions
      
  in WASMModule
     { wasmTypes = funcTypes
     , wasmFunctions = typedFunctions
     , wasmGlobals = globals
     , wasmExports = exports
     , wasmMemory = Just memory
     , wasmTable = Just table
     }

-- Lambda lifting for entire program
liftProgramLambdas :: [IR.IRFunction] -> [IR.IRGlobal] -> ([IR.IRFunction], [IR.IRGlobal])
liftProgramLambdas functions globals =
  let functionCtx = Func.emptyFunctionContext
      (liftedFunctions, _) = foldl liftFunctionLambdas ([], functionCtx) functions
      (liftedGlobals, _) = foldl liftGlobalLambdas ([], functionCtx) globals
  in (liftedFunctions, liftedGlobals)

-- Lift lambdas in function definitions
liftFunctionLambdas :: ([IR.IRFunction], Func.FunctionContext) -> IR.IRFunction -> ([IR.IRFunction], Func.FunctionContext)
liftFunctionLambdas (acc, ctx) (IR.IRFunction name params returnType body) =
  let (liftedBody, newCtx) = Func.liftLambdas body ctx
      liftedFunction = IR.IRFunction name params returnType liftedBody
  in (liftedFunction : acc, newCtx)

-- Lift lambdas in global definitions
liftGlobalLambdas :: ([IR.IRGlobal], Func.FunctionContext) -> IR.IRGlobal -> ([IR.IRGlobal], Func.FunctionContext)
liftGlobalLambdas (acc, ctx) (IR.IRGlobal name ty expr) =
  let (liftedExpr, newCtx) = Func.liftLambdas expr ctx
      liftedGlobal = IR.IRGlobal name ty liftedExpr
  in (liftedGlobal : acc, newCtx)

-- Generate exports for main function and public API
generateExports :: Maybe Text -> [WASMFunction] -> [WASMExport]
generateExports mainFunc functions =
  let mainExports = case mainFunc of
        Just name -> case findFunctionIndex name functions of
          Just idx -> [WASMExport name ExportFunc idx]
          Nothing -> []
        Nothing -> []
      
      -- Export memory for external access
      memoryExports = [WASMExport "memory" ExportMemory 0]
      
  in mainExports ++ memoryExports

-- Find function index by name
findFunctionIndex :: Text -> [WASMFunction] -> Maybe Int
findFunctionIndex name functions = 
  let indices = [i | (i, func) <- zip [0..] functions, funcName func == name]
  in case indices of
       (i:_) -> Just i
       [] -> Nothing

-- Enhanced WAT generation with all features
generateWAT :: WASMModule -> Text
generateWAT (WASMModule types functions globals exports memory table) = 
  "(module\n" <>
  generateMemorySection memory <>
  generateTableSection table <>
  T.intercalate "\n" (map generateFuncTypeWAT (zip [0..] types)) <> "\n" <>
  T.intercalate "\n" (map (generateFunctionWAT types) (zip [0..] functions)) <> "\n" <>
  T.intercalate "\n" (map generateGlobalWAT globals) <> "\n" <>
  T.intercalate "\n" (map generateExportWAT exports) <> "\n" <>
  ")"

-- Generate memory section
generateMemorySection :: Maybe WASMMemory -> Text
generateMemorySection Nothing = ""
generateMemorySection (Just (WASMMemory minPages maxPages)) = 
  "(memory " <> T.pack (show minPages) <> 
  case maxPages of
    Nothing -> ")\n"
    Just max -> " " <> T.pack (show max) <> ")\n"

-- Generate table section
generateTableSection :: Maybe WASMTable -> Text
generateTableSection Nothing = ""
generateTableSection (Just (WASMTable minSize maxSize elemType)) =
  "(table " <> T.pack (show minSize) <>
  case maxSize of
    Nothing -> " " <> elemType <> ")\n"
    Just max -> " " <> T.pack (show max) <> " " <> elemType <> ")\n"

-- Generate function type in WAT format
generateFuncTypeWAT :: (Int, WASMFuncType) -> Text
generateFuncTypeWAT (idx, WASMFuncType params results) = 
  "(type $type" <> T.pack (show idx) <> " (func" <>
  (if null params then "" else " (param " <> T.intercalate " " (map wasmTypeToText params) <> ")") <>
  (if null results then "" else " (result " <> T.intercalate " " (map wasmTypeToText results) <> ")") <>
  "))"

-- Generate function in WAT format with proper parameter handling
generateFunctionWAT :: [WASMFuncType] -> (Int, WASMFunction) -> Text
generateFunctionWAT types (idx, WASMFunction name typeIdx params locals returnType body) = 
  let actualTypeIdx = if typeIdx < length types then typeIdx else 0
      
      paramDecls = T.intercalate "" (map generateParamWAT params)
      localDecls = T.intercalate "" (map generateLocalWAT locals)
      bodyText = T.intercalate "\n  " (map instructionToText body)
      
  in "(func $" <> name <> 
     paramDecls <> 
     " (result " <> wasmTypeToText returnType <> ")" <>
     localDecls <>
     "\n  " <> bodyText <>
     "\n)"

-- Generate parameter declaration
generateParamWAT :: (Text, WASMType) -> Text
generateParamWAT (name, ty) = " (param $" <> name <> " " <> wasmTypeToText ty <> ")"

-- Generate local variable declaration
generateLocalWAT :: (Text, WASMType) -> Text
generateLocalWAT (name, ty) = " (local $" <> name <> " " <> wasmTypeToText ty <> ")"

-- Generate global in WAT format
generateGlobalWAT :: WASMGlobal -> Text
generateGlobalWAT (WASMGlobal name ty mut initInstructions) = 
  "(global $" <> name <> " " <> 
  (if mut then "(mut " else "") <> wasmTypeToText ty <> (if mut then ")" else "") <>
  " (" <> T.intercalate " " (map instructionToText initInstructions) <> "))"

-- Generate export in WAT format
generateExportWAT :: WASMExport -> Text
generateExportWAT (WASMExport name ExportFunc idx) = 
  "(export \"" <> name <> "\" (func " <> T.pack (show idx) <> "))"
generateExportWAT (WASMExport name ExportGlobal idx) = 
  "(export \"" <> name <> "\" (global " <> T.pack (show idx) <> "))"
generateExportWAT (WASMExport name ExportMemory idx) = 
  "(export \"" <> name <> "\" (memory " <> T.pack (show idx) <> "))"
generateExportWAT (WASMExport name ExportTable idx) = 
  "(export \"" <> name <> "\" (table " <> T.pack (show idx) <> "))"

