{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM.Types where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Memento.IR as IR

-- WebAssembly types
data WASMType = F64 | I32 | I64 | F32 deriving (Show, Eq, Ord)

-- WebAssembly function type signatures
data WASMFuncType = WASMFuncType
  { funcTypeParams :: [WASMType]
  , funcTypeResults :: [WASMType]
  } deriving (Show, Eq, Ord)

-- WebAssembly instructions
data WASMInstruction
  = LocalGet Text
  | LocalSet Text
  | LocalTee Text
  | GlobalGet Text
  | GlobalSet Text
  | I32Const Int
  | I64Const Int
  | F32Const Float
  | F64Const Double
  | F64Add | F64Sub | F64Mul | F64Div
  | I32Add | I32Sub | I32Mul | I32Div
  | F64Eq | F64Lt | F64Gt | F64Ne | F64Le | F64Ge
  | I32Eq | I32Lt | I32Gt | I32Ne | I32Le | I32Ge
  | I32Eqz | I64Eqz
  -- Type conversion
  | I32TruncF64S | I32TruncF64U
  | F64ConvertI32S | F64ConvertI32U
  | Call Text
  | CallIndirect Int
  | If WASMType [WASMInstruction] [WASMInstruction]
  | Block Text WASMType [WASMInstruction]
  | Loop Text WASMType [WASMInstruction]
  | Br Text
  | BrIf Text
  | BrTable [Text] Text
  | Return
  | Drop
  | Select
  | Unreachable
  -- Memory operations
  | I32Load Int Int
  | I64Load Int Int
  | F32Load Int Int
  | F64Load Int Int
  | I32Store Int Int
  | I64Store Int Int
  | F32Store Int Int
  | F64Store Int Int
  | MemorySize
  | MemoryGrow
  deriving (Show)

-- WebAssembly function definition
data WASMFunction = WASMFunction
  { funcName :: Text
  , funcTypeIndex :: Int
  , funcParams :: [(Text, WASMType)]    -- Function parameters
  , funcLocals :: [(Text, WASMType)]    -- Local variables (non-parameters)
  , funcReturnType :: WASMType          -- Function return type
  , funcBody :: [WASMInstruction]
  } deriving (Show)

-- WebAssembly global definition
data WASMGlobal = WASMGlobal
  { globalName :: Text
  , globalType :: WASMType
  , globalMutable :: Bool
  , globalInit :: [WASMInstruction]
  } deriving (Show)

-- WebAssembly export definition
data WASMExport = WASMExport
  { exportName :: Text
  , exportType :: WASMExportType
  , exportIndex :: Int
  } deriving (Show)

data WASMExportType = ExportFunc | ExportGlobal | ExportMemory | ExportTable deriving (Show)

-- WebAssembly memory definition
data WASMMemory = WASMMemory
  { memoryMin :: Int
  , memoryMax :: Maybe Int
  } deriving (Show)

-- WebAssembly table for function pointers
data WASMTable = WASMTable
  { tableMin :: Int
  , tableMax :: Maybe Int
  , tableElementType :: Text  -- "funcref"
  } deriving (Show)

-- Complete WebAssembly module
data WASMModule = WASMModule
  { wasmTypes :: [WASMFuncType]
  , wasmFunctions :: [WASMFunction]
  , wasmGlobals :: [WASMGlobal] 
  , wasmExports :: [WASMExport]
  , wasmMemory :: Maybe WASMMemory
  , wasmTable :: Maybe WASMTable
  } deriving (Show)

-- Convert IR types to WASM types
irTypeToWASM :: IR.IRType -> WASMType
irTypeToWASM IR.IRTNumber = F64
irTypeToWASM IR.IRTInt = I32
irTypeToWASM IR.IRTBool = I32
irTypeToWASM IR.IRTString = I32  -- String pointer
irTypeToWASM (IR.IRTFunction _ _) = I32  -- Function pointer
irTypeToWASM (IR.IRTData _) = I32  -- Data structure pointer
irTypeToWASM IR.IRTUnknown = F64  -- Default to F64

-- Convert WASM types to text representation
wasmTypeToText :: WASMType -> Text
wasmTypeToText F64 = "f64"
wasmTypeToText I32 = "i32"
wasmTypeToText I64 = "i64"
wasmTypeToText F32 = "f32"

-- Generate instruction text
instructionToText :: WASMInstruction -> Text
instructionToText (LocalGet name) = "local.get $" <> name
instructionToText (LocalSet name) = "local.set $" <> name
instructionToText (LocalTee name) = "local.tee $" <> name
instructionToText (GlobalGet name) = "global.get $" <> name
instructionToText (GlobalSet name) = "global.set $" <> name
instructionToText (I32Const i) = "i32.const " <> T.pack (show i)
instructionToText (I64Const i) = "i64.const " <> T.pack (show i)
instructionToText (F32Const f) = "f32.const " <> T.pack (show f)
instructionToText (F64Const d) = "f64.const " <> T.pack (show d)
instructionToText F64Add = "f64.add"
instructionToText F64Sub = "f64.sub"
instructionToText F64Mul = "f64.mul"
instructionToText F64Div = "f64.div"
instructionToText I32Add = "i32.add"
instructionToText I32Sub = "i32.sub"
instructionToText I32Mul = "i32.mul"
instructionToText I32Div = "i32.div_s"
instructionToText F64Eq = "f64.eq"
instructionToText F64Lt = "f64.lt"
instructionToText F64Gt = "f64.gt"
instructionToText F64Ne = "f64.ne"
instructionToText F64Le = "f64.le"
instructionToText F64Ge = "f64.ge"
instructionToText I32Eq = "i32.eq"
instructionToText I32Lt = "i32.lt_s"
instructionToText I32Gt = "i32.gt_s"
instructionToText I32Ne = "i32.ne"
instructionToText I32Le = "i32.le_s"
instructionToText I32Ge = "i32.ge_s"
instructionToText I32Eqz = "i32.eqz"
instructionToText I64Eqz = "i64.eqz"
instructionToText I32TruncF64S = "i32.trunc_f64_s"
instructionToText I32TruncF64U = "i32.trunc_f64_u"
instructionToText F64ConvertI32S = "f64.convert_i32_s"
instructionToText F64ConvertI32U = "f64.convert_i32_u"
instructionToText (Call name) = "call $" <> name
instructionToText (CallIndirect typeIdx) = "call_indirect " <> T.pack (show typeIdx)
instructionToText (If _ty thenInstrs elseInstrs) = 
  "if " <> 
  T.intercalate " " (map instructionToText thenInstrs) <> 
  " else " <> 
  T.intercalate " " (map instructionToText elseInstrs) <> 
  " end"
instructionToText (Block label _ty instrs) = 
  "block $" <> label <> " " <> T.intercalate " " (map instructionToText instrs) <> " end"
instructionToText (Loop label _ty instrs) = 
  "loop $" <> label <> " " <> T.intercalate " " (map instructionToText instrs) <> " end"
instructionToText (Br label) = "br $" <> label
instructionToText (BrIf label) = "br_if $" <> label
instructionToText (BrTable labels defaultLabel) = 
  "br_table " <> T.intercalate " " (map ("$" <>) labels) <> " $" <> defaultLabel
instructionToText Return = "return"
instructionToText Drop = "drop"
instructionToText Select = "select"
instructionToText Unreachable = "unreachable"
instructionToText (I32Load align offset) = "i32.load offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText (I64Load align offset) = "i64.load offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText (F32Load align offset) = "f32.load offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText (F64Load align offset) = "f64.load offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText (I32Store align offset) = "i32.store offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText (I64Store align offset) = "i64.store offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText (F32Store align offset) = "f32.store offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText (F64Store align offset) = "f64.store offset=" <> T.pack (show offset) <> " align=" <> T.pack (show align)
instructionToText MemorySize = "memory.size"
instructionToText MemoryGrow = "memory.grow"