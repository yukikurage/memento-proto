{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM.Memory where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.CodegenWASM.Types
import qualified Language.Memento.IR as IR

-- Memory layout constants
type Address = Int
type Size = Int

-- Object layout in memory
data ObjectLayout = ObjectLayout
  { objTag :: Int        -- Constructor tag (for ADTs)
  , objSize :: Size      -- Size in bytes
  , objFields :: [Address] -- Field offsets
  } deriving (Show)

-- Runtime representation of values
data RuntimeValue
  = RVImmediate WASMType -- Immediate value (numbers, booleans)
  | RVPointer ObjectLayout -- Heap-allocated object
  | RVClosure [Text] Int   -- Closure with captured variables and function index
  deriving (Show)

-- Memory layout configuration
memoryPageSize :: Int
memoryPageSize = 65536  -- 64KB

initialMemoryPages :: Int  
initialMemoryPages = 1

heapStartOffset :: Address
heapStartOffset = 1024  -- Start heap after static data

-- Object header layout (8 bytes)
objectHeaderSize :: Size
objectHeaderSize = 8

tagOffset :: Address
tagOffset = 0

sizeOffset :: Address
sizeOffset = 4

-- Generate memory allocation code
allocateObject :: Int -> Size -> [WASMInstruction]
allocateObject tag size = 
  [ -- Store tag at current heap pointer
    GlobalGet "heap_ptr"
  , I32Const tag
  , I32Store 4 0  -- align=4, offset=0
  , -- Store size at heap_ptr + 4  
    GlobalGet "heap_ptr"
  , I32Const size
  , I32Store 4 4  -- align=4, offset=4
  , -- Save current heap pointer as obj_ptr and keep on stack
    GlobalGet "heap_ptr"
  , LocalTee "obj_ptr"  -- Save object pointer in local variable and keep on stack
  , -- Update heap pointer
    GlobalGet "heap_ptr"
  , I32Const (objectHeaderSize + size)
  , I32Add
  , GlobalSet "heap_ptr"
  ]

-- Load object tag
loadObjectTag :: Address -> [WASMInstruction]
loadObjectTag addr =
  [ I32Const addr
  , I32Load 4 tagOffset
  ]

-- Load object field
loadObjectField :: Address -> Int -> [WASMInstruction]
loadObjectField addr fieldIndex =
  [ I32Const addr
  , I32Const (objectHeaderSize + fieldIndex * 4)
  , I32Add
  , I32Load 4 0
  ]

-- Store object field
storeObjectField :: Address -> Int -> [WASMInstruction]
storeObjectField addr fieldIndex =
  [ I32Const addr
  , I32Const (objectHeaderSize + fieldIndex * 4)
  , I32Add
  , I32Store 4 0  -- Value should be on stack
  ]

-- Generate data constructor layout
generateConstructorLayout :: IR.IRConstructor -> ObjectLayout
generateConstructorLayout (IR.IRConstructor _name fields tag) =
  let fieldSize = length fields * 4  -- 4 bytes per field (pointer size)
      fieldOffsets = [i * 4 | i <- [0..length fields - 1]]
  in ObjectLayout
     { objTag = tag
     , objSize = fieldSize
     , objFields = fieldOffsets
     }

-- Generate constructor allocation code
generateConstructorAllocation :: IR.IRConstructor -> [Text] -> [WASMInstruction]
generateConstructorAllocation ctor@(IR.IRConstructor _name fields tag) argNames =
  let layout = generateConstructorLayout ctor
      fieldCount = length fields
  in allocateObject tag (objSize layout) ++
     -- Store the allocated object address
     [ LocalTee "obj_ptr" ] ++
     -- Store each field
     concatMap (\(i, argName) -> 
       [ LocalGet "obj_ptr"
       , LocalGet argName
       , I32Store 4 (objectHeaderSize + i * 4)
       ]) (zip [0..] argNames) ++
     -- Return object pointer
     [ LocalGet "obj_ptr" ]

-- Pattern matching support
generateTagCheck :: Text -> Int -> [WASMInstruction]
generateTagCheck objVar expectedTag =
  [ LocalGet objVar
  , I32Load 4 tagOffset
  , I32Const expectedTag
  , I32Eq
  ]

-- Extract field from object
generateFieldExtraction :: Text -> Int -> Text -> [WASMInstruction]
generateFieldExtraction objVar fieldIndex resultVar =
  [ LocalGet objVar
  , I32Load 4 (objectHeaderSize + fieldIndex * 4)
  , F64ConvertI32S  -- Convert loaded i32 to f64 for result
  , LocalSet resultVar
  ]

-- Closure support
closureHeaderSize :: Size
closureHeaderSize = 12  -- tag(4) + function_index(4) + captured_count(4)

generateClosureAllocation :: Int -> [Text] -> [WASMInstruction]
generateClosureAllocation funcIndex capturedVars =
  let capturedCount = length capturedVars
      closureSize = closureHeaderSize + capturedCount * 4
  in allocateObject (-1) closureSize ++  -- -1 tag for closures
     [ LocalTee "closure_ptr"
     -- Store function index
     , LocalGet "closure_ptr"
     , I32Const funcIndex
     , I32Store 4 4
     -- Store captured count  
     , LocalGet "closure_ptr"
     , I32Const capturedCount
     , I32Store 4 8
     ] ++
     -- Store captured variables
     concatMap (\(i, varName) ->
       [ LocalGet "closure_ptr"
       , LocalGet varName
       , I32Store 4 (closureHeaderSize + i * 4)
       ]) (zip [0..] capturedVars) ++
     [ LocalGet "closure_ptr" ]

-- Call closure
generateClosureCall :: [Text] -> [WASMInstruction]
generateClosureCall argNames =
  [ -- Load function index from closure
    LocalGet "closure_ptr"
  , I32Load 4 4
  ] ++
  -- Push closure pointer as first argument
  [ LocalGet "closure_ptr" ] ++
  -- Push regular arguments
  map LocalGet argNames ++
  [ CallIndirect 0  -- Type index for closure calls
  ]

-- String support (UTF-8 encoding)
generateStringAllocation :: Text -> [WASMInstruction]
generateStringAllocation str =
  let utf8Bytes = T.length str  -- Simplified: assume ASCII
      stringSize = 4 + utf8Bytes  -- length(4) + bytes
  in allocateObject 0 stringSize ++  -- 0 tag for strings
     [ -- Save object pointer in string_ptr and keep it on stack for return
       LocalTee "string_ptr"
     -- Store length
     , LocalGet "string_ptr"
     , I32Const utf8Bytes
     , I32Store 4 4
     -- TODO: Store actual string bytes
     , LocalGet "string_ptr"  -- Return string pointer
     ]

-- Global heap pointer management
heapGlobals :: [WASMGlobal]
heapGlobals =
  [ WASMGlobal "heap_ptr" I32 True [I32Const heapStartOffset]
  ]