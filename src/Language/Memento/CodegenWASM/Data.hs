{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM.Data where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Memento.CodegenWASM.Types
import Language.Memento.CodegenWASM.Memory
import qualified Language.Memento.IR as IR

-- Simplified data context for primitive IR (no user-defined data types)
data DataContext = DataContext
  { dcConstructorTags :: Map Text Int      -- Empty for primitive IR
  , dcConstructorTypes :: Map Text [WASMType]  -- Empty for primitive IR
  , dcTypeLayouts :: Map Text TypeLayout  -- Empty for primitive IR
  } deriving (Show)

-- Memory layout for a data type (kept for backward compatibility)
data TypeLayout = TypeLayout
  { tlTypeName :: Text
  , tlConstructors :: [ConstructorLayout]
  , tlMaxSize :: Int  -- Maximum size among all constructors
  , tlAlignment :: Int
  } deriving (Show)

-- Memory layout for a constructor (kept for backward compatibility)
data ConstructorLayout = ConstructorLayout
  { clName :: Text
  , clTag :: Int
  , clFields :: [FieldLayout]
  , clTotalSize :: Int
  } deriving (Show)

-- Field layout information (kept for backward compatibility)
data FieldLayout = FieldLayout
  { flOffset :: Int
  , flSize :: Int
  , flType :: WASMType
  , flAlignment :: Int
  } deriving (Show)

-- Standard sizes and alignments
sizeOf :: WASMType -> Int
sizeOf I32 = 4
sizeOf I64 = 8
sizeOf F32 = 4
sizeOf F64 = 8

alignmentOf :: WASMType -> Int
alignmentOf = sizeOf  -- Simple alignment equals size

-- Calculate aligned offset
alignOffset :: Int -> Int -> Int
alignOffset offset alignment = ((offset + alignment - 1) `div` alignment) * alignment

-- Create empty data context for primitive IR (no user-defined data types)
buildDataContext :: () -> DataContext
buildDataContext () = DataContext Map.empty Map.empty Map.empty

-- Note: All data constructor and pattern matching functions removed for primitive IR
-- Primitive IR only supports basic types and functions, no user-defined data types

-- Store instruction helper (moved from removed code)
storeInstruction :: WASMType -> Int -> Int -> WASMInstruction
storeInstruction I32 alignment offset = I32Store alignment offset
storeInstruction I64 alignment offset = I64Store alignment offset  
storeInstruction F32 alignment offset = F32Store alignment offset
storeInstruction F64 alignment offset = F64Store alignment offset