{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.IR where

import Data.Text (Text)

-- Primitive IR for WASM/JS backends - no data types or pattern matching
data IRProgram = IRProgram
  { irFunctions :: [IRFunction]
  , irGlobals :: [IRGlobal]
  , irMainFunc :: Maybe Text
  } deriving (Show, Eq)

data IRFunction = IRFunction
  { irFuncName :: Text
  , irFuncParams :: [(Text, IRType)]
  , irFuncReturnType :: IRType
  , irFuncBody :: IRExpr
  } deriving (Show, Eq)

data IRGlobal = IRGlobal
  { irGlobalName :: Text
  , irGlobalType :: IRType
  , irGlobalValue :: IRExpr
  } deriving (Show, Eq)

-- Primitive types only - no user-defined data types
data IRType
  = IRTNumber
  | IRTInt
  | IRTBool
  | IRTString
  | IRTFunction [IRType] IRType
  | IRTUnknown
  deriving (Show, Eq)

-- Primitive expressions only - no pattern matching or data constructors
data IRExpr
  = IRVar Text
  | IRLiteral IRLiteral
  | IRBinOp IRBinOp IRExpr IRExpr
  | IRCall IRExpr [IRExpr]
  | IRLambda [(Text, IRType)] IRType IRExpr
  | IRIf IRExpr IRExpr IRExpr
  | IRLet Text IRType IRExpr IRExpr
  | IRBlock [IRExpr]
  deriving (Show, Eq)

data IRLiteral
  = IRNumberLit Double
  | IRIntLit Int
  | IRBoolLit Bool
  | IRStringLit Text
  deriving (Show, Eq)

data IRBinOp
  = IRAdd | IRSub | IRMul | IRDiv
  | IREq | IRLt | IRGt
  deriving (Show, Eq)