{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.IR where

import Data.Text (Text)

-- IR for WebAssembly backend - simplified from AST
data IRProgram = IRProgram
  { irDataDefs :: [IRDataDef]
  , irFunctions :: [IRFunction]
  , irGlobals :: [IRGlobal]
  , irMainFunc :: Maybe Text
  } deriving (Show, Eq)

data IRDataDef = IRDataDef
  { irDataName :: Text
  , irDataConstructors :: [IRConstructor]
  } deriving (Show, Eq)

data IRConstructor = IRConstructor
  { irCtorName :: Text
  , irCtorFields :: [IRType]
  , irCtorTag :: Int  -- For WASM variant discriminant
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

data IRType
  = IRTNumber
  | IRTInt
  | IRTBool
  | IRTString
  | IRTFunction [IRType] IRType
  | IRTData Text  -- User-defined type
  | IRTUnknown
  deriving (Show, Eq)

data IRExpr
  = IRVar Text
  | IRLiteral IRLiteral
  | IRBinOp IRBinOp IRExpr IRExpr
  | IRCall IRExpr [IRExpr]
  | IRLambda [(Text, IRType)] IRType IRExpr
  | IRIf IRExpr IRExpr IRExpr
  | IRLet Text IRType IRExpr IRExpr
  | IRBlock [IRExpr]
  | IRCtorApp Text [IRExpr]  -- Constructor application
  | IRMatch IRExpr [IRCase]      -- Pattern matching
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

data IRCase = IRCase
  { irCasePattern :: IRPattern
  , irCaseBody :: IRExpr
  } deriving (Show, Eq)

data IRPattern
  = IRPatVar Text IRType
  | IRPatWildcard IRType
  | IRPatLiteral IRLiteral
  | IRPatConstructor Text [IRPattern]
  deriving (Show, Eq)