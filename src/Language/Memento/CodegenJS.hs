{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenJS 
  ( generateJSFromIR
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Memento.IR as IR

-- | Generate JavaScript code from primitive IR
generateJSFromIR :: IR.IRProgram -> Text
generateJSFromIR (IR.IRProgram irFunctions irGlobals mainFunc) = 
  T.concat
    [ "'use strict';\n\n"
    , runtimeHelpers
    , "\n\n"
    , generateGlobals irGlobals
    , "\n\n"
    , generateFunctions irFunctions
    , "\n\n"
    , generateMainExecution mainFunc
    ]

-- | Runtime helper functions for primitive operations
runtimeHelpers :: Text
runtimeHelpers = T.unlines
  [ "// Runtime helpers for Memento primitive IR"
  , "function mementoAdd(a, b) { return a + b; }"
  , "function mementoSub(a, b) { return a - b; }"
  , "function mementoMul(a, b) { return a * b; }"
  , "function mementoDiv(a, b) { return a / b; }"
  , "function mementoEq(a, b) { return a === b; }"
  , "function mementoLt(a, b) { return a < b; }"
  , "function mementoGt(a, b) { return a > b; }"
  ]

-- | Generate JavaScript global variable declarations
generateGlobals :: [IR.IRGlobal] -> Text
generateGlobals globals = 
  T.intercalate "\n" (map generateGlobal globals)

generateGlobal :: IR.IRGlobal -> Text
generateGlobal (IR.IRGlobal name _ty expr) = 
  "const " <> safeName name <> " = " <> generateExpr expr <> ";"

-- | Generate JavaScript function declarations
generateFunctions :: [IR.IRFunction] -> Text
generateFunctions functions = 
  T.intercalate "\n\n" (map generateFunction functions)

generateFunction :: IR.IRFunction -> Text
generateFunction (IR.IRFunction name params _returnType body) = 
  let paramNames = T.intercalate ", " (map (safeName . fst) params)
  in "function " <> safeName name <> "(" <> paramNames <> ") {\n" <>
     "  return " <> generateExpr body <> ";\n" <>
     "}"

-- | Generate JavaScript expressions from IR
generateExpr :: IR.IRExpr -> Text
generateExpr expr = case expr of
  IR.IRVar name -> safeName name
  
  IR.IRLiteral lit -> generateLiteral lit
  
  IR.IRBinOp op left right -> 
    let opFunc = case op of
          IR.IRAdd -> "mementoAdd"
          IR.IRSub -> "mementoSub"
          IR.IRMul -> "mementoMul"
          IR.IRDiv -> "mementoDiv"
          IR.IREq -> "mementoEq"
          IR.IRLt -> "mementoLt"
          IR.IRGt -> "mementoGt"
    in opFunc <> "(" <> generateExpr left <> ", " <> generateExpr right <> ")"
  
  IR.IRCall func args -> 
    generateExpr func <> "(" <> T.intercalate ", " (map generateExpr args) <> ")"
  
  IR.IRLambda params _returnType body -> 
    let paramNames = T.intercalate ", " (map (safeName . fst) params)
    in "((" <> paramNames <> ") => " <> generateExpr body <> ")"
  
  IR.IRIf cond thenExpr elseExpr -> 
    "(" <> generateExpr cond <> " ? " <> generateExpr thenExpr <> " : " <> generateExpr elseExpr <> ")"
  
  IR.IRLet name _ty value body -> 
    "((() => { const " <> safeName name <> " = " <> generateExpr value <> "; return " <> generateExpr body <> "; })())"
  
  IR.IRBlock exprs -> 
    case exprs of
      [] -> "undefined"
      [e] -> generateExpr e
      _ -> "((() => { " <> T.intercalate "; " (map (\e -> "(" <> generateExpr e <> ")") exprs) <> "; })())"

-- | Generate JavaScript literals
generateLiteral :: IR.IRLiteral -> Text
generateLiteral lit = case lit of
  IR.IRNumberLit n -> T.pack (show n)
  IR.IRIntLit i -> T.pack (show i)
  IR.IRBoolLit True -> "true"
  IR.IRBoolLit False -> "false"
  IR.IRStringLit s -> "\"" <> escapeString s <> "\""

-- | Generate main execution block
generateMainExecution :: Maybe Text -> Text
generateMainExecution Nothing = ""
generateMainExecution (Just mainName) = 
  "\n// Execute main function\nconsole.log(JSON.stringify(" <> safeName mainName <> "()));"

-- | Escape string literals for JavaScript
escapeString :: Text -> Text
escapeString = T.replace "\"" "\\\"" . T.replace "\\" "\\\\" . T.replace "\n" "\\n" . T.replace "\r" "\\r" . T.replace "\t" "\\t"

-- | Make JavaScript identifiers safe
safeName :: Text -> Text
safeName name = 
  -- Replace any characters that aren't valid in JS identifiers
  T.map (\c -> if c `elem` ("-+*/<>=!@#$%^&()[]{}|;:,." :: String) then '_' else c) name