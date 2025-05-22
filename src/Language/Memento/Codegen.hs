{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax

-- | JavaScriptコードの生成
generateJS :: Expr -> Text
generateJS expr =
  T.concat
    [ "'use strict';\n" -- strict modeを有効化
    , "console.log(\n"
    , generateExpr expr
    , "\n);\n" -- 最後の式の値を出力
    ]

-- | 式のJavaScriptコードの生成
generateExpr :: Expr -> Text
generateExpr = \case
  Var name -> name
  Number n -> T.pack $ show n
  Bool b -> T.pack $ if b then "true" else "false"
  BinOp op e1 e2 ->
    T.concat
      [ "("
      , generateExpr e1
      , " "
      , generateBinOp op
      , " "
      , generateExpr e2
      , ")"
      ]
  If cond then_ else_ ->
    T.concat
      [ "("
      , generateExpr cond
      , " ? "
      , generateExpr then_
      , " : "
      , generateExpr else_
      , ")"
      ]
  Lambda name _ body ->
    T.concat
      [ "(("
      , name
      , ") => "
      , generateExpr body
      , ")"
      ]
  Apply func arg ->
    T.concat
      [ "("
      , generateExpr func
      , ")("
      , generateExpr arg
      , ")"
      ]

-- | 二項演算子の生成
generateBinOp :: BinOp -> Text
generateBinOp = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Eq -> "==="
  Lt -> "<"