{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax

baseDefinitions :: Text
baseDefinitions =
  T.unlines
    [ "// ベース関数群"
    , "const ret = (x) => [\"ret\", x];"
    , ""
    , "// モナディックなbind関数"
    , "const bind = (c, f) => {"
    , "  if (c[0] === \"ret\") {"
    , "    return f(c[1]);"
    , "  } else {"
    , "    return [\"op\", c[1], c[2], (x) => bind(c[3](x), f)];"
    , "  }"
    , "};"
    , ""
    , "// グローバルエフェクトハンドラ"
    , "const globalHandler = (c) => {"
    , "  if (c[0] === \"ret\") {"
    , "    return [\"Success\", c[1]];"
    , "  } else {"
    , "    switch(c[1] /* name */) {"
    , "      case \"throw\":"
    , "        return [\"Failure\", c[2]];"
    , "      default:"
    , "        return [\"Error\", \"Unknown operation: \" + c[1]];"
    , "    }"
    , "  }"
    , "};"
    ]

-- | コードフォーマッティングのヘルパー関数
indent :: Int -> Text
indent level = T.replicate level "  "

-- | JavaScriptコードの生成
generateJS :: Expr -> Text
generateJS expr =
  T.concat
    [ "'use strict';\n\n" -- strict modeを有効化
    , baseDefinitions
    , "\n\nconst main = "
    , generateExpr expr
    , ";\n\n"
    , "console.log(globalHandler(main));\n" -- エフェクトハンドラを通して実行結果を出力
    ]

{- | 式のJavaScriptコードの生成
新しいセマンティクス: エフェクトシステムを使用したコード生成
-}
generateExpr :: Expr -> Text
generateExpr = \case
  -- 基本値
  Var name -> T.concat ["ret(", name, ")"]
  Number n -> T.concat ["ret(", T.pack $ show n, ")"]
  Bool b -> T.concat ["ret(", T.pack $ if b then "true" else "false", ")"]
  -- 二項演算子
  BinOp op e1 e2 ->
    let expr1 = generateExpr e1
        expr2 = generateExpr e2
     in T.unlines
          [ "bind(" <> expr1 <> ","
          , "  (v1) => bind(" <> expr2 <> ","
          , "    (v2) => ret(v1 " <> generateBinOp op <> " v2)"
          , "  )"
          , ")"
          ]
  -- [if cond then x else y] = bind(cond, (c) => c ? x : y)
  If cond then_ else_ ->
    let condExpr = generateExpr cond
        thenExpr = generateExpr then_
        elseExpr = generateExpr else_
     in T.unlines
          [ "bind(" <> condExpr <> ","
          , "  (c) => c"
          , "    ? " <> thenExpr
          , "    : " <> elseExpr
          , ")"
          ]
  -- [Lambda x. c] = ret((x) => [c])
  Lambda name _ body ->
    let bodyExpr = generateExpr body
     in T.unlines
          [ "ret((" <> name <> ") => {"
          , "  return " <> bodyExpr <> ";"
          , "})"
          ]
  -- [Apply f x] = bind([x], (v) => bind([f], (f) => f(v)))
  Apply func arg ->
    let funcExpr = generateExpr func
        argExpr = generateExpr arg
     in T.unlines
          [ "bind("
          , argExpr
          , ", (v) => bind("
          , funcExpr
          , ", (f) => f(v)))"
          ]
  -- do演算 - エフェクトを表現
  Do name ->
    T.concat ["[\"op\", \"", name, "\", null, (v) => ret(v)]"]

-- | 二項演算子の生成
generateBinOp :: BinOp -> Text
generateBinOp = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Eq -> "==="
  Lt -> "<"
  Gt -> ">"