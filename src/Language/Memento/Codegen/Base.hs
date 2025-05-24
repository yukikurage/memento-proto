{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Base (
  baseDefinitions
) where

import Data.Text (Text)
import qualified Data.Text as T

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
