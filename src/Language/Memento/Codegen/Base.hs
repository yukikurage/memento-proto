{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Base (
  baseDefinitions,
) where

import Data.Text (Text)
import qualified Data.Text as T

baseDefinitions :: Text
baseDefinitions =
  T.unlines
    [ ""
    ]
