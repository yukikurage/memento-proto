{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Core where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (List)
import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Syntax (AST, unVariable)
import Language.Memento.Syntax.Tag (KVariable)
import Language.Memento.Syntax.Variable (Variable (Var))

-- | Generate a symbol name for a data type
getSymName :: AST KVariable -> Text
getSymName astV = "_SYM_" <> genVariable astV -- _SYM_DataName

-- | Generate a JavaScript variable name
genVariable :: AST KVariable -> Text
genVariable astV = case unHFix astV of
  meta :*: stx -> case unVariable stx of
    Var v -> v

-- | Generate a JavaScript const definition
genConstDef :: AST KVariable -> Text -> Text
genConstDef v e = "const " <> genVariable v <> " = " <> e <> ";"

genConstDefRaw :: Text -> Text -> Text
genConstDefRaw v e = "const " <> v <> " = " <> e <> ";"

-- | Match data constructor
matchData :: AST KVariable -> Text -> Text
matchData astV scrutinee = scrutinee <> "[0] === " <> getSymName astV

-- | Extract data from constructor
extractData :: Text -> Int -> List Text
extractData scrutinee numArgs =
  map (\idx -> scrutinee <> "[" <> T.pack (show idx) <> "]") [1 .. numArgs]