{-# LANGUAGE NamedFieldPuns #-}

module Language.Memento.Codegen.Pattern where

import Data.List (foldl', zipWith)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- Memento Syntax Imports

-- Using qualified import for the Syntax coproduct type alias
-- AST is HFix (Syntax :*: Metadata)

import Language.Memento.Data.HCoproduct (Injective, hProject) -- Key assumption: Inject class and hProject are available
import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Syntax (AST)
import qualified Language.Memento.Syntax
import Language.Memento.Syntax.Literal (Literal (..))
import Language.Memento.Syntax.Pattern (Pattern (..))
import Language.Memento.Syntax.Tag (KLiteral, KPattern, KVariable)
import Language.Memento.Syntax.Variable (Variable (..))
