{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser (
  parseProgram,
  -- Potentially re-export other specific parsers if they are intended to be part of the public API
  -- For now, just parseProgram as per original module structure for external use.
  module Language.Memento.Parser.Core,
  module Language.Memento.Parser.Types,
  module Language.Memento.Parser.Patterns,
  module Language.Memento.Parser.Expressions,
  module Language.Memento.Parser.Definitions,
) where

-- Keep only Program from Syntax

-- Keep only necessary Megaparsec imports
import Control.Monad.State (evalState)
import Data.Text (Text)
import Data.Void (Void)
import Language.Memento.Parser.Core
import Language.Memento.Parser.Definitions
import Language.Memento.Parser.Expressions
import Language.Memento.Parser.Patterns
import Language.Memento.Parser.Types
import Language.Memento.Syntax (Program (..))
import Text.Megaparsec (ParseErrorBundle, Parsec, between, eof, many, parse, runParserT)

-- | プログラムのパーサー (トップレベル定義のリスト)
program :: Parser Program
program = Program <$> between sc eof (many (definitionParser expr))

-- | プログラムのパース
parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram txt = evalState (runParserT program "" txt) 0
