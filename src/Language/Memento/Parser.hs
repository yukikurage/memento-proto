{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser (
  parseProgram,
  -- Potentially re-export other specific parsers if they are intended to be part of the public API
  -- For now, just parseProgram as per original module structure for external use.
  module Language.Memento.Parser.Core,
  module Language.Memento.Parser.Types,
  module Language.Memento.Parser.Patterns,
  module Language.Memento.Parser.Expressions,
  module Language.Memento.Parser.Definitions
) where

import Data.Text (Text)
import Data.Void (Void)
import Language.Memento.Syntax (Program (..)) -- Keep only Program from Syntax
import Language.Memento.Parser.Core
import Language.Memento.Parser.Types
import Language.Memento.Parser.Patterns
import Language.Memento.Parser.Expressions
import Language.Memento.Parser.Definitions
import Text.Megaparsec (Parsec, eof, many, parse, ParseErrorBundle, between) -- Keep only necessary Megaparsec imports

-- | プログラムのパーサー (トップレベル定義のリスト)
program :: Parser Program
program = Program <$> between sc eof (many definitionParser)

-- | プログラムのパース
parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse program ""
