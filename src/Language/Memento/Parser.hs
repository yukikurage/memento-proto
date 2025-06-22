{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Memento.Parser where

import Data.Text
import qualified Data.Text as T
import Data.Void
import Language.Memento.Data.HFix (HFix (..))
import Language.Memento.Data.HProduct ((:*:) (..))
import Language.Memento.Parser.Class
import qualified Language.Memento.Parser.Core as Core
import qualified Language.Memento.Parser.Definitions as Def
import qualified Language.Memento.Parser.Expressions as Expr
import qualified Language.Memento.Parser.Literal as Lit
import qualified Language.Memento.Parser.Metadata as Meta
import qualified Language.Memento.Parser.Patterns as Pat
import qualified Language.Memento.Parser.Programs as Prog
import qualified Language.Memento.Parser.Types as Types
import qualified Language.Memento.Parser.Variables as Var
import Language.Memento.Syntax (AST, Syntax)
import Language.Memento.Syntax.Metadata (Metadata)
import Language.Memento.Syntax.Tag (KProgram)
import Language.Memento.Syntax.Variable (Variable)
import Text.Megaparsec
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

type Parser = Parsec Void Text

instance FixParser Syntax AST Parser where
  parseFix :: Parser (Syntax AST a) -> Parser (AST a)
  parseFix p = do
    (meta, hast) <- Meta.parseMetadata p
    return $ HFix{unHFix = meta :*: hast}

instance PropagateFix Syntax AST where
  propagateFix :: AST c -> AST b -> Syntax AST a -> AST a
  propagateFix (HFix{unHFix = meta :*: _}) (HFix{unHFix = meta' :*: _}) syntax =
    let meta'' = Meta.propagateMetadata meta meta'
     in HFix{unHFix = meta'' :*: syntax}

instance CoreParser Parser where
  parseLexeme :: Parser a -> Parser a
  parseLexeme = Core.parseLexeme
  parseSymbol :: Text -> Parser Text
  parseSymbol = Core.parseSymbol
  parseParens :: Parser a -> Parser a
  parseParens = Core.parseParens
  parseBraces :: Parser a -> Parser a
  parseBraces = Core.parseBraces
  parseBrackets :: Parser a -> Parser a
  parseBrackets = Core.parseBrackets
  parseAngleBrackets :: Parser a -> Parser a
  parseAngleBrackets = Core.parseAngleBrackets
  parseReservedWord :: Text -> Parser ()
  parseReservedWord = Core.parseReservedWord
  parseIdentifier :: Parser Text
  parseIdentifier = Core.parseIdentifier

instance VariableParser AST Parser where
  parseVariable = Var.parseVariable @Syntax

instance LiteralParser AST Parser where
  parseLiteral = Lit.parseLiteral @Syntax

instance ExpressionParser AST Parser where
  parseExpr = Expr.parseExpr @Syntax

instance PatternParser AST Parser where
  parsePattern = Pat.parsePattern @Syntax

instance DefinitionParser AST Parser where
  parseDefinition = Def.parseDefinition @Syntax

instance MTypeParser AST Parser where
  parseMType = Types.parseMType @Syntax

instance ProgramParser AST Parser where
  parseProgram = Prog.parseProgram @Syntax

parseAST :: Parser (AST KProgram)
parseAST = parseProgram <* eof

{- | Convenience wrapper: parse a complete program from Text.
  Exposes the same signature that earlier client code expected.
-}
parseProgramText :: T.Text -> Either (ParseErrorBundle T.Text Void) (AST KProgram)
parseProgramText = parse (Core.parseLexeme (return ()) *> parseAST) "<stdin>"