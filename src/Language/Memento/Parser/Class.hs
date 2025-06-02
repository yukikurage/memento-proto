{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Memento.Parser.Class where

import Data.Text (Text)
import Language.Memento.Syntax.Definition (Definition)
import Language.Memento.Syntax.Expr (Expr)
import Language.Memento.Syntax.Literal (Literal)
import Language.Memento.Syntax.MType (MType)
import Language.Memento.Syntax.Pattern (Pattern)
import Language.Memento.Syntax.Tag (KBinOp, KDefinition, KExpr, KLiteral, KPattern, KProgram, KType, KVariable)
import Language.Memento.Syntax.Variable (Variable)

class CoreParser m where
  parseLexeme :: m a -> m a
  parseSymbol :: Text -> m Text
  parseParens :: m a -> m a
  parseBraces :: m a -> m a
  parseBrackets :: m a -> m a
  parseReservedWord :: Text -> m ()
  parseIdentifier :: m Text

class VariableParser f m where
  parseVariable :: m (f KVariable)

class LiteralParser f m where
  parseLiteral :: m (f KLiteral)

class ExpressionParser f m where
  parseExpr :: m (f KExpr)

class PatternParser f m where
  parsePattern :: m (f KPattern)

class DefinitionParser f m where
  parseDefinition :: m (f KDefinition)

class MTypeParser f m where
  parseMType :: m (f KType)

class ProgramParser f m where
  parseProgram :: m (f KProgram)

class FixParser h f m where
  parseFix :: m (h f a) -> m (f a) -- Fix point を取るようなパーサ (Metadata の付与等を行う)

-- パースしたブロック (第三引数) の左端と右端 (第一、二引数) の情報から、メタデータ等を付与できる
-- FixParser を純粋、あるいは遅延して行う事が可能
-- 汚くない？？？
class PropagateFix h f where
  propagateFix :: f c -> f b -> h f a -> f a