module Language.Memento.Parser.Metadata where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Data.HCoproduct (Injective (hInject))
import qualified Language.Memento.Parser.Class as PClass
import Language.Memento.Syntax.Definition (Definition (..))
import Language.Memento.Syntax.Literal (Literal (BoolLiteral, IntLiteral, NumberLiteral, StringLiteral))
import Language.Memento.Syntax.Metadata (Metadata (Metadata))
import Language.Memento.Syntax.Tag (KDefinition, KLiteral)
import Text.Megaparsec (MonadParsec (try), choice, getSourcePos, manyTill)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer as L

parseMetadata ::
  (MonadParsec s Text m) => m a -> m (Metadata f b, a)
parseMetadata p = do
  startPos <- getSourcePos
  a <- p
  endPos <- getSourcePos
  return (Metadata startPos endPos, a)

propagateMetadata :: Metadata f a -> Metadata f b -> Metadata f c
propagateMetadata (Metadata startPos1 _) (Metadata _ endPos2) = Metadata startPos1 endPos2