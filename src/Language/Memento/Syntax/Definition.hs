{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Definition where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Base (List)
import Language.Memento.Syntax.Tag (KDefinition, KExpr, KType, KVariable)

data Definition f a where
  ValDef ::
    f KVariable ->
    f KType ->
    f KExpr ->
    Definition f KDefinition
  -- 今回の言語では data は単一のコンストラクタしか持たない
  -- Union を使って複数コンストラクタをまとめる
  -- data Just : (number) => Just
  -- data Nothing : () => Nothing
  -- type Maybe = Just | Nothing
  -- justVal : Maybe := Just(10)
  -- nothingVal : Maybe := Nothing()
  DataDef ::
    f KVariable -> -- Data type & constructor name
    f KType -> -- Data constructor type
    Definition f KDefinition

deriving instance (Show (f KVariable), Show (f KType), Show (f KExpr)) => Show (Definition f a)
deriving instance (Eq (f KVariable), Eq (f KType), Eq (f KExpr)) => Eq (Definition f a)
deriving instance (Ord (f KVariable), Ord (f KType), Ord (f KExpr)) => Ord (Definition f a)