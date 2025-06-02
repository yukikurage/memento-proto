{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Literal where

import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Memento.Syntax.Tag (KLiteral, KType)

data Literal (f :: Type -> Type) a where
  NumberLiteral :: Double -> Literal f KLiteral
  BoolLiteral :: Bool -> Literal f KLiteral
  StringLiteral :: Text -> Literal f KLiteral
  IntLiteral :: Int -> Literal f KLiteral

deriving instance Show (Literal f a)
deriving instance Eq (Literal f a)
deriving instance Ord (Literal f a)