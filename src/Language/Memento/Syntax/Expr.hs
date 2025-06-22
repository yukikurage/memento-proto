{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Memento.Syntax.Expr where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Base (List)
import Language.Memento.Data.HFunctor (HFunctor (hmap))
import Language.Memento.Syntax.Tag (KBinOp, KExpr, KLet, KLiteral, KPattern, KType, KVariable)

data Expr f a where
  EVar :: f KVariable -> Expr f KExpr
  ELiteral :: f KLiteral -> Expr f KExpr
  ELambda :: List (f KPattern, Maybe (f KType)) -> f KExpr -> Expr f KExpr
  EApply :: f KExpr -> List (f KExpr) -> Expr f KExpr
  EMatch :: List (f KExpr) -> List (List (f KPattern, Maybe (f KType)), f KExpr) -> Expr f KExpr -- Match はλ式の集合
  EIf :: f KExpr -> f KExpr -> f KExpr -> Expr f KExpr
  EBinOp :: f KBinOp -> f KExpr -> f KExpr -> Expr f KExpr
  EBlock :: List (f KLet) -> f KExpr -> Expr f KExpr

deriving instance
  ( Show (f KBinOp)
  , Show (f KLet)
  , Show (f KVariable)
  , Show (f KLiteral)
  , Show (f KPattern)
  , Show (f KType)
  , Show (f KExpr)
  ) =>
  Show (Expr f a)
deriving instance
  ( Eq (f KBinOp)
  , Eq (f KLet)
  , Eq (f KVariable)
  , Eq (f KLiteral)
  , Eq (f KPattern)
  , Eq (f KType)
  , Eq (f KExpr)
  ) =>
  Eq (Expr f a)
deriving instance
  ( Ord (f KBinOp)
  , Ord (f KLet)
  , Ord (f KVariable)
  , Ord (f KLiteral)
  , Ord (f KPattern)
  , Ord (f KType)
  , Ord (f KExpr)
  ) =>
  Ord (Expr f a)

data Let f a where
  Let :: f KPattern -> Maybe (f KType) -> f KExpr -> Let f KLet

deriving instance
  ( Show (f KPattern)
  , Show (f KType)
  , Show (f KExpr)
  ) =>
  Show (Let f a)
deriving instance
  ( Eq (f KPattern)
  , Eq (f KType)
  , Eq (f KExpr)
  ) =>
  Eq (Let f a)
deriving instance
  ( Ord (f KPattern)
  , Ord (f KType)
  , Ord (f KExpr)
  ) =>
  Ord (Let f a)

instance HFunctor Expr where
  hmap f (EVar v) = EVar (f v)
  hmap f (ELiteral l) = ELiteral (f l)
  hmap f (ELambda ps e) = ELambda (map (\(p, mt) -> (f p, fmap f mt)) ps) (f e)
  hmap f (EApply e es) = EApply (f e) (map f es)
  hmap f (EMatch es cs) = EMatch (map f es) (map (\(ps, e) -> (map (\(p, mt) -> (f p, fmap f mt)) ps, f e)) cs)
  hmap f (EIf e1 e2 e3) = EIf (f e1) (f e2) (f e3)
  hmap f (EBinOp op e1 e2) = EBinOp (f op) (f e1) (f e2)
  hmap f (EBlock ls e) = EBlock (map f ls) (f e)

instance HFunctor Let where
  hmap f (Let p mt e) = Let (f p) (fmap f mt) (f e)