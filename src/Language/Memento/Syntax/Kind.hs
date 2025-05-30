{-# LANGUAGE KindSignatures #-}

module Language.Memento.Syntax.Kind where

import Data.Kind (Type)

{- Tokens -}

-- | `KToken a` represents a token of type `a`
data KToken :: Type -> Type

{- Types -}
data KType :: Type

-- data KEffects :: Type

{- Definitions -}
data KDefinition :: Type

{- Expressions -}
data KExpr :: Type
data KPattern :: Type
data KBinOp :: Type

-- data KHandlerClause :: Type

{- Programs -}
data KProgram :: Type