{-# LANGUAGE KindSignatures #-}

module Language.Memento.Syntax.Tag where

import Data.Kind (Type)

{- Literals -}
data KLiteral :: Type

{- Variables -}
data KVariable :: Type

{- Types -}
data KType :: Type

-- data KEffects :: Type

{- Definitions -}
data KDefinition :: Type

{- Expressions -}
data KLet :: Type
data KExpr :: Type
data KPattern :: Type
data KBinOp :: Type

-- data KHandlerClause :: Type

{- Programs -}
data KProgram :: Type
