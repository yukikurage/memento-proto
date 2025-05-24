{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker.Types (
  ConstructorSignature (..),
  AdtInfo (..),
  OperatorSignature (..),
  EffectInfo (..),
) where

import Data.Map (Map)
import Data.Text (Text)
import Language.Memento.Syntax (Effect (..), Effects, Type (..), TypeError (..))

-- | Signature of a data constructor
data ConstructorSignature = ConstructorSignature
  { csArgType :: Type -- Argument types,
  , csResultType :: Type -- Result ADT type
  }
  deriving (Show, Eq)

-- | Information about a declared Algebraic Data Type (ADT)
data AdtInfo = AdtInfo
  { adtName :: Text -- Name of the ADT
  , adtConstructors :: Map Text ConstructorSignature -- Constructors (name -> signature)
  }
  deriving (Show, Eq)

-- | Signature of an effect operator
data OperatorSignature = OperatorSignature
  { osArgType :: Type -- Argument type of the operator
  , osRetType :: Type -- Return type of the operator
  , osEffectName :: Text -- Name of the effect this operator belongs to
  }
  deriving (Show, Eq)

-- | Information about a declared Effect
data EffectInfo = EffectInfo
  { eiName :: Text -- Name of the Effect
  , eiOps :: Map Text OperatorSignature -- Operators (op_name -> signature)
  }
  deriving (Show, Eq)
