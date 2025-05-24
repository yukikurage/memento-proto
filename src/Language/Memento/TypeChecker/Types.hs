{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker.Types (
  ConstructorSignature (..),
  AdtInfo (..),
  OperatorSignature (..),
  EffectInfo (..),
  resolveType,
  unify,
  isSubEffects,
  extractConstructorType,
  extractHandlerType,
  buildConstructorType -- Added based on its usage in TypeChecker.hs with registerAdtsAndConstructors
) where

import Control.Monad (forM_, unless, when) -- ensure 'when' and 'unless' are imported
import Control.Monad.Except (throwError)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax (Effect (..), Effects, Type (..), TypeError (..))
import Language.Memento.TypeChecker.Monad -- Importing Monad for TypeCheck type and environment accessors

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

{- | Resolve a type to ensure it refers to known ADTs or primitive types.
currentAdtName is for allowing recursive definitions.
-}
resolveType :: Type -> Maybe Text -> Maybe Text -> TypeCheck ()
resolveType typ currentAdtName currentEffectName = do
  adtEnv <- getAdtEnv
  effectEnv <- getEffectEnv
  case typ of
    TNumber -> return ()
    TBool -> return ()
    THandler (argT, consumedEffects) (retT, generatedEffects) -> do
      forM_ consumedEffects $ \(Effect effect) -> do
        let isCurrent = Just effect == currentEffectName
        unless (isCurrent || Map.member effect effectEnv) $
          throwError $
            CustomErrorType $
              "Undefined effect referenced in function type: " <> T.pack (show effect)
      forM_ generatedEffects $ \(Effect effect) -> do
        let isCurrent = Just effect == currentEffectName
        unless (isCurrent || Map.member effect effectEnv) $
          throwError $
            CustomErrorType $
              "Undefined effect referenced in function type: " <> T.pack (show generatedEffects)
      resolveType argT currentAdtName currentEffectName
      resolveType retT currentAdtName currentEffectName
    TFunction argT (retT, generatedEffects) -> do
      forM_ generatedEffects $ \(Effect effect) -> do
        let isCurrent = Just effect == currentEffectName
        unless (isCurrent || Map.member effect effectEnv) $
          throwError $
            CustomErrorType $
              "Undefined effect referenced in function type: " <> T.pack (show generatedEffects)
      resolveType argT currentAdtName currentEffectName
      resolveType retT currentAdtName currentEffectName
    TAlgebraicData name -> do
      let isCurrent = Just name == currentAdtName
      unless (isCurrent || Map.member name adtEnv) $
        throwError $
          CustomErrorType $
            "Undefined algebraic data type: " <> name

-- | エフェクトのサブタイピングをチェック
isSubEffects :: Effects -> Effects -> Bool
isSubEffects e1 e2 = e1 `Set.isSubsetOf` e2

{- | Unify two types. Throws TypeMismatch if they are not equal.
   関数型の場合は引数と戻り値の型を再帰的に照合し、エフェクトについてはサブタイピングを適用する
   エフェクトのサブタイピング: S1 ⊂ S2 のとき T with S1 は T with S2 に代入可能
   (左辺) : 実際に宣言された型など、代入先の型
   (右辺) : 推論された型など
-}
unify :: Type -> Type -> TypeCheck ()
unify (THandler (argT1, eff1) (retT1, eff2)) (THandler (argT2, eff3) (retT2, eff4)) = do
  unify argT1 argT2
  unify retT1 retT2
  unless (eff4 `isSubEffects` eff2) $
    throwError $
      EffectMismatch eff4 eff2
  unless (eff1 `isSubEffects` eff3) $
    throwError $
      EffectMismatch eff1 eff3
unify (TFunction argT1 (retT1, eff1)) (TFunction argT2 (retT2, eff2)) = do
  unify argT1 argT2
  unify retT1 retT2
  unless (eff2 `isSubEffects` eff1) $
    throwError $
      EffectMismatch eff2 eff1 -- Corrected based on previous step's notes
unify expected actual =
  Control.Monad.when (expected /= actual) $ -- Explicitly using Control.Monad.when
    throwError $
      TypeMismatch expected actual

-- | Helper to build the functional type of a constructor
buildConstructorType :: Type -> Type -> Type
buildConstructorType argType resultType = TFunction argType (resultType, Set.empty)

{- | 逆に Type から引数の型の列と結果の型を取り出す
| 結果の型が分解不可能になるまで繰り返す
-}
extractConstructorType :: Type -> TypeCheck (Type, (Type, Effects))
extractConstructorType typ = case typ of
  TFunction argT (retT, retEffects) -> return (argT, (retT, retEffects))
  _ -> throwError $ CustomErrorType $ "Expected a function type but got: " <> T.pack (show typ)

extractHandlerType :: Type -> TypeCheck ((Type, Effects), (Type, Effects))
extractHandlerType typ = case typ of
  THandler (argT, argEffects) (retT, retEffects) -> return ((argT, argEffects), (retT, retEffects))
  _ -> throwError $ CustomErrorType $ "Expected a handler type but got: " <> T.pack (show typ)
