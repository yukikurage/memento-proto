-- May be needed for some existing logic
{-# LANGUAGE ScopedTypeVariables #-}

{-
トップレベル定義を公理として追加
-}
module Language.Memento.TypeChecker.Registration where

import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (modify) -- Added import for modify
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax (ArgumentWithMetadata (ArgumentWithMetadata), ConstructorDef (..), ConstructorDefMetadata (ConstructorDefMetadata), ConstructorDefWithMetadata (ConstructorDefWithMetadata), Definition (..), DefinitionMetadata (DefinitionMetadata), DefinitionWithMetadata (DefinitionWithMetadata), Effect (..), EffectsWithMetadata (EffectsWithMetadata), OperatorDef (..), OperatorDefMetadata (OperatorDefMetadata), OperatorDefWithMetadata (OperatorDefWithMetadata), Type (..), TypeArgumentWithMetadata (TypeArgumentWithMetadata), TypeVariable, TypeVariableWithMetadata, TypeWithMetadata (TypeWithMetadata), Variable, VariableWithMetadata (VariableWithMetadata))
import Language.Memento.TypeChecker.Monad
import Language.Memento.TypeChecker.Solver
import Language.Memento.TypeChecker.Types

{- | コンストラクタを公理として追加
| ついでにマッピングも追加する
-}
registerConstructor :: TypeVariable -> ConstructorDefWithMetadata -> ConstraintCollectorM ()
registerConstructor dataTy (ConstructorDefWithMetadata (ConstructorDef (ArgumentWithMetadata var _) typ) (ConstructorDefMetadata _ uniqueId)) = do
  let
    typeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId
    constraint = (typeVariable, serializeST (typeToSolvedType typ), COEqual)
  addCSTypeConstraints constraint -- COEqual の制約を追加; 導入した型変数は確実に指定した型を指す
  addTypePreference typeVariable Minimize -- COEqual があるので実際にはどちらでも良い
  addEnvSingleton var typeVariable -- 変数から型変数へのマッピングを追加
  -- コンストラクタの情報を追加
  case typ of
    TypeWithMetadata (TFunction argT (retT, EffectsWithMetadata effs _)) _
      | null effs -> addCSConstructor var (ConstructorInfo argT retT dataTy)
    _ -> throwError $ InvalidConstructorType typ
  pure ()

-- | オペレータを公理として追加
registerOperator :: TypeVariable -> OperatorDefWithMetadata -> ConstraintCollectorM ()
registerOperator effectTy (OperatorDefWithMetadata (OperatorDef (ArgumentWithMetadata var _) typ) (OperatorDefMetadata _ uniqueId)) = do
  let
    typeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId
    constraint = (typeVariable, serializeST (typeToSolvedType typ), COEqual)
  addCSTypeConstraints constraint -- COEqual の制約を追加; 導入した型変数は確実に指定した型を指す
  addTypePreference typeVariable Minimize -- COEqual があるので実際にはどちらでも良い
  addEnvSingleton var typeVariable -- 変数から型変数へのマッピングを追加
  -- オペレータの情報を追加
  case typ of
    TypeWithMetadata (TFunction argT (retT, EffectsWithMetadata effs _)) _
      | null effs -> addCSOperator var (OperatorInfo argT retT effectTy)
    _ -> throwError $ InvalidOperatorType typ
  pure ()

-- | 定義を公理として追加
registerDefinition :: DefinitionWithMetadata -> ConstraintCollectorM ()
registerDefinition (DefinitionWithMetadata def (DefinitionMetadata _ uniqueId)) = case def of
  DataDef (TypeArgumentWithMetadata dataTy _) constructorDefs -> mapM_ (registerConstructor dataTy) constructorDefs
  EffectDef (TypeArgumentWithMetadata effectTy _) operatorDefs -> mapM_ (registerOperator effectTy) operatorDefs
  ValDef (ArgumentWithMetadata var _) typ _ -> do
    let
      typeVariable = UnsolvedTypeVariable $ T.pack $ show uniqueId
      constraint = (typeVariable, serializeST (typeToSolvedType typ), COEqual)
    addCSTypeConstraints constraint
    addTypePreference typeVariable Minimize
    addEnvSingleton var typeVariable
    pure ()
