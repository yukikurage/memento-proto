{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.DataTypeAnalysis
  ( DataTypeInfo (..),
    ConstructorInfo (..),
    TypeParameterInfo (..),
    DataTypeAnalysis (..),
    extractDataTypes,
    analyzeDataTypes,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.Syntax
import Language.Memento.Syntax.Definition (ConstructorDef (..), Definition (..))
import qualified Language.Memento.Syntax.Definition as SDefinition
import qualified Language.Memento.Syntax.MType as SMType
import qualified Language.Memento.Syntax.Program as SProgram
import Language.Memento.Syntax.Tag (KDefinition, KProgram, KType, KTypeVariable)
import qualified Language.Memento.Syntax.Variable as SVariable
import Language.Memento.TypeSolver.Types

-- | Information about a single constructor in a data type
data ConstructorInfo = ConstructorInfo
  { ciName :: T.Text,                    -- ^ Constructor name
    ciTypeParams :: [T.Text],            -- ^ Type parameter names used in this constructor
    ciArgTypes :: [AST KType],           -- ^ Argument types (raw AST)
    ciReturnType :: AST KType            -- ^ Return type (raw AST)
  }
  deriving (Show, Eq)

-- | Type parameter information
data TypeParameterInfo = TypeParameterInfo
  { tpiName :: T.Text,           -- ^ Parameter name
    tpiIsGeneric :: Bool         -- ^ True if generic (T, U), False if concrete (number, string)
  }
  deriving (Show, Eq)

-- | Information about a complete data type
data DataTypeInfo = DataTypeInfo
  { dtiName :: T.Text,                   -- ^ Data type name
    dtiTypeParams :: [T.Text],           -- ^ Type parameter names (extracted from first constructor)
    dtiTypeParamInfo :: [TypeParameterInfo], -- ^ Detailed type parameter information
    dtiConstructors :: [ConstructorInfo], -- ^ All constructors for this type
    dtiKind :: Int                       -- ^ Number of type parameters
  }
  deriving (Show, Eq)

-- | Complete analysis of all data types in a program
data DataTypeAnalysis = DataTypeAnalysis
  { dtaDataTypes :: Map.Map T.Text DataTypeInfo,        -- ^ Map from type name to info
    dtaConstructors :: Map.Map T.Text T.Text,           -- ^ Map from constructor name to type name
    dtaTypeParams :: Map.Map T.Text [T.Text]            -- ^ Map from type name to parameter names
  }
  deriving (Show, Eq)

-- | Extract all data type definitions from an AST program
extractDataTypes :: AST KProgram -> [DataTypeInfo]
extractDataTypes ast = case unProgram (extractSyntax ast) of
  SProgram.Program declAsts -> 
    [extractDataType declAst | declAst <- declAsts, isDataDecl declAst]
  where
    isDataDecl :: AST KDefinition -> Bool
    isDataDecl declAst = case unDefinition (extractSyntax declAst) of
      SDefinition.DataDef _ _ -> True
      _ -> False

-- | Extract information from a single data type declaration
extractDataType :: AST KDefinition -> DataTypeInfo
extractDataType declAst = case unDefinition (extractSyntax declAst) of
  SDefinition.DataDef dataNameAst constructorDefs ->
    let SVariable.Var dataName = unVariable (extractSyntax dataNameAst)
        constructors = map (extractConstructorInfo dataName) constructorDefs
        -- Extract type parameters from constructor return types (handles existential types correctly)
        (typeParams, typeParamInfo) = case constructors of
          [] -> ([], [])
          (c:_) -> extractDataTypeParamsWithInfo dataName (ciReturnType c)
    in DataTypeInfo
      { dtiName = dataName
      , dtiTypeParams = typeParams
      , dtiTypeParamInfo = typeParamInfo
      , dtiConstructors = constructors
      , dtiKind = length typeParams
      }
  _ -> error "extractDataType called on non-data declaration"

-- | Extract information from a single constructor
extractConstructorInfo :: T.Text -> SDefinition.ConstructorDef AST -> ConstructorInfo
extractConstructorInfo dataName (SDefinition.ConstructorDef ctorNameAst typeParams fullTypeAst) =
  let SVariable.Var ctorName = unVariable (extractSyntax ctorNameAst)
      paramNames = [case unTypeVariable (extractSyntax paramAst) of
                     SVariable.TypeVar paramName -> paramName
                   | paramAst <- typeParams]
      (argTypes, retType) = extractFunctionSignature fullTypeAst
  in ConstructorInfo
    { ciName = ctorName
    , ciTypeParams = paramNames
    , ciArgTypes = argTypes
    , ciReturnType = retType
    }

-- | Extract argument types and return type from a function type AST
extractFunctionSignature :: AST KType -> ([AST KType], AST KType)
extractFunctionSignature typeAst = case unMType (extractSyntax typeAst) of
  SMType.TFunction params retAst -> (map snd params, retAst)  -- Extract types from (param, type) pairs
  _ -> ([], typeAst)  -- Not a function, treat as zero-argument constructor

-- | Extract type parameters from a data type's return type (e.g., Consume<T> -> ["T"])
extractDataTypeParams :: T.Text -> AST KType -> [T.Text]
extractDataTypeParams dataName typeAst = fst $ extractDataTypeParamsWithInfo dataName typeAst

-- | Extract type parameters with detailed information (generic vs concrete)
extractDataTypeParamsWithInfo :: T.Text -> AST KType -> ([T.Text], [TypeParameterInfo])
extractDataTypeParamsWithInfo dataName typeAst = case unMType (extractSyntax typeAst) of
  SMType.TVar varAst -> 
    let SVariable.TypeVar varName = unTypeVariable (extractSyntax varAst)
    in if varName == dataName
       then ([], [])  -- No type parameters (e.g., List -> [])
       else ([], [])  -- Not the expected data type
  SMType.TApplication baseAst argAsts ->
    let SVariable.TypeVar baseName = unTypeVariable (extractSyntax baseAst)
    in if baseName == dataName
       then 
         let paramInfo = map extractTypeParameterInfo argAsts
             paramNames = map tpiName paramInfo
         in (paramNames, paramInfo)
       else ([], [])  -- Not the expected data type
  _ -> ([], [])  -- Other cases, no parameters

-- | Extract detailed type parameter information
extractTypeParameterInfo :: AST KType -> TypeParameterInfo
extractTypeParameterInfo typeAst = case unMType (extractSyntax typeAst) of
  SMType.TVar varAst -> 
    let SVariable.TypeVar varName = unTypeVariable (extractSyntax varAst)
    in TypeParameterInfo varName True  -- Generic type variable
  SMType.TNumber -> TypeParameterInfo "number" False    -- Concrete type parameter
  SMType.TInt -> TypeParameterInfo "int" False          -- Concrete type parameter  
  SMType.TBool -> TypeParameterInfo "bool" False        -- Concrete type parameter
  SMType.TString -> TypeParameterInfo "string" False    -- Concrete type parameter
  SMType.TNever -> TypeParameterInfo "never" False      -- Concrete type parameter
  SMType.TUnknown -> TypeParameterInfo "unknown" False  -- Concrete type parameter
  _ -> TypeParameterInfo "unknown" False  -- Fallback for complex types

-- | Extract type parameter name from type AST, handling both generic and concrete types
extractTypeVarName :: AST KType -> T.Text
extractTypeVarName typeAst = tpiName $ extractTypeParameterInfo typeAst

-- | Perform complete data type analysis on a program
analyzeDataTypes :: AST KProgram -> DataTypeAnalysis
analyzeDataTypes ast =
  let dataTypes = extractDataTypes ast
      dataTypeMap = Map.fromList [(dtiName dt, dt) | dt <- dataTypes]
      constructorMap = Map.fromList 
        [(ciName ctor, dtiName dt) | dt <- dataTypes, ctor <- dtiConstructors dt]
      typeParamMap = Map.fromList 
        [(dtiName dt, dtiTypeParams dt) | dt <- dataTypes]
  in DataTypeAnalysis
    { dtaDataTypes = dataTypeMap
    , dtaConstructors = constructorMap
    , dtaTypeParams = typeParamMap
    }