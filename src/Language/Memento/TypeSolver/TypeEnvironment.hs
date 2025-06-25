{-# LANGUAGE OverloadedStrings #-}

-- | Immutable type environment management for the Memento type solver
-- Handles scoping, polymorphic types, and environment composition
module Language.Memento.TypeSolver.TypeEnvironment
  ( TypeEnvironment (..),
    Scope (..),
    DefinitionInfo (..),
    emptyTypeEnv,
    lookupType,
    extendEnv,
    extendEnvScheme,
    withScope,
    mergeScopes,
    extractDefinitions,
    buildProgramEnvironment,
    instantiateFromEnv,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.Syntax
import qualified Language.Memento.Syntax.Definition as SDefinition
import qualified Language.Memento.Syntax.Literal as SLiteral
import qualified Language.Memento.Syntax.MType as SMType
import qualified Language.Memento.Syntax.Program as SProgram
import Language.Memento.Syntax.Tag (KDefinition, KExpr, KLiteral, KProgram, KType, KTypeVariable, KVariable)
import qualified Language.Memento.Syntax.Variable as SVariable
import Language.Memento.TypeSolver.Types

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Immutable type environment supporting scoped lookup
data TypeEnvironment = TypeEnvironment
  { teGlobal :: Map.Map T.Text TypeScheme,            -- ^ Global bindings (definitions)
    teLocal :: [Scope],                               -- ^ Local scopes stack (innermost first)
    teTypeConstructors :: Map.Map T.Text TypeConstructorInfo, -- ^ Type constructor info
    teConstructors :: Set.Set T.Text,                 -- ^ Available constructor names
    teGenericContext :: Map.Map T.Text T.Text         -- ^ Current generic type mappings
  }
  deriving (Show, Eq)

-- | A single scope for local bindings (let, lambda, pattern matching)
data Scope = Scope
  { scopeBindings :: Map.Map T.Text TypeScheme,       -- ^ Local variable bindings
    scopeGenericContext :: Map.Map T.Text T.Text     -- ^ Generic types introduced in this scope
  }
  deriving (Show, Eq)

-- | Information about a top-level definition
data DefinitionInfo = DefinitionInfo
  { diName :: T.Text,                                 -- ^ Definition name
    diTypeParams :: [T.Text],                         -- ^ Type parameter names
    diDeclaredType :: AST KType,                      -- ^ Declared type (AST)
    diBodyAST :: Maybe (AST KExpr)                    -- ^ Optional body for value definitions
  }
  deriving (Show, Eq)

-- ============================================================================
-- Environment Construction
-- ============================================================================

-- | Create an empty type environment
emptyTypeEnv :: TypeEnvironment
emptyTypeEnv = TypeEnvironment
  { teGlobal = Map.empty
  , teLocal = []
  , teTypeConstructors = Map.empty
  , teConstructors = Set.empty
  , teGenericContext = Map.empty
  }

-- | Extend environment with a simple type binding
extendEnv :: T.Text -> Type -> TypeEnvironment -> TypeEnvironment
extendEnv name typ env = extendEnvScheme name (TypeScheme [] typ) env

-- | Extend environment with a type scheme
extendEnvScheme :: T.Text -> TypeScheme -> TypeEnvironment -> TypeEnvironment
extendEnvScheme name scheme env = case teLocal env of
  [] -> env { teGlobal = Map.insert name scheme (teGlobal env) }
  (scope:rest) -> 
    let updatedScope = scope { scopeBindings = Map.insert name scheme (scopeBindings scope) }
    in env { teLocal = updatedScope : rest }

-- | Create a new scope and execute computation within it
withScope :: TypeEnvironment -> (TypeEnvironment -> (a, TypeEnvironment)) -> (a, TypeEnvironment)
withScope env computation = 
  let envWithScope = env { teLocal = emptyScope : teLocal env }
      (result, envAfterComputation) = computation envWithScope
      envRestored = envAfterComputation { teLocal = teLocal env }  -- Remove the scope
  in (result, envRestored)
  where
    emptyScope = Scope Map.empty Map.empty

-- | Merge multiple scopes (used for parallel scope resolution)
mergeScopes :: [Scope] -> Scope
mergeScopes scopes = Scope
  { scopeBindings = Map.unions (map scopeBindings scopes)
  , scopeGenericContext = Map.unions (map scopeGenericContext scopes)
  }

-- ============================================================================
-- Environment Lookup
-- ============================================================================

-- | Look up a type scheme in the environment (searches local scopes first, then global)
lookupType :: T.Text -> TypeEnvironment -> Maybe TypeScheme
lookupType name env = 
  case findInLocalScopes name (teLocal env) of
    Just scheme -> Just scheme
    Nothing -> Map.lookup name (teGlobal env)
  where
    findInLocalScopes :: T.Text -> [Scope] -> Maybe TypeScheme
    findInLocalScopes _ [] = Nothing
    findInLocalScopes n (scope:rest) = 
      case Map.lookup n (scopeBindings scope) of
        Just scheme -> Just scheme
        Nothing -> findInLocalScopes n rest

-- | Instantiate a polymorphic type from the environment with fresh variables
instantiateFromEnv :: TypeEnvironment -> T.Text -> Int -> Maybe (Type, Int)
instantiateFromEnv env name varCounter = do
  scheme <- lookupType name env
  return $ instantiateScheme scheme varCounter
  where
    instantiateScheme :: TypeScheme -> Int -> (Type, Int)
    instantiateScheme (TypeScheme [] typ) counter = (typ, counter)
    instantiateScheme (TypeScheme vars typ) counter = 
      let (freshVars, newCounter) = generateFreshVars counter vars
          substitution = Map.fromList (zip vars (map TVar freshVars))
          instantiatedType = pureSubstGenerics substitution typ
      in (instantiatedType, newCounter)
    
    generateFreshVars :: Int -> [T.Text] -> ([TypeVar], Int)
    generateFreshVars counter [] = ([], counter)
    generateFreshVars counter (_:rest) = 
      let freshVar = TypeVar $ T.pack ("t" ++ show counter)
          (restVars, finalCounter) = generateFreshVars (counter + 1) rest
      in (freshVar : restVars, finalCounter)

-- ============================================================================
-- Program-Level Environment Building
-- ============================================================================

-- | Extract all top-level definitions from a program
extractDefinitions :: AST KProgram -> [DefinitionInfo]
extractDefinitions ast = case unProgram (extractSyntax ast) of
  SProgram.Program declAsts -> map extractDefinitionInfo declAsts
  where
    extractDefinitionInfo :: AST KDefinition -> DefinitionInfo
    extractDefinitionInfo declAst = case unDefinition (extractSyntax declAst) of
      SDefinition.ValDef varAst typeParams typeAst exprAst ->
        let SVariable.Var name = unVariable (extractSyntax varAst)
            paramNames = extractTypeParamNames typeParams
        in DefinitionInfo
          { diName = name
          , diTypeParams = paramNames
          , diDeclaredType = typeAst
          , diBodyAST = Just exprAst
          }
      
      SDefinition.DataDef dataNameAst constructorDefs ->
        let SVariable.Var dataName = unVariable (extractSyntax dataNameAst)
            -- For data definitions, we'll handle them separately
            -- since they don't fit the simple DefinitionInfo pattern
        in DefinitionInfo
          { diName = dataName
          , diTypeParams = []  -- Data types are handled by DataTypeAnalysis
          , diDeclaredType = undefined  -- Will be skipped in conversion
          , diBodyAST = Nothing
          }
      
      SDefinition.TypeDef aliasAst _params typeAst ->
        let SVariable.Var aliasName = unVariable (extractSyntax aliasAst)
        in DefinitionInfo
          { diName = aliasName
          , diTypeParams = []  -- TODO: Extract type alias parameters
          , diDeclaredType = typeAst
          , diBodyAST = Nothing
          }

-- | Build type environment from program definitions
buildProgramEnvironment :: AST KProgram -> TypeEnvironment -> Either TypeError TypeEnvironment
buildProgramEnvironment ast baseEnv = do
  let definitions = extractDefinitions ast
  -- Phase 1: Add all definition types to environment
  envWithTypes <- foldl addDefinitionType (Right baseEnv) definitions
  return envWithTypes
  where
    addDefinitionType :: Either TypeError TypeEnvironment -> DefinitionInfo -> Either TypeError TypeEnvironment
    addDefinitionType (Left err) _ = Left err
    addDefinitionType (Right env) defInfo = 
      case convertDefinitionToScheme env defInfo of
        Right scheme -> Right $ extendEnvScheme (diName defInfo) scheme env
        Left err -> Left err

-- | Convert a definition to a type scheme
convertDefinitionToScheme :: TypeEnvironment -> DefinitionInfo -> Either TypeError TypeScheme
convertDefinitionToScheme env defInfo = 
  -- Skip data definitions (they should be handled by DataTypeAnalysis)
  if null (diTypeParams defInfo) && diBodyAST defInfo == Nothing
  then Right $ TypeScheme [] TUnknown  -- Placeholder for data/type definitions
  else do
    -- Handle generic context for polymorphic definitions
    let typeParams = diTypeParams defInfo
        envWithGenerics = addGenericContext env typeParams
    
    -- Convert declared type
    declaredType <- convertASTTypeInEnv envWithGenerics (diDeclaredType defInfo)
    
    -- Create type scheme with fresh generic variables
    let freshGenerics = map (\param -> "_" <> param <> "_fresh") typeParams
    return $ TypeScheme freshGenerics declaredType

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Extract type parameter names from AST
extractTypeParamNames :: [AST KTypeVariable] -> [T.Text]
extractTypeParamNames typeParams = 
  [ case unTypeVariable (extractSyntax paramAst) of
      SVariable.TypeVar paramName -> paramName
  | paramAst <- typeParams
  ]

-- | Add generic type context to environment
addGenericContext :: TypeEnvironment -> [T.Text] -> TypeEnvironment
addGenericContext env typeParams = 
  let genericMappings = Map.fromList [(param, "_" <> param <> "_fresh") | param <- typeParams]
      newGenericContext = Map.union genericMappings (teGenericContext env)
  in env { teGenericContext = newGenericContext }

-- | Convert AST type to solver type within an environment context
convertASTTypeInEnv :: TypeEnvironment -> AST KType -> Either TypeError Type
convertASTTypeInEnv env ast = case unMType (extractSyntax ast) of
  SMType.TNumber -> Right TNumber
  SMType.TInt -> Right TNumber
  SMType.TBool -> Right TBool
  SMType.TString -> Right TString
  SMType.TNever -> Right TNever
  SMType.TUnknown -> Right TUnknown
  
  SMType.TVar varAst -> do
    let SVariable.TypeVar name = unTypeVariable (extractSyntax varAst)
        meta = extractMetadata ast
    case name of
      "string" -> Right TString
      "number" -> Right TNumber
      "bool" -> Right TBool
      "never" -> Right TNever
      "unknown" -> Right TUnknown
      _ -> case Map.lookup name (teTypeConstructors env) of
        Just (TypeConstructorInfo k _ _) | k == 0 -> Right $ TApplication name []
        Just _ -> Left $ UnboundTypeVariable (Just meta) name  -- Missing type arguments
        Nothing -> case Map.lookup name (teGenericContext env) of
          Just g -> Right $ TGeneric g
          Nothing -> Left $ UnboundTypeVariable (Just meta) name
  
  SMType.TFunction params retType -> do
    paramTypes <- mapM (convertASTTypeInEnv env . snd) params
    returnType <- convertASTTypeInEnv env retType
    return $ TFunction paramTypes returnType
  
  SMType.TUnion types -> do
    convertedTypes <- mapM (convertASTTypeInEnv env) types
    return $ mkUnion convertedTypes
  
  SMType.TIntersection types -> do
    convertedTypes <- mapM (convertASTTypeInEnv env) types
    return $ mkIntersection convertedTypes
  
  SMType.TApplication baseAst argAsts -> do
    let SVariable.TypeVar baseName = unTypeVariable (extractSyntax baseAst)
        meta = extractMetadata ast
    case Map.lookup baseName (teTypeConstructors env) of
      Just (TypeConstructorInfo k _ _) -> do
        if k /= length argAsts
          then Left $ SomeTypeError $ 
            "Type constructor " <> baseName <> " expects " <> T.pack (show k) <> 
            " arguments, got " <> T.pack (show (length argAsts))
          else do
            argTypes <- mapM (convertASTTypeInEnv env) argAsts
            return $ TApplication baseName argTypes
      Nothing -> Left $ UnboundTypeVariable (Just meta) baseName
  
  SMType.TLiteral litAst -> Right $ convertLiteralASTToType litAst
  where
    convertLiteralASTToType :: AST KLiteral -> Type
    convertLiteralASTToType litAst = case unLiteral (extractSyntax litAst) of
      SLiteral.NumberLiteral n -> TLiteral (LNumber n)
      SLiteral.IntLiteral i -> TLiteral (LNumber (fromIntegral i))
      SLiteral.BoolLiteral b -> TLiteral (LBool b)
      SLiteral.StringLiteral s -> TLiteral (LString s)

-- | Pure generic substitution (local version for TypeEnvironment)
pureSubstGenerics :: Map.Map T.Text Type -> Type -> Type
pureSubstGenerics subst typ = case typ of
  TVar v -> typ
  TGeneric g -> Map.findWithDefault (TGeneric g) g subst
  TNumber -> typ
  TBool -> typ
  TString -> typ
  TNever -> typ
  TUnknown -> typ
  TLiteral l -> typ
  TFunction argTypes retType -> 
    TFunction (map (pureSubstGenerics subst) argTypes) (pureSubstGenerics subst retType)
  TUnion types -> TUnion (Set.map (pureSubstGenerics subst) types)
  TIntersection types -> TIntersection (Set.map (pureSubstGenerics subst) types)
  TApplication name argTypes -> 
    TApplication name (map (pureSubstGenerics subst) argTypes)

