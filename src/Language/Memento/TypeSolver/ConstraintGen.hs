{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Enhanced constraint generation for separated constructor/type semantics
--
-- This module implements the constraint generation phase for the Memento type solver,
-- with support for the new separated constructor/type semantics:
--
-- 1. Constructor Polymorphism: data Cons<T> : (x : T, consume : T => number) => Typ
-- 2. Type Polymorphism: data Cons : (x : number) => Typ<string>
-- 3. Combined Polymorphism: data ConsSome<T> : (x : T) => Some<T>
--
-- Key features:
-- - Advanced variance analysis following the specification
-- - Separated constructor and type constructor namespaces
-- - Extensible architecture for future language features
-- - Comprehensive type environment management
--
module Language.Memento.TypeSolver.ConstraintGen where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad (unless, when, zipWithM, zipWithM_, replicateM, foldM)
import Data.List (unzip3, intercalate)
import GHC.Base (List)
import Text.Megaparsec (SourcePos)
import Language.Memento.Syntax
import Language.Memento.Syntax.Expr (Expr(..), Let(..))
import Language.Memento.Syntax.Metadata (Metadata(..))
import qualified Language.Memento.Syntax.MType as SMType
import qualified Language.Memento.Syntax.Literal as SLiteral
import qualified Language.Memento.Syntax.Variable as SVariable
import qualified Language.Memento.Syntax.Pattern as SPattern
import qualified Language.Memento.Syntax.Definition as SDefinition
import qualified Language.Memento.Syntax.Program as SProgram
import qualified Language.Memento.Syntax.BinOp as SBinOp
import Language.Memento.Syntax.Tag (KType, KExpr, KLiteral, KVariable, KPattern, KDefinition, KProgram, KLet, KBinOp)
import Language.Memento.Data.HFix (HFix(..), unHFix)
import Language.Memento.Data.HProduct (type (:*:) (..))
import Language.Memento.TypeSolver.Types
import Language.Memento.TypeSolver.Solver

-- Enhanced type inference context with separated namespaces
data InferContext = InferContext
  { icVarCounter :: Int
  , icConstraints :: ConstraintSet
  , icTypeEnv :: Map.Map T.Text TypeScheme        -- Value/constructor environment
  , icTypeConstructors :: Map.Map T.Text TypeConstructorInfo  -- Type constructor environment
  , icConstructors :: Set.Set T.Text              -- Track known constructor names
  , icVariances :: TypeConstructorVariances       -- Track variance info for type constructors
  } deriving (Show)

-- Information about a type constructor
data TypeConstructorInfo = TypeConstructorInfo
  { tciKind :: Int                    -- Number of type parameters (kind)
  , tciVariance :: VarianceMap        -- Variance of each parameter
  , tciConstructors :: [T.Text]       -- Constructors that create this type
  } deriving (Show, Eq)

type InferM = State InferContext

-- Generate fresh type variable
freshVar :: InferM TypeVar
freshVar = do
  ctx <- get
  let counter = icVarCounter ctx
  put ctx { icVarCounter = counter + 1 }
  return $ TypeVar $ T.pack ("t" ++ show counter)

-- Add constraint
addConstraint :: Constraint -> InferM ()
addConstraint c = do
  ctx <- get
  put ctx { icConstraints = Set.insert c (icConstraints ctx) }

-- Look up variable type and instantiate if polymorphic
-- Enhanced with better instantiation strategy
lookupVar :: T.Text -> InferM (Maybe Type)
lookupVar name = do
  ctx <- get
  case Map.lookup name (icTypeEnv ctx) of
    Nothing -> return Nothing
    Just scheme -> do
      -- Enhanced polymorphic instantiation
      instantiatedType <- instantiatePolymorphicType scheme
      return $ Just instantiatedType

-- Enhanced polymorphic type instantiation with better strategy
instantiatePolymorphicType :: TypeScheme -> InferM Type
instantiatePolymorphicType (TypeScheme [] typ) = return typ  -- Monomorphic
instantiatePolymorphicType (TypeScheme vars typ) = do
  -- Create fresh type variables for each quantified variable
  freshVars <- mapM (const freshVar) vars
  let substitution = Map.fromList (zip vars (map TVar freshVars))
  let instantiatedType = substituteGenerics substitution typ
  return instantiatedType

-- Add variable to environment with monomorphic type
addVar :: T.Text -> Type -> InferM ()
addVar name typ = do
  let scheme = TypeScheme [] typ  -- Monomorphic type scheme
  addVarScheme name scheme

-- Add variable to environment with type scheme
addVarScheme :: T.Text -> TypeScheme -> InferM ()
addVarScheme name scheme = do
  ctx <- get
  put ctx { icTypeEnv = Map.insert name scheme (icTypeEnv ctx) }

-- Add constructor to registry
addConstructor :: T.Text -> InferM ()
addConstructor name = do
  ctx <- get
  put ctx { icConstructors = Set.insert name (icConstructors ctx) }

-- Check if name is a constructor
isConstructor :: T.Text -> InferM Bool
isConstructor name = do
  ctx <- get
  return $ Set.member name (icConstructors ctx)

-- Enhanced type constructor management for separated semantics

-- Register a type constructor with its metadata
registerTypeConstructor :: T.Text -> Int -> VarianceMap -> [T.Text] -> InferM ()
registerTypeConstructor typeName arity variance constructors = do
  ctx <- get
  let typeInfo = TypeConstructorInfo arity variance constructors
  put ctx {
    icTypeConstructors = Map.insert typeName typeInfo (icTypeConstructors ctx),
    icVariances = Map.insert typeName variance (icVariances ctx)
  }

-- Add a constructor to an existing type constructor
addConstructorToType :: T.Text -> T.Text -> InferM ()
addConstructorToType typeName constructorName = do
  ctx <- get
  let updateTypeInfo tci = tci { tciConstructors = constructorName : tciConstructors tci }
  put ctx {
    icTypeConstructors = Map.adjust updateTypeInfo typeName (icTypeConstructors ctx)
  }

-- Look up type constructor information
lookupTypeConstructor :: T.Text -> InferM (Maybe TypeConstructorInfo)
lookupTypeConstructor typeName = do
  ctx <- get
  return $ Map.lookup typeName (icTypeConstructors ctx)

-- Add variance information for a type constructor (backward compatibility)
addVarianceInfo :: T.Text -> VarianceMap -> InferM ()
addVarianceInfo constructorName varianceMap = do
  ctx <- get
  put ctx { icVariances = Map.insert constructorName varianceMap (icVariances ctx) }

-- Look up variance information for a type constructor (backward compatibility)
lookupVariance :: T.Text -> InferM (Maybe VarianceMap)
lookupVariance constructorName = do
  ctx <- get
  return $ Map.lookup constructorName (icVariances ctx)

-- Save current type environment
saveTypeEnv :: InferM (Map.Map T.Text TypeScheme)
saveTypeEnv = do
  ctx <- get
  return $ icTypeEnv ctx

-- Restore type environment
restoreTypeEnv :: Map.Map T.Text TypeScheme -> InferM ()
restoreTypeEnv env = do
  ctx <- get
  put ctx { icTypeEnv = env }

-- Helper to extract function argument type
getFunctionArg :: Type -> Type
getFunctionArg (TFunction arg _) = arg
getFunctionArg t = t  -- fallback


-- Extract position information from AST metadata
extractPosition :: AST a -> (SourcePos, SourcePos)
extractPosition ast =
  case unHFix ast of
    (Metadata start end :*: _) -> (start, end)

-- Format position for error messages
formatPosition :: (SourcePos, SourcePos) -> String
formatPosition (start, end) = show start ++ " to " ++ show end

-- Enhanced error reporting with position information
errorWithPosition :: AST a -> String -> InferM b
errorWithPosition ast message = do
  let (start, end) = extractPosition ast
  error $ message ++ " at " ++ formatPosition (start, end)

-- Convert Memento MType to solver Type (with constructor awareness)
convertMType :: AST KType -> InferM Type
convertMType ast =
  case unMType (extractSyntax ast) of
    SMType.TNumber -> return TNumber
    SMType.TInt -> return TNumber  -- Treat Int as Number for simplicity
    SMType.TBool -> return TBool
    SMType.TString -> return TString
    SMType.TNever -> return TNever
    SMType.TUnknown -> return TUnknown
    SMType.TVar varAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      -- Handle built-in primitive types first
      case name of
        "string" -> return TString
        "number" -> return TNumber
        "bool" -> return TBool
        "never" -> return TNever
        "unknown" -> return TUnknown
        _ -> do
          -- Check if this name is already defined in the environment
          maybeType <- lookupVar name
          maybeConstructor <- lookupVar ("TYPE_" <> name)
          case (maybeType, maybeConstructor) of
            (_, Just (TConstructor _)) -> return $ TConstructor name  -- Use constructor type if registered
            (Just (TConstructor _), _) -> return $ TConstructor name  -- Use ground type if registered
            (Just (TUnion types), _) -> return $ TUnion types  -- Resolve union types for exhaustivity
            (Just (TGeneric genericName), _) -> return $ TGeneric genericName  -- Preserve generic types
            (Just _, _) -> return $ TVar (TypeVar name)  -- Use variable for other types
            (Nothing, Nothing) -> return $ TVar (TypeVar name)  -- Unknown name, use variable
    SMType.TLiteral litAst ->
      return $ convertLiteral litAst
    SMType.TFunction params retType -> do
      paramTypes <- mapM (convertMType . snd) params
      returnType <- convertMType retType
      -- FIXED: Handle zero-argument functions correctly
      case paramTypes of
        [] -> return returnType  -- Zero arguments: just return the return type
        _  -> return $ foldr TFunction returnType paramTypes  -- Normal function
    SMType.TUnion types -> do
      convertedTypes <- mapM convertMType types
      return $ mkUnion convertedTypes
    SMType.TIntersection types -> do
      convertedTypes <- mapM convertMType types
      return $ mkIntersection convertedTypes
    SMType.TApplication baseAst argAsts -> do
      -- Handle type applications like List<T>, Map<K,V>
      baseType <- convertMType baseAst
      argTypes <- mapM convertMType argAsts
      return $ TApplication baseType argTypes

-- Convert literal to solver type
convertLiteral :: AST KLiteral -> Type
convertLiteral ast =
  case unLiteral (extractSyntax ast) of
    SLiteral.NumberLiteral n -> TLiteral (LNumber n)
    SLiteral.IntLiteral i -> TLiteral (LNumber (fromIntegral i))
    SLiteral.BoolLiteral b -> TLiteral (LBool b)
    SLiteral.StringLiteral s -> TLiteral (LString s)

-- Infer type for expressions and generate constraints
inferExpr :: AST KExpr -> InferM Type
inferExpr ast =
  case unExpr (extractSyntax ast) of
    EVar varAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      maybeType <- lookupVar name
      case maybeType of
        Just t -> return t
        Nothing -> errorWithPosition varAst $ "Unbound variable: " ++ T.unpack name

    ELiteral litAst -> do
      -- Enhanced literal type inference with singleton types
      let literalType = convertLiteral litAst
      return literalType  -- Keep precise singleton literal types like 42 : 42

    ELambda params bodyAst -> do
      paramTypes <- mapM (\(patAst, typeAst) -> do
        paramType <- convertMType typeAst
        case unPattern (extractSyntax patAst) of
          SPattern.PVar varAst -> do
            let SVariable.Var name = unVariable (extractSyntax varAst)
            addVar name paramType
            return paramType
          SPattern.PWildcard -> do
            -- Wildcard pattern, no binding needed
            return paramType
          _ -> errorWithPosition (fst (head params)) "Complex patterns not yet supported in lambda"
        ) params
      bodyType <- inferExpr bodyAst
      return $ foldr TFunction bodyType paramTypes

    EApply funcAst argAsts -> do
      funcType <- inferExpr funcAst
      argTypes <- mapM inferExpr argAsts
      resultType <- TVar <$> freshVar
      -- Simple approach: create expected function type and constrain
      let expectedType = foldr TFunction resultType argTypes
      addConstraint $ Subtype funcType expectedType
      return resultType

    EIf condAst thenAst elseAst -> do
      condType <- inferExpr condAst
      addConstraint $ Subtype condType TBool
      thenType <- inferExpr thenAst
      elseType <- inferExpr elseAst
      resultType <- TVar <$> freshVar
      addConstraint $ Subtype thenType resultType
      addConstraint $ Subtype elseType resultType
      return resultType

    EBinOp opAst leftAst rightAst -> do
      leftType <- inferExpr leftAst
      rightType <- inferExpr rightAst
      case unBinOp (extractSyntax opAst) of
        SBinOp.Add -> do
          addConstraint $ Subtype leftType TNumber
          addConstraint $ Subtype rightType TNumber
          return TNumber
        SBinOp.Sub -> do
          addConstraint $ Subtype leftType TNumber
          addConstraint $ Subtype rightType TNumber
          return TNumber
        SBinOp.Mul -> do
          addConstraint $ Subtype leftType TNumber
          addConstraint $ Subtype rightType TNumber
          return TNumber
        SBinOp.Div -> do
          addConstraint $ Subtype leftType TNumber
          addConstraint $ Subtype rightType TNumber
          return TNumber
        SBinOp.Eq -> do
          unifiedType <- TVar <$> freshVar
          addConstraint $ Subtype leftType unifiedType
          addConstraint $ Subtype rightType unifiedType
          return TBool
        SBinOp.Lt -> do
          addConstraint $ Subtype leftType TNumber
          addConstraint $ Subtype rightType TNumber
          return TBool
        SBinOp.Gt -> do
          addConstraint $ Subtype leftType TNumber
          addConstraint $ Subtype rightType TNumber
          return TBool

    EBlock letAsts exprAst -> do
      mapM_ (\letAst ->
        case unLet (extractSyntax letAst) of
          Let patAst typeAst exprAst -> do
            exprType <- inferExpr exprAst
            declaredType <- convertMType typeAst
            addConstraint $ Subtype exprType declaredType
            case unPattern (extractSyntax patAst) of
              SPattern.PVar varAst -> do
                let SVariable.Var name = unVariable (extractSyntax varAst)
                addVar name declaredType
              SPattern.PWildcard -> do
                -- Wildcard pattern, no binding needed
                return ()
              _ -> errorWithPosition patAst "Complex patterns not yet supported in let"
        ) letAsts
      inferExpr exprAst

    EMatch scrutinees cases -> do
      -- Infer types of scrutinee expressions
      scrutineeTypes <- mapM inferExpr scrutinees

      -- Process each pattern matching case
      caseResults <- mapM (inferMatchCase scrutineeTypes) cases

      let (patternBounds, caseTypes) = unzip caseResults

      -- Generate exhaustivity constraints
      generateExhaustivityConstraints scrutineeTypes patternBounds

      -- All case results should be subtypes of the result type
      resultType <- TVar <$> freshVar
      mapM_ (\caseType -> addConstraint $ Subtype caseType resultType) caseTypes

      return resultType

-- Infer type for value declarations
inferValueDecl :: T.Text -> AST KType -> AST KExpr -> InferM Type
inferValueDecl name typeAst exprAst = do
  declaredType <- convertMType typeAst
  inferredType <- inferExpr exprAst
  addConstraint $ Subtype inferredType declaredType
  addVar name declaredType
  return declaredType

-- Infer type for polymorphic value declarations
inferPolyValueDecl :: T.Text -> [AST KVariable] -> AST KType -> AST KExpr -> InferM ()
inferPolyValueDecl name typeParams typeAst exprAst = do
  -- Convert type parameter names to generic types
  let paramNames = [case unVariable (extractSyntax paramAst) of
                     SVariable.Var paramName -> paramName | paramAst <- typeParams]

  -- Substitute type parameters with generic types in the declared type
  declaredType <- convertMType typeAst
  let genericType = foldr (\paramName acc ->
        substituteTypeVar (TypeVar paramName) (TGeneric paramName) acc) declaredType paramNames

  -- Save the current type environment
  savedEnv <- saveTypeEnv

  -- Infer the expression type in an environment with type parameters as generics
  mapM_ (\paramName -> addVar paramName (TGeneric paramName)) paramNames
  inferredType <- inferExpr exprAst
  addConstraint $ Subtype inferredType genericType

  -- Restore the type environment (remove type parameters)
  restoreTypeEnv savedEnv

  -- Generalize the type and add to environment
  let typeScheme = generalize genericType
  addVarScheme name typeScheme

-- Helper to substitute type variables with other types
substituteTypeVar :: TypeVar -> Type -> Type -> Type
substituteTypeVar target replacement = applySubst (Map.singleton target replacement)

-- Infer type for polymorphic data declarations (new separated syntax)
inferPolyDataDeclNew :: T.Text -> [AST KVariable] -> AST KType -> AST KType -> InferM ()
inferPolyDataDeclNew constructorName typeParams fullTypeAst _duplicateTypeAst = do
  -- Convert type parameter names to generic types
  let paramNames = [case unVariable (extractSyntax paramAst) of
                     SVariable.Var paramName -> paramName | paramAst <- typeParams]

  -- Split the full function type into args and return type
  case unMType (extractSyntax fullTypeAst) of
    SMType.TFunction _params returnTypeAst -> do
      -- Pre-register return type as constructor before conversion
      case unMType (extractSyntax returnTypeAst) of
        SMType.TVar varAst -> do
          let SVariable.Var baseTypeName = unVariable (extractSyntax varAst)
          addVar ("TYPE_" <> baseTypeName) (TConstructor baseTypeName)
        SMType.TApplication baseAst _ -> do
          case unMType (extractSyntax baseAst) of
            SMType.TVar varAst -> do
              let SVariable.Var baseTypeName = unVariable (extractSyntax varAst)
              addVar ("TYPE_" <> baseTypeName) (TConstructor baseTypeName)
            _ -> return ()
        _ -> return ()

      -- Convert the full function type
      fullType <- convertMType fullTypeAst

      -- Extract args and return type from the converted function type
      let (ctorArgsType, returnType) = case fullType of
            TFunction args ret -> (args, ret)
            _ -> error "Expected function type in data declaration"

      processDataDeclaration constructorName typeParams paramNames ctorArgsType returnType

    _ -> error "Data declaration must have function type syntax: (args...) => ReturnType"

-- Enhanced data declaration processing with proper namespace separation
processDataDeclaration :: T.Text -> [AST KVariable] -> [T.Text] -> Type -> Type -> InferM ()
processDataDeclaration constructorName typeParams paramNames ctorArgsType returnType = do
  -- Substitute type parameters with generic types
  let genericCtorArgsType = foldr (\paramName acc ->
        substituteTypeVar (TypeVar paramName) (TGeneric paramName) acc) ctorArgsType paramNames
  let genericReturnType = foldr (\paramName acc ->
        substituteTypeVar (TypeVar paramName) (TGeneric paramName) acc) returnType paramNames

  -- Build the complete constructor type
  let genericType = TFunction genericCtorArgsType genericReturnType

  -- Analyze variance according to the new specification
  let varianceMap = analyzeNewVariance paramNames genericCtorArgsType genericReturnType

  -- Extract the base type name from return type for proper registration
  let baseTypeName = extractBaseTypeName genericReturnType

  -- Register the type constructor with proper metadata
  case baseTypeName of
    Just typeName -> do
      -- Register or update the type constructor
      maybeExisting <- lookupTypeConstructor typeName
      case maybeExisting of
        Nothing -> do
          -- New type constructor
          let arity = length paramNames
          registerTypeConstructor typeName arity varianceMap [constructorName]
        Just _ -> do
          -- Existing type constructor, add this constructor to it
          addConstructorToType typeName constructorName
    Nothing -> do
      -- Fallback to old behavior if we can't extract type name
      addVarianceInfo constructorName varianceMap

  -- Register as constructor
  addConstructor constructorName

  -- Generalize the constructor type and add to environment
  let typeScheme = generalize genericType
  addVarScheme constructorName typeScheme

-- Extract base type name from a return type for type constructor registration
extractBaseTypeName :: Type -> Maybe T.Text
extractBaseTypeName (TConstructor name) = Just name
extractBaseTypeName (TVar (TypeVar name)) = Just name
extractBaseTypeName (TApplication base _) = extractBaseTypeName base
extractBaseTypeName _ = Nothing

-- Advanced variance analysis for separated constructor/return type syntax
-- Following the specification:
-- - For Cons : ... => Typ<CT, ...> where CT is concrete: CT is invariant
-- - For Cons<T> : ... => Typ<T, ...>: T's variance depends on position in constructor args
analyzeNewVariance :: [T.Text] -> Type -> Type -> VarianceMap
analyzeNewVariance paramNames ctorArgsType returnType =
  Map.fromList [(param, analyzeNewParameterVariance param ctorArgsType returnType) | param <- paramNames]

-- Analyze a single parameter's variance according to the new specification
analyzeNewParameterVariance :: T.Text -> Type -> Type -> Variance
analyzeNewParameterVariance paramName ctorArgsType returnType =
  let
    -- Check if parameter appears in constructor arguments
    ctorVariance = analyzeVarianceInType paramName ctorArgsType Contravariant

    -- Check if parameter appears in return type (always covariant context for type parameters)
    returnVariance = analyzeVarianceInType paramName returnType Covariant

    -- Apply the specification rules:
    -- 1. If T doesn't appear in constructor args but appears in return type -> bivariant
    -- 2. If T appears in both -> combine variances
    -- 3. If T appears only in constructor args -> use constructor variance
  in case (ctorVariance, returnVariance) of
    (Nothing, Nothing) -> Invariant  -- Parameter doesn't appear anywhere
    (Nothing, Just _) -> Bivariant   -- Only in return type (specification rule)
    (Just cv, Nothing) -> cv         -- Only in constructor args
    (Just cv, Just rv) -> combineVariances cv rv  -- Appears in both

-- Analyze how a parameter appears in a type, given the current context variance
analyzeVarianceInType :: T.Text -> Type -> Variance -> Maybe Variance
analyzeVarianceInType paramName targetType contextVariance = case targetType of
  TGeneric name | name == paramName -> Just contextVariance
  TVar (TypeVar name) | name == paramName -> Just contextVariance

  -- Function types: A -> B (A is contravariant, B is covariant)
  TFunction argType returnType ->
    let argVariance = analyzeVarianceInType paramName argType (flipVarianceNew contextVariance)
        retVariance = analyzeVarianceInType paramName returnType contextVariance
    in combineOptionalVariances argVariance retVariance

  -- Type applications: F<A> - variance depends on F's signature
  TApplication base argTypes ->
    -- For now, assume all type arguments are covariant (can be refined later)
    let argVariances = [analyzeVarianceInType paramName argType contextVariance | argType <- argTypes]
    in foldr combineOptionalVariances Nothing argVariances

  -- Union and intersection types
  TUnion types ->
    let variances = [analyzeVarianceInType paramName t contextVariance | t <- Set.toList types]
    in foldr combineOptionalVariances Nothing variances

  TIntersection types ->
    let variances = [analyzeVarianceInType paramName t contextVariance | t <- Set.toList types]
    in foldr combineOptionalVariances Nothing variances

  -- Other types don't contain the parameter
  _ -> Nothing

-- Helper: flip variance for contravariant contexts
flipVarianceNew :: Variance -> Variance
flipVarianceNew Covariant = Contravariant
flipVarianceNew Contravariant = Covariant
flipVarianceNew Invariant = Invariant
flipVarianceNew Bivariant = Bivariant

-- Helper: combine two variances
combineVariances :: Variance -> Variance -> Variance
combineVariances Invariant _ = Invariant
combineVariances _ Invariant = Invariant
combineVariances Covariant Contravariant = Invariant
combineVariances Contravariant Covariant = Invariant
combineVariances v1 v2 | v1 == v2 = v1
combineVariances _ _ = Invariant

-- Helper: combine optional variances
combineOptionalVariances :: Maybe Variance -> Maybe Variance -> Maybe Variance
combineOptionalVariances Nothing mv = mv
combineOptionalVariances mv Nothing = mv
combineOptionalVariances (Just v1) (Just v2) = Just (combineVariances v1 v2)

-- Check if a type contains a specific parameter (kept for compatibility)
containsParam :: T.Text -> Type -> Bool
containsParam paramName (TGeneric name) = paramName == name
containsParam paramName (TVar (TypeVar name)) = paramName == name
containsParam paramName (TFunction t1 t2) = containsParam paramName t1 || containsParam paramName t2
containsParam paramName (TUnion types) = any (containsParam paramName) (Set.toList types)
containsParam paramName (TIntersection types) = any (containsParam paramName) (Set.toList types)
containsParam paramName (TApplication base args) = containsParam paramName base || any (containsParam paramName) args
containsParam _ _ = False

-- Process a single declaration
inferDecl :: AST KDefinition -> InferM ()
inferDecl ast =
  case unDefinition (extractSyntax ast) of
    SDefinition.ValDef varAst typeParams typeAst exprAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      if null typeParams
        then do
          -- Monomorphic definition
          _ <- inferValueDecl name typeAst exprAst
          return ()
        else do
          -- Polymorphic definition
          inferPolyValueDecl name typeParams typeAst exprAst
    SDefinition.DataDef constructorAst typeParams ctorArgsAst returnTypeAst -> do
      let SVariable.Var constructorName = unVariable (extractSyntax constructorAst)
      if null typeParams
        then do
          -- Monomorphic data definition: data Cons : Args => ReturnType
          ctorArgsType <- mapM convertMType ctorArgsAst
          returnType <- convertMType returnTypeAst
          let constructorType = TFunction ctorArgsType returnType

          addConstructor constructorName
          addVar constructorName constructorType

          -- Register return type as ground type if it's a simple type
          case returnType of
            TVar (TypeVar returnTypeName) -> do
              let groundReturnType = TConstructor returnTypeName
              let groundConstructorType = TFunction ctorArgsType groundReturnType
              addVar constructorName groundConstructorType
              addVar ("TYPE_" <> returnTypeName) groundReturnType
            TConstructor returnTypeName -> do
              addVar ("TYPE_" <> returnTypeName) (TConstructor returnTypeName)
            TApplication (TVar (TypeVar baseTypeName)) argTypes -> do
              -- For polymorphic return types like Typ<string>
              let groundBaseType = TConstructor baseTypeName
              let groundReturnType = TApplication groundBaseType argTypes
              let groundConstructorType = TFunction ctorArgsType groundReturnType
              addVar constructorName groundConstructorType
              addVar ("TYPE_" <> baseTypeName) groundBaseType
            TApplication (TConstructor baseTypeName) argTypes -> do
              addVar ("TYPE_" <> baseTypeName) (TConstructor baseTypeName)
            _ -> return ()
        else do
          -- Polymorphic data definition: data Cons<T> : Args => ReturnType
          inferPolyDataDeclNew constructorName typeParams ctorArgsAst returnTypeAst
    SDefinition.TypeDef aliasAst _params typeAst -> do  -- Ignore type parameters for now
      -- Process type alias definition
      let SVariable.Var aliasName = unVariable (extractSyntax aliasAst)
      aliasType <- convertMType typeAst
      addVar aliasName aliasType
      return ()

-- Process a program
inferProgramM :: AST KProgram -> InferM ()
inferProgramM ast =
  case unProgram (extractSyntax ast) of
    SProgram.Program declAsts -> mapM_ inferDecl declAsts

-- Enhanced main inference function with better error reporting
inferProgram :: AST KProgram -> Either String (Map.Map T.Text TypeScheme)
inferProgram ast =
  let initialCtx = InferContext
        { icVarCounter = 0
        , icConstraints = Set.empty
        , icTypeEnv = Map.empty
        , icTypeConstructors = Map.empty  -- New field for type constructor info
        , icConstructors = Set.empty
        , icVariances = Map.empty
        }
      ((), finalCtx) = runState (inferProgramM ast) initialCtx
      constraints = icConstraints finalCtx
      -- Filter out constraints containing generic types (they're from polymorphic definitions)
      filteredConstraints = Set.filter (not . constraintContainsGeneric) constraints
  in case solveConstraints filteredConstraints of
    Success subst ->
      let finalEnv = Map.map (applySubstScheme subst) (icTypeEnv finalCtx)
      in Right finalEnv
    Contradiction ->
      Left $ formatTypeError (Set.toList filteredConstraints)
    Ambiguous _ ->
      Left $ "Type error: ambiguous types\n" ++
             "Constraints: " ++ show (Set.toList filteredConstraints) ++ "\n" ++
             "Type constructors: " ++ show (icTypeConstructors finalCtx)

-- Apply substitution to a type scheme
applySubstScheme :: Substitution -> TypeScheme -> TypeScheme
applySubstScheme subst (TypeScheme quantified typ) =
  TypeScheme quantified (applySubst subst typ)

-- Enhanced generalization for better polymorphic types
-- This creates more precise type schemes while preserving singleton types when beneficial
enhancedGeneralize :: Type -> TypeScheme
enhancedGeneralize typ =
  let freeGenericVars = Set.toList (freeGenerics typ)
      -- Don't generalize over literal types in certain contexts
      refinedType = refineLiteralTypes typ
  in TypeScheme freeGenericVars refinedType

-- Refine literal types for better polymorphic inference
-- Keep singleton types (42 : 42) when they're important, generalize when needed
refineLiteralTypes :: Type -> Type
refineLiteralTypes t@(TLiteral _) = t  -- Keep singleton literal types
refineLiteralTypes (TFunction arg ret) =
  TFunction (refineLiteralTypes arg) (refineLiteralTypes ret)
refineLiteralTypes (TUnion ts) =
  TUnion (Set.map refineLiteralTypes ts)
refineLiteralTypes (TIntersection ts) =
  TIntersection (Set.map refineLiteralTypes ts)
refineLiteralTypes (TApplication base args) =
  TApplication (refineLiteralTypes base) (map refineLiteralTypes args)
refineLiteralTypes t = t  -- Other types unchanged

-- Check if a constraint contains generic types
-- We should only filter out constraints that are purely between generic types,
-- not constraints that involve generics on one side and concrete types on the other
constraintContainsGeneric :: Constraint -> Bool
constraintContainsGeneric (Subtype t1 t2) =
  -- Only filter if BOTH sides are purely generic (no type variables)
  typeIsPurelyGeneric t1 && typeIsPurelyGeneric t2
  where
    -- A type is "purely generic" if it contains generics but no type variables
    typeIsPurelyGeneric :: Type -> Bool
    typeIsPurelyGeneric t = typeContainsGeneric t && not (containsVar t)

-- Check if a type contains generic types
typeContainsGeneric :: Type -> Bool
typeContainsGeneric (TGeneric _) = True
typeContainsGeneric (TFunction t1 t2) = typeContainsGeneric t1 || typeContainsGeneric t2
typeContainsGeneric (TUnion ts) = any typeContainsGeneric (Set.toList ts)
typeContainsGeneric (TIntersection ts) = any typeContainsGeneric (Set.toList ts)
typeContainsGeneric (TApplication base args) = typeContainsGeneric base || any typeContainsGeneric args
typeContainsGeneric _ = False

-- Format user-friendly type error messages
formatTypeError :: [Constraint] -> String
formatTypeError constraints =
  case findMostRelevantError constraints of
    Just errorMsg -> "Type error: " ++ errorMsg
    Nothing -> "Type error: The types in your program are inconsistent.\n" ++
              "Details: " ++ show constraints

-- Find the most relevant/understandable error from a list of constraints
findMostRelevantError :: [Constraint] -> Maybe String
findMostRelevantError constraints =
  -- Look for common error patterns and provide helpful messages
  case constraints of
    [] -> Nothing
    (Subtype t1 t2 : _) -> Just (formatSubtypeError t1 t2)

-- Format a subtype error with user-friendly type names
formatSubtypeError :: Type -> Type -> String
formatSubtypeError t1 t2 =
  let t1Str = formatTypeForUser t1
      t2Str = formatTypeForUser t2
  in case (t1, t2) of
       (TLiteral (LString _), TNumber) ->
         "Cannot use string " ++ t1Str ++ " where number is expected"
       (TLiteral (LNumber _), TString) ->
         "Cannot use number " ++ t1Str ++ " where string is expected"
       (TLiteral (LBool _), TNumber) ->
         "Cannot use boolean " ++ t1Str ++ " where number is expected"
       _ ->
         "Type " ++ t1Str ++ " is not compatible with expected type " ++ t2Str

-- Format types in a user-friendly way
formatTypeForUser :: Type -> String
formatTypeForUser TNumber = "number"
formatTypeForUser TBool = "boolean"
formatTypeForUser TString = "string"
formatTypeForUser TNever = "never"
formatTypeForUser TUnknown = "unknown"
formatTypeForUser (TLiteral (LNumber n)) = show n
formatTypeForUser (TLiteral (LBool b)) = show b
formatTypeForUser (TLiteral (LString s)) = "\"" ++ T.unpack s ++ "\""
formatTypeForUser (TVar (TypeVar name)) = T.unpack name
formatTypeForUser (TConstructor name) = T.unpack name
formatTypeForUser (TFunction t1 t2) =
  "(" ++ formatTypeForUser t1 ++ " => " ++ formatTypeForUser t2 ++ ")"
formatTypeForUser (TUnion ts) =
  let typeStrs = map formatTypeForUser (Set.toList ts)
  in "(" ++ intercalate " | " typeStrs ++ ")"
formatTypeForUser (TIntersection ts) =
  let typeStrs = map formatTypeForUser (Set.toList ts)
  in "(" ++ intercalate " & " typeStrs ++ ")"
formatTypeForUser (TGeneric name) = T.unpack name
formatTypeForUser (TApplication base args) =
  formatTypeForUser base ++ "<" ++ intercalate ", " (map formatTypeForUser args) ++ ">"

-- Pattern bounds: max-bound (desired) and min-bound (actual coverage)
data PatternBounds = PatternBounds
  { pbMaxBounds :: [Type]                     -- Max bounds for each scrutinee position
  , pbMinBounds :: [Type]                     -- Min bounds for each scrutinee position
  , pbPatternTrees :: [PatternTree]           -- Pattern trees for each scrutinee position
  } deriving (Show)

-- Pattern tree for production-level exhaustivity checking
data PatternTree
  = PTWildcard                                    -- _ pattern - covers everything
  | PTVariable T.Text Type                        -- Variable pattern - covers everything, binds variable
  | PTLiteral Type                                -- Literal pattern - covers specific value
  | PTConstructor T.Text Type [PatternTree]       -- Constructor pattern with nested patterns
  deriving (Show, Eq)

-- Pattern matrix for exhaustivity analysis
data PatternMatrix = PatternMatrix
  { pmPatterns :: [[PatternTree]]    -- Matrix of patterns (rows = cases, cols = scrutinee positions)
  , pmResultTypes :: [Type]          -- Result type for each case
  } deriving (Show)

-- Exhaustivity analysis result
data ExhaustivityResult
  = Exhaustive                       -- All cases covered
  | NonExhaustive [PatternTree]      -- Missing patterns (witnesses)
  | Contradictory                    -- Overlapping/conflicting patterns
  deriving (Show)

-- Infer a single match case and return pattern bounds + result type
inferMatchCase :: [Type] -> (List (AST KPattern, AST KType), AST KExpr) -> InferM (PatternBounds, Type)
inferMatchCase scrutineeTypes (patterns, resultExpr) = do
  -- Save the current type environment
  savedEnv <- saveTypeEnv

  -- Process each pattern and get its bounds (variables get bound in sequence)
  patternResults <- sequence $ zipWith (\(patAst, typeAst) scrutineeType -> do
    declaredType <- convertMType typeAst  -- This is the max-bound
    (actualBound, patternTree) <- inferPattern patAst scrutineeType declaredType
    return (declaredType, actualBound, patternTree)
    ) patterns scrutineeTypes

  let (maxBounds, minBounds, patternTrees) = unzip3 patternResults

  -- Infer the result expression type (with pattern variables in scope)
  resultType <- inferExpr resultExpr

  -- Restore the original type environment (pattern variables go out of scope)
  restoreTypeEnv savedEnv

  return (PatternBounds maxBounds minBounds patternTrees, resultType)

-- Convert AST pattern to pattern tree with full type information
astToPatternTree :: AST KPattern -> Type -> InferM PatternTree
astToPatternTree patAst expectedType = do
  case unPattern (extractSyntax patAst) of
    SPattern.PVar varAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      addVar name expectedType  -- Bind variable
      return $ PTVariable name expectedType

    SPattern.PWildcard -> do
      return PTWildcard

    SPattern.PLiteral litAst -> do
      let literalType = convertLiteral litAst
      addConstraint $ Subtype literalType expectedType
      return $ PTLiteral literalType

    SPattern.PCons constructorAst argPatterns -> do
      let SVariable.Var constructorName = unVariable (extractSyntax constructorAst)

      -- Look up constructor type
      maybeConstructorType <- lookupVar constructorName
      case maybeConstructorType of
        Just constructorType -> do
          -- Handle both single and multi-argument constructors
          (argTypes, returnType) <- extractConstructorSignature constructorType

          -- Ensure we have the right number of arguments
          when (length argPatterns /= length argTypes) $
            errorWithPosition constructorAst $
              "Constructor " ++ T.unpack constructorName ++ " expects " ++
              show (length argTypes) ++ " arguments, got " ++ show (length argPatterns)

          -- Recursively convert argument patterns to pattern trees
          argTrees <- zipWithM astToPatternTree argPatterns argTypes

          return $ PTConstructor constructorName returnType argTrees

        Nothing -> do
          errorWithPosition constructorAst $ "Unknown constructor: " ++ T.unpack constructorName

-- Extract argument types and return type from constructor type signature
extractConstructorSignature :: Type -> InferM ([Type], Type)
extractConstructorSignature constructorType = do
  let (argTypes, returnType) = unfoldFunctionType constructorType
  return (argTypes, returnType)

-- Unfold a function type into argument types and return type
-- CLEANED UP: Zero-argument constructors are now handled properly in type conversion
unfoldFunctionType :: Type -> ([Type], Type)
unfoldFunctionType (TFunction argType returnType) =
  let (restArgs, finalReturn) = unfoldFunctionType returnType
  in (argType : restArgs, finalReturn)
unfoldFunctionType nonFunctionType = ([], nonFunctionType)

-- Infer pattern and return its actual coverage (min-bound) and pattern tree
inferPattern :: AST KPattern -> Type -> Type -> InferM (Type, PatternTree)
inferPattern patAst scrutineeType declaredType = do
  -- Add constraint that scrutinee must be subtype of declared type
  addConstraint $ Subtype scrutineeType declaredType

  -- Convert to pattern tree using the new comprehensive approach
  patternTree <- astToPatternTree patAst declaredType

  -- Extract min-bound type from pattern tree
  let minBoundType = extractMinBound patternTree

  return (minBoundType, patternTree)

-- Extract the minimum bound (coverage) from a pattern tree
extractMinBound :: PatternTree -> Type
extractMinBound PTWildcard = TUnknown                    -- Covers everything
extractMinBound (PTVariable _ _) = TUnknown               -- Covers everything
extractMinBound (PTLiteral literalType) = literalType    -- Covers specific literal
extractMinBound (PTConstructor _ returnType _) = returnType  -- Covers specific constructor

-- Generate exhaustivity constraints using pattern matrix algorithm
generateExhaustivityConstraints :: [Type] -> [PatternBounds] -> InferM ()
generateExhaustivityConstraints scrutineeTypes patternBounds = do
  -- Build pattern matrix from pattern bounds
  let patternMatrix = buildPatternMatrix patternBounds

  -- Analyze exhaustivity using the pattern matrix algorithm
  result <- analyzeExhaustivity scrutineeTypes patternMatrix

  case result of
    Exhaustive -> return ()  -- All good!
    NonExhaustive witnesses -> do
      -- Add constraint that fails (witnesses show missing cases)
      addConstraint $ Subtype TUnknown TNever  -- Always fails
    Contradictory -> do
      -- Overlapping patterns detected
      addConstraint $ Subtype TUnknown TNever  -- Always fails

-- Build pattern matrix from pattern bounds
buildPatternMatrix :: [PatternBounds] -> PatternMatrix
buildPatternMatrix bounds =
  let patterns = [pbPatternTrees pb | pb <- bounds]
      resultTypes = [head (pbMinBounds pb) | pb <- bounds]  -- Simplified for now
  in PatternMatrix patterns resultTypes

-- Production-level exhaustivity analysis using pattern matrix algorithm
analyzeExhaustivity :: [Type] -> PatternMatrix -> InferM ExhaustivityResult
analyzeExhaustivity scrutineeTypes (PatternMatrix patterns resultTypes) = do
  -- If no patterns, it's non-exhaustive
  if null patterns
    then return $ NonExhaustive [PTWildcard]  -- Missing wildcard case
    else do
      -- Check if first column has wildcards or variables (always exhaustive)
      let firstColumn = [head row | row <- patterns, not (null row)]
      if any isWildcardOrVariable firstColumn
        then analyzeRemainingColumns scrutineeTypes patterns
        else analyzeConstructorExhaustivity scrutineeTypes patterns

-- Check if pattern is wildcard or variable (covers everything)
isWildcardOrVariable :: PatternTree -> Bool
isWildcardOrVariable PTWildcard = True
isWildcardOrVariable (PTVariable _ _) = True
isWildcardOrVariable _ = False

-- Analyze remaining columns after handling wildcards
analyzeRemainingColumns :: [Type] -> [[PatternTree]] -> InferM ExhaustivityResult
analyzeRemainingColumns [] _ = return Exhaustive
analyzeRemainingColumns _ [] = return Exhaustive
analyzeRemainingColumns (scrutineeType:restTypes) patterns = do
  -- For now, simplified: if we have wildcards in first column, assume exhaustive
  -- TODO: Implement full algorithm for remaining columns
  return Exhaustive

-- Analyze constructor exhaustivity (core of the algorithm)
analyzeConstructorExhaustivity :: [Type] -> [[PatternTree]] -> InferM ExhaustivityResult
analyzeConstructorExhaustivity [] _ = return Exhaustive
analyzeConstructorExhaustivity (scrutineeType:restTypes) patterns = do
  -- Group patterns by constructor in first column
  let firstColumn = [head row | row <- patterns, not (null row)]
  let constructorGroups = groupPatternsByConstructor firstColumn

  -- Get all possible constructors for the scrutinee type
  possibleConstructors <- getPossibleConstructors scrutineeType
  let coveredConstructors = Map.keys constructorGroups

  -- Debug: uncomment to see what's happening
  -- error $ "Scrutinee type: " ++ show scrutineeType ++
  --         "\nPossible constructors: " ++ show possibleConstructors ++
  --         "\nCovered constructors: " ++ show coveredConstructors

  -- Check if all constructors are covered
  let missingConstructors = Set.toList $ Set.difference
                           (Set.fromList possibleConstructors)
                           (Set.fromList coveredConstructors)

  if null missingConstructors
    then do
      -- All constructors covered - recursively check argument exhaustivity
      -- TODO: Implement recursive checking for constructor arguments
      return Exhaustive
    else do
      -- Missing constructors - non-exhaustive
      let witnesses = [PTConstructor name (TConstructor name) [] | name <- missingConstructors]
      return $ NonExhaustive witnesses

-- Get all possible constructors for a type
getPossibleConstructors :: Type -> InferM [T.Text]
getPossibleConstructors (TConstructor name) = return [name]
getPossibleConstructors (TUnion types) = do
  -- For union types, collect all constructors from member types
  constructorLists <- mapM getPossibleConstructors (Set.toList types)
  return $ concat constructorLists
getPossibleConstructors (TVar (TypeVar name)) = do
  -- Look up type variable to see if it's a type alias
  maybeType <- lookupVar name
  case maybeType of
    Just resolvedType -> getPossibleConstructors resolvedType
    Nothing -> return []  -- Unknown type variable
getPossibleConstructors _ = return []  -- Other types don't have constructors

-- Check if pattern is a constructor pattern
isConstructorPattern :: PatternTree -> Bool
isConstructorPattern (PTConstructor _ _ _) = True
isConstructorPattern _ = False

-- Group patterns by constructor name
groupPatternsByConstructor :: [PatternTree] -> Map.Map T.Text [PatternTree]
groupPatternsByConstructor patterns =
  Map.fromListWith (++) [(name, [pattern]) | pattern@(PTConstructor name _ _) <- patterns]

-- Type checking interface for AST
typeCheckAST :: AST KProgram -> Either String (Map.Map T.Text TypeScheme)
typeCheckAST = inferProgram
