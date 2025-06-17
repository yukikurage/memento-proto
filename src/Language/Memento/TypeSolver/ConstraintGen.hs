{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.ConstraintGen where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad (unless, when, zipWithM)
import Data.List (unzip3)
import GHC.Base (List)
import Language.Memento.Syntax
import Language.Memento.Syntax.Expr (Expr(..), Let(..))
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

-- Type inference context
data InferContext = InferContext
  { icVarCounter :: Int
  , icConstraints :: ConstraintSet
  , icTypeEnv :: Map.Map T.Text Type
  , icConstructors :: Set.Set T.Text  -- Track known constructor names
  } deriving (Show)

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

-- Look up variable type
lookupVar :: T.Text -> InferM (Maybe Type)
lookupVar name = do
  ctx <- get
  return $ Map.lookup name (icTypeEnv ctx)

-- Add variable to environment
addVar :: T.Text -> Type -> InferM ()
addVar name typ = do
  ctx <- get
  put ctx { icTypeEnv = Map.insert name typ (icTypeEnv ctx) }

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

-- Save current type environment
saveTypeEnv :: InferM (Map.Map T.Text Type)
saveTypeEnv = do
  ctx <- get
  return $ icTypeEnv ctx

-- Restore type environment
restoreTypeEnv :: Map.Map T.Text Type -> InferM ()
restoreTypeEnv env = do
  ctx <- get
  put ctx { icTypeEnv = env }

-- Helper to extract function argument type
getFunctionArg :: Type -> Type
getFunctionArg (TFunction arg _) = arg
getFunctionArg t = t  -- fallback

-- Extract syntax from AST, handling the metadata
extractSyntax :: AST a -> Syntax AST a
extractSyntax ast =
  case unHFix ast of
    (metadata :*: syntax) -> syntax

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
      -- Check if this name is already defined in the environment
      maybeType <- lookupVar name
      case maybeType of
        Just (TConstructor _) -> return $ TConstructor name  -- Use ground type if registered  
        Just (TUnion types) -> return $ TUnion types  -- Resolve union types for exhaustivity
        Just _ -> return $ TVar (TypeVar name)  -- Use variable for other types
        Nothing -> return $ TVar (TypeVar name)  -- Unknown name, use variable
    SMType.TLiteral litAst ->
      return $ convertLiteral litAst
    SMType.TFunction params retType -> do
      paramTypes <- mapM (convertMType . snd) params
      returnType <- convertMType retType
      return $ foldr TFunction returnType paramTypes
    SMType.TUnion types -> do
      convertedTypes <- mapM convertMType types
      return $ mkUnion convertedTypes
    SMType.TIntersection types -> do
      convertedTypes <- mapM convertMType types
      return $ mkIntersection convertedTypes

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
        Nothing -> error $ "Unbound variable: " ++ show name

    ELiteral litAst -> return $ convertLiteral litAst

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
          _ -> error "Complex patterns not yet supported in lambda"
        ) params
      bodyType <- inferExpr bodyAst
      return $ foldr TFunction bodyType paramTypes

    EApply funcAst argAsts -> do
      funcType <- inferExpr funcAst
      argTypes <- mapM inferExpr argAsts
      resultType <- TVar <$> freshVar
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
              _ -> error "Complex patterns not yet supported in let"
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

-- Process a single declaration
inferDecl :: AST KDefinition -> InferM ()
inferDecl ast =
  case unDefinition (extractSyntax ast) of
    SDefinition.ValDef varAst typeAst exprAst -> do
      let SVariable.Var name = unVariable (extractSyntax varAst)
      _ <- inferValueDecl name typeAst exprAst
      return ()
    SDefinition.DataDef constructorAst typeAst -> do
      -- Process data constructor definition
      let SVariable.Var constructorName = unVariable (extractSyntax constructorAst)
      -- Don't register as constructor yet - process type first
      constructorType <- convertMType typeAst
      addConstructor constructorName  -- Register as constructor after type processing
      addVar constructorName constructorType
      
      -- Register return types as ground types
      case constructorType of
        TFunction _ (TVar (TypeVar returnTypeName)) -> do
          let groundReturnType = TConstructor returnTypeName
          let groundConstructorType = TFunction (getFunctionArg constructorType) groundReturnType
          addVar constructorName groundConstructorType
          addVar ("TYPE_" <> returnTypeName) groundReturnType
        TFunction _ (TConstructor returnTypeName) -> do
          -- Handle case where return type was already converted to TConstructor
          let groundReturnType = TConstructor returnTypeName
          let groundConstructorType = TFunction (getFunctionArg constructorType) groundReturnType
          addVar constructorName groundConstructorType
          addVar ("TYPE_" <> returnTypeName) groundReturnType
        TVar (TypeVar typeName) -> do
          let groundType = TConstructor typeName  
          addVar constructorName groundType
          addVar ("TYPE_" <> typeName) groundType
        TConstructor typeName -> do
          -- Handle case where type was already converted to TConstructor
          let groundType = TConstructor typeName  
          addVar constructorName groundType
          addVar ("TYPE_" <> typeName) groundType
        _ -> return ()
      return ()
    SDefinition.TypeDef aliasAst typeAst -> do
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

-- Main inference function
inferProgram :: AST KProgram -> Either String (Map.Map T.Text Type)
inferProgram ast =
  let initialCtx = InferContext 0 Set.empty Map.empty Set.empty
      ((), finalCtx) = runState (inferProgramM ast) initialCtx
      constraints = icConstraints finalCtx
  in case solveConstraints constraints of
    Success subst ->
      let finalEnv = Map.map (applySubst subst) (icTypeEnv finalCtx)
      in Right finalEnv
    Contradiction -> Left "Type error: contradictory constraints"
    Ambiguous _ -> Left "Type error: ambiguous types"

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
            error $ "Constructor " ++ T.unpack constructorName ++ " expects " ++ 
                   show (length argTypes) ++ " arguments, got " ++ show (length argPatterns)
          
          -- Recursively convert argument patterns to pattern trees
          argTrees <- zipWithM astToPatternTree argPatterns argTypes
          
          return $ PTConstructor constructorName returnType argTrees
          
        Nothing -> do
          error $ "Unknown constructor: " ++ T.unpack constructorName

-- Extract argument types and return type from constructor type signature
extractConstructorSignature :: Type -> InferM ([Type], Type)
extractConstructorSignature constructorType = do
  let (argTypes, returnType) = unfoldFunctionType constructorType
  return (argTypes, returnType)

-- Unfold a function type into argument types and return type
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
typeCheckAST :: AST KProgram -> Either String (Map.Map T.Text Type)
typeCheckAST = inferProgram
