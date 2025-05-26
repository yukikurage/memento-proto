{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeChecker.InferTypes (
  TCType (..),
  EffectK (..), -- Renamed from TCEffects for clarity (Kind of Effect)
  TypeVar (..),
  EffectVar (..),
  Bound (..),
  Constraint (..),
  Substitution (..),
  UnificationError (..),
  applySubst,
  applySubstEffects,
  applySubstBound,
  composeSubst,
  getSubstMap,
  emptySubst,
  singleSubst,
  unifyPure,
  unifyEffects,
  bindVar,
  occursCheck,
  occursCheckEffects,
  checkBoundCompatibility,
  isBoundSubset,
  getUnifications,
  freshTypeVar, -- Added for getUnifications
  generateConstraints, -- Alias for getUnifications
  solve,
  applyConstraint,
  mapError,
  inferHM,
  mapConstraintError
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text) -- Keep existing Text import
import qualified Data.Text as Text (pack) -- For freshTypeVar and mapConstraintError, qualified
import GHC.Generics (Generic)
import Language.Memento.Syntax (Effect (..), Expr(..), BinOp(..), Pattern(..), TypeError(..)) -- Added TypeError
import Data.Maybe (fromMaybe)
import Control.Monad.Except (Except, throwError, catchError, liftEither) -- Added liftEither
import Control.Monad (unless)
import Language.Memento.TypeChecker.Monad (TypeCheck, getEnv, addBinding, withBinding, withBindings, resolveType, unify, isSubEffects, extractConstructorType, extractHandlerType, buildConstructorType) -- Add TypeCheck and environment accessors
import Control.Monad.State (gets, modify) -- For freshTypeVar
import Language.Memento.TypeChecker.TypeConversion (typeToTCType) -- Added import


-- Represents a type variable
newtype TypeVar = TV Text deriving (Show, Eq, Ord, Generic)

-- Represents an effect variable
newtype EffectVar = EV Text deriving (Show, Eq, Ord, Generic)

-- Represents the bound of an effect, as described in the issue
-- Omega can be represented by Nothing for the upper bound of effects,
-- or a finite set of effects.
data Bound = BoundOmega | BoundFinite (Set Effect) deriving (Show, Eq, Ord, Generic)

-- TypeChecker Types (TCType)
data TCType
  = TCNum -- Number
  | TCBool -- Boolean
  | TCVar TypeVar -- Type Variable (e.g., id_0)
  | TCFun TCType TCType EffectK -- Function: arg_type -> return_type with effects
  | TCHandler TCType EffectK TCType EffectK -- Handler: (handler_arg_type with consumed_effects) -> (handler_return_type with produced_effects)
  | TCAlgebraicData Text [TCType] -- ADT: Name and potential type arguments (though polymorphism is future)
  | TCTuple [TCType] -- Tuple
  deriving (Show, Eq, Ord, Generic)

-- Represents the "kind" of effects in TCType.
-- It can be a concrete set of known effects, an effect variable,
-- or a bounded effect as per the issue description.
data EffectK
  = EffectsConcrete (Set Effect) -- A concrete set of effects, e.g., {Throw, Nondet}
  | EffectsVar EffectVar         -- An effect variable, e.g., eff_0
  | EffectsBound { lowerBound :: Bound, upperBound :: Bound } -- Effects bounded by known sets or Omega
                                                              -- e.g., Bound <Throw> Omega
                                                              -- lowerBound usually specific effects, upperBound usually Omega or a larger set
  deriving (Show, Eq, Ord, Generic)

-- Applying a substitution to a TCType
applySubst :: Substitution -> TCType -> TCType
applySubst s@(Substitution subMap) tcType = case tcType of
  TCNum -> TCNum
  TCBool -> TCBool
  TCVar tv -> fromMaybe tcType (Map.lookup tv subMap) 
  TCFun argType retType effects ->
    TCFun (applySubst s argType) (applySubst s retType) (applySubstEffects s effects)
  TCHandler argType consumedEffects retType producedEffects ->
    TCHandler (applySubst s argType) (applySubstEffects s consumedEffects) (applySubst s retType) (applySubstEffects s producedEffects)
  TCAlgebraicData name tyArgs -> TCAlgebraicData name (map (applySubst s) tyArgs)
  TCTuple types -> TCTuple (map (applySubst s) types)

-- Applying a substitution to an EffectK
applySubstEffects :: Substitution -> EffectK -> EffectK
applySubstEffects s@(Substitution _subMap) effectK = case effectK of
  EffectsConcrete _ -> effectK 
  EffectsVar _ev -> effectK 
  EffectsBound lower upper -> EffectsBound (applySubstBound s lower) (applySubstBound s upper)

-- Applying a substitution to a Bound
applySubstBound :: Substitution -> Bound -> Bound
applySubstBound _ bound = bound 

-- Composing two substitutions
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s2@(Substitution s2Map) (Substitution s1Map) =
  Substitution $ Map.map (applySubst s2) s1Map `Map.union` s2Map

-- Helper to extract the map from Substitution
getSubstMap :: Substitution -> Map TypeVar TCType
getSubstMap (Substitution m) = m

-- Empty substitution
emptySubst :: Substitution
emptySubst = Substitution Map.empty

-- Substitution for a single type variable
singleSubst :: TypeVar -> TCType -> Substitution
singleSubst tv t = Substitution $ Map.singleton tv t

-- Unification function
unifyPure :: TCType -> TCType -> Except UnificationError Substitution
unifyPure t1 t2 | t1 == t2 = return emptySubst
unifyPure (TCVar var) t = bindVar var t
unifyPure t (TCVar var) = bindVar var t
unifyPure (TCFun arg1 ret1 eff1) (TCFun arg2 ret2 eff2) = do
  s1 <- unifyPure arg1 arg2
  s2 <- unifyPure (applySubst s1 ret1) (applySubst s1 ret2)
  s3 <- unifyEffects (applySubstEffects (s2 `composeSubst` s1) eff1) (applySubstEffects (s2 `composeSubst` s1) eff2)
  return $ s3 `composeSubst` s2 `composeSubst` s1
unifyPure (TCHandler arg1 consomm1 ret1 prod1) (TCHandler arg2 consomm2 ret2 prod2) = do
  s1 <- unifyPure arg1 arg2
  s2 <- unifyEffects (applySubstEffects s1 consomm1) (applySubstEffects s1 consomm2)
  s3 <- unifyPure (applySubst (s2 `composeSubst` s1) ret1) (applySubst (s2 `composeSubst` s1) ret2) 
  s4 <- unifyEffects (applySubstEffects (s3 `composeSubst` s2 `composeSubst` s1) prod1) (applySubstEffects (s3 `composeSubst` s2 `composeSubst` s1) prod2)
  return $ s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
unifyPure (TCAlgebraicData name1 args1) (TCAlgebraicData name2 args2)
  | name1 == name2 && length args1 == length args2 = unifyMany args1 args2
  | otherwise = throwError $ TypeMismatch (TCAlgebraicData name1 args1) (TCAlgebraicData name2 args2)
unifyPure (TCTuple fields1) (TCTuple fields2)
  | length fields1 == length fields2 = unifyMany fields1 fields2
  | otherwise = throwError $ TypeMismatch (TCTuple fields1) (TCTuple fields2)
unifyPure t1 t2 = throwError $ TypeMismatch t1 t2

-- Helper to unify lists of types
unifyMany :: [TCType] -> [TCType] -> Except UnificationError Substitution
unifyMany [] [] = return emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  s1 <- unifyPure t1 t2
  s2 <- unifyMany (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  return $ s2 `composeSubst` s1
unifyMany _ _ = error "unifyMany called with lists of different lengths" 

-- Bind a type variable to a type, performing occurs check
bindVar :: TypeVar -> TCType -> Except UnificationError Substitution
bindVar var t | t == TCVar var = return emptySubst
              | occursCheck var t = throwError $ OccursCheckFailed var t
              | otherwise = return $ singleSubst var t

-- Occurs check: check if a type variable occurs in a type
occursCheck :: TypeVar -> TCType -> Bool
occursCheck var (TCVar v) = var == v
occursCheck var (TCFun arg ret eff) = occursCheck var arg || occursCheck var ret || occursCheckEffects var eff
occursCheck var (TCHandler arg ce ret pe) = occursCheck var arg || occursCheckEffects var ce || occursCheck var ret || occursCheckEffects var pe
occursCheck var (TCAlgebraicData _ args) = any (occursCheck var) args
occursCheck var (TCTuple fields) = any (occursCheck var) fields
occursCheck _ _ = False

-- Occurs check for effects
occursCheckEffects :: TypeVar -> EffectK -> Bool
occursCheckEffects var effectK = False 

-- Unify two EffectK instances
unifyEffects :: EffectK -> EffectK -> Except UnificationError Substitution
unifyEffects e1 e2 | e1 == e2 = return emptySubst
unifyEffects (EffectsVar ev1) (EffectsVar ev2) | ev1 == ev2 = return emptySubst
unifyEffects (EffectsConcrete s1) (EffectsConcrete s2)
  | s1 == s2 = return emptySubst
  | otherwise = throwError $ EffectMismatch (EffectsConcrete s1) (EffectsConcrete s2)
unifyEffects (EffectsBound lb1 ub1) (EffectsBound lb2 ub2) = do
  unless (checkBoundCompatibility lb1 ub1 lb2 ub2) $
    throwError $ EffectMismatch (EffectsBound lb1 ub1) (EffectsBound lb2 ub2)
  return emptySubst 
unifyEffects e1 e2 = throwError $ EffectMismatch e1 e2 

-- Check compatibility of two effect bounds.
checkBoundCompatibility :: Bound -> Bound -> Bound -> Bound -> Bool
checkBoundCompatibility lowerAnn upperAnn lowerInf upperInf =
  isBoundSubset lowerAnn upperInf && isBoundSubset lowerInf upperAnn

-- Helper to check if b1 is a subset of b2, considering Omega.
isBoundSubset :: Bound -> Bound -> Bool
isBoundSubset _ BoundOmega = True 
isBoundSubset BoundOmega _ = False 
isBoundSubset (BoundFinite s1) (BoundFinite s2) = s1 `Set.isSubsetOf` s2

-- Simple helper to combine multiple EffectK values.
-- This is a placeholder for more sophisticated effect combination/unification.
-- It prioritizes effect variables if present, otherwise unions concrete effects.
-- More complex scenarios (e.g., combining bounds, or concrete + variable) are simplified.
combineManyEffectKs :: [EffectK] -> EffectK
combineManyEffectKs eks =
  let concreteEffects = Set.unions [s | EffectsConcrete s <- eks]
      effectVars = [ev | EffectsVar ev <- eks]
      -- Ignores EffectsBound for simplicity in this initial version
  in if not (null effectVars)
     then EffectsVar (head effectVars) -- Take the first variable if any (very naive)
     else EffectsConcrete concreteEffects

-- Counter for generating fresh type variables
type FreshCounter = Int

-- Function to generate a fresh type variable
freshTypeVar :: TypeCheck TypeVar
freshTypeVar = do
  currentIdx <- gets tsFreshCounter
  modify $ \st -> st { tsFreshCounter = currentIdx + 1 }
  return $ TV (Text.pack $ "tv" ++ show currentIdx)

-- Helper function for Branch case
-- tcBranchFunType is the TCType variable representing the overall branch function (e.g., alfa -> beta with eff_gamma)
processClause :: TCType -> Clause -> TypeCheck [Constraint]
processClause tcBranchFunType (Clause pat body) = do
  -- For the pattern (parameter of this specific branch case)
  tvPat <- freshTypeVar
  let tcPatType = TCVar tvPat
  -- TODO: Richer pattern constraint generation (like in Lambda)
  (patternBindings, patConstraints) <- case pat of
    PVar varName -> return ([(varName, tcPatType)], [])
    PWildcard    -> return ([], [])
    _            -> throwError $ OtherUnificationError (Text.pack $ "Unsupported pattern in Branch clause: " ++ show pat)

  -- For the body (return value of this specific branch case)
  tvBody <- freshTypeVar
  let tcBodyType = TCVar tvBody
  (bodyConstraints, bodyEffects) <- withBindings patternBindings $ getUnifications body tcBodyType

  -- The branch function type (tcBranchFunType) must be unifiable with TCFun tcPatType tcBodyType bodyEffects
  -- This means the pattern's type must match the function's arg type,
  -- the body's type must match the function's return type,
  -- and the body's effects must match the function's effects.
  let caseFunTypeConstraint = Constraint tcBranchFunType (TCFun tcPatType tcBodyType bodyEffects)
  
  return $ caseFunTypeConstraint : patConstraints ++ bodyConstraints

-- Generate constraints for an expression
getUnifications :: Expr -> TCType -> TypeCheck ([Constraint], EffectK)
getUnifications expr exprTy = case expr of
  Number _ -> return ([(exprTy, TCNum)], EffectsConcrete Set.empty)
  Bool _ -> return ([(exprTy, TCBool)], EffectsConcrete Set.empty)
  Var name -> do
    env <- getEnv -- This correctly fetches Map Text TCType
    case Map.lookup name env of
      Just tcTypeFromEnv -> 
        -- The type found in the environment is already a TCType.
        -- Generate a constraint between the expression's type variable (exprTy)
        -- and the type found in the environment.
        return ([(exprTy, tcTypeFromEnv)], EffectsConcrete Set.empty)
      Nothing -> 
        -- Use the UnboundVariable constructor from Language.Memento.Syntax.TypeError
        throwError $ UnboundVariable name 

  Lambda pat mMaybeTypeAnn body -> do
    tvPat <- freshTypeVar
    let tcPatType = TCVar tvPat -- Type variable for the pattern/parameter

    -- Generate bindings from the pattern and initial pattern constraints
    -- TODO: This pattern handling is still very basic.
    -- A proper implementation would involve a separate function like `checkPatternAndGetBindingsTC`
    -- similar to the existing `checkPatternAndGetBindings` but for TCTypes.
    (patternBindings, basePatConstraints) <- case pat of
      PVar varName -> return ([(varName, tcPatType)], [])
      PWildcard    -> return ([], [])
      -- Other patterns (PConstructor, PNumber, PBool, PTuple) are not yet supported here.
      _            -> throwError $ OtherUnificationError (Text.pack $ "Unsupported pattern in Lambda: " ++ show pat)

    -- Handle optional type annotation on the lambda parameter
    let annotationConstraints = case mMaybeTypeAnn of
          Just syntaxTypeAnn ->
            let tcAnnotationType = typeToTCType syntaxTypeAnn -- Replaced placeholder
            in [Constraint tcPatType tcAnnotationType]
          Nothing -> []
    
    let allPatternConstraints = basePatConstraints ++ annotationConstraints

    -- Generate a type variable for the lambda body
    tvBody <- freshTypeVar
    let tcBodyType = TCVar tvBody
    
    -- Get constraints from the body in an environment extended with pattern bindings
    (bodyConstraints, bodyEffects) <- withBindings patternBindings $ getUnifications body tcBodyType 
    
    -- The type of the lambda is a function type
    let lambdaFunType = TCFun tcPatType tcBodyType bodyEffects
    
    -- Combine all constraints
    let totalConstraints = (exprTy, lambdaFunType) : bodyConstraints ++ allPatternConstraints
           
    -- Lambda expressions themselves are effect-free; their bodies carry effects (captured in lambdaFunType)
    return (totalConstraints, EffectsConcrete Set.empty)

  Apply fun arg -> do
    tvFun <- freshTypeVar
    let tcFunType = TCVar tvFun
    tvArg <- freshTypeVar
    let tcArgType = TCVar tvArg
    
    (funConstraints, funEffects) <- getUnifications fun tcFunType
    (argConstraints, argEffects) <- getUnifications arg tcArgType
    
    tvRet <- freshTypeVar
    let tcRetType = TCVar tvRet
    
    evApp <- EV . Text.pack . show <$> freshTypeVar 
    let appEffects = EffectsVar evApp

    let expectedFunType = TCFun tcArgType tcRetType appEffects 
    
    let constraints = [ (tcFunType, expectedFunType) 
                      , (exprTy, tcRetType)        
                      ] ++ funConstraints ++ argConstraints
                      
    return (constraints, combineManyEffectKs [funEffects, argEffects, appEffects]) -- NEW

  BinOp op e1 e2 -> do
    tvE1 <- freshTypeVar; let tcE1 = TCVar tvE1
    tvE2 <- freshTypeVar; let tcE2 = TCVar tvE2
    (e1Constraints, e1Effects) <- getUnifications e1 tcE1
    (e2Constraints, e2Effects) <- getUnifications e2 tcE2
    
    let combinedEffects = combineManyEffectKs [e1Effects, e2Effects]

    let (opConstraints, resultType) = case op of
          Add -> ([(tcE1, TCNum), (tcE2, TCNum)], TCNum)
          Sub -> ([(tcE1, TCNum), (tcE2, TCNum)], TCNum)
          Mul -> ([(tcE1, TCNum), (tcE2, TCNum)], TCNum)
          Div -> ([(tcE1, TCNum), (tcE2, TCNum)], TCNum)
          Eq  -> ([(tcE1, tcE2)], TCBool) 
          Lt  -> ([(tcE1, TCNum), (tcE2, TCNum)], TCBool)
          Gt  -> ([(tcE1, TCNum), (tcE2, TCNum)], TCBool)
          
    let constraints = (exprTy, resultType) : opConstraints ++ e1Constraints ++ e2Constraints
    return (constraints, combinedEffects)

  If cond thenE elseE -> do
    tvCond <- freshTypeVar; let tcCond = TCVar tvCond
    tvThen <- freshTypeVar; let tcThen = TCVar tvThen
    tvElse <- freshTypeVar; let tcElse = TCVar tvElse

    (condConstraints, condEffects) <- getUnifications cond tcCond
    (thenConstraints, thenEffects) <- getUnifications thenE tcThen
    (elseConstraints, elseEffects) <- getUnifications elseE tcElse

    let combinedEffects = combineManyEffectKs [condEffects, thenEffects, elseEffects]

    let constraints = [ (tcCond, TCBool) 
                      , (tcThen, tcElse) 
                      , (exprTy, tcThen) 
                      ] ++ condConstraints ++ thenConstraints ++ elseConstraints
    return (constraints, combinedEffects)

  Branch mOverallTypeAnn clauses -> do
    tvExprFun <- freshTypeVar -- Type variable for the branch expression itself (which is a function)
    let tcExprFunType = TCVar tvExprFun 
    
    allClauseConstraints <- fmap concat $ mapM (processClause tcExprFunType) clauses
    
    -- Constraint for the overall expression type
    let exprConstraint = (exprTy, tcExprFunType)

    -- Handle overall type annotation if present
    annotationConstraints <- case mOverallTypeAnn of
      Just overallAnn -> do
        let tcOverallAnn = typeToTCType overallAnn
        -- Check if tcOverallAnn is a function type, if not, this is an error.
        -- For now, assume it is, or that unifyPure will catch it.
        return [(tcExprFunType, tcOverallAnn)]
      Nothing -> return []

    return (exprConstraint : annotationConstraints ++ allClauseConstraints, EffectsConcrete Set.empty)

  -- Placeholder for other expressions (keep this last)
  _ -> return ([(exprTy, TCVar (TV "todo_type_for_unimplemented_expr"))], EffectsVar (EV "todo_effect_for_unimplemented_expr"))

-- Alias for getUnifications
generateConstraints :: Expr -> TCType -> TypeCheck ([Constraint], EffectK)
generateConstraints = getUnifications

-- Constraint Solver
solve :: [Constraint] -> Except UnificationError Substitution
solve [] = return emptySubst
solve ((Constraint t1 t2):cs) = do
  s1 <- unifyPure t1 t2 `catchError` mapError (t1,t2)
  s2 <- solve (map (applyConstraint s1) cs)
  return (s2 `composeSubst` s1)

-- Helper to apply substitution to a constraint
applyConstraint :: Substitution -> Constraint -> Constraint
applyConstraint s (Constraint t1 t2) = Constraint (applySubst s t1) (applySubst s t2)

-- Helper to wrap unification errors with context
mapError :: (TCType, TCType) -> UnificationError -> Except UnificationError Substitution
mapError (t1,t2) err = throwError err -- Initial simple version.

-- Main Hindley-Milner inference function
inferHM :: Expr -> TypeCheck (TCType, EffectK)
inferHM expr = do
  -- 1. Generate a fresh type variable for the expression's overall type
  tvExpr <- freshTypeVar
  let tcExprType = TCVar tvExpr

  -- 2. Get constraints and initial effects from the expression
  (constraints, initialEffects) <- getUnifications expr tcExprType

  -- 3. Solve the constraints to get a substitution
  substitution <- liftEither $ solve constraints `catchError` mapConstraintError
  
  -- 4. Apply the substitution to the expression's type variable and the effects
  let inferredType = applySubst substitution tcExprType
  let inferredEffects = applySubstEffects substitution initialEffects

  return (inferredType, inferredEffects)

-- Specific error mapping for errors arising from the solve function
mapConstraintError :: UnificationError -> Except UnificationError Substitution
mapConstraintError err = throwError $ OtherUnificationError (Text.pack $ "Constraint solving failed: " ++ show err)

-- Unification Constraint
data Constraint = Constraint TCType TCType deriving (Show, Eq, Ord, Generic)

newtype Substitution = Substitution (Map TypeVar TCType) deriving (Show, Eq, Ord, Generic)

-- Unification Error
data UnificationError
  = OccursCheckFailed TypeVar TCType
  | TypeMismatch TCType TCType
  | EffectMismatch EffectK EffectK
  | CannotUnifyBound Bound Bound 
  | OtherUnificationError Text
  deriving (Show, Eq, Generic)
