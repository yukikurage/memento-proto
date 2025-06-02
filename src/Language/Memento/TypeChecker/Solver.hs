{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-
Type Solver

first, Constraint -> Bounds

solve : Bounds -> Substitutions
  go : (Bounds, Substitutions) -> (Bounds, Substitutions)

  repeat `go` until `null Bounds` (or max iterations)
-}
module Language.Memento.TypeChecker.Solver where

{-
import Control.Monad (foldM, forM_, unless, void, zipWithM)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Language.Memento.Syntax (Effect, EffectWithMetadata (EffectWithMetadata), EffectsWithMetadata (EffectsWithMetadata), Type (TAlgebraicData, TBool, TFunction, THandler, TNumber, TTuple), TypeVariable (TypeVariable), TypeVariableWithMetadata (TypeVariableWithMetadata), TypeWithMetadata (TypeWithMetadata), Variable (Variable), VariableWithMetadata (VariableWithMetadata))

{-
Types for Effects
-}

newtype UnsolvedEffectsVariable = UnsolvedEffectsVariable Text deriving (Show, Eq, Generic, Ord)

data SolvedEffects
  = SESet (Set Effect)
  | SEFull
  deriving (Show, Eq, Generic)

data UnsolvedEffects
  = UEVar UnsolvedEffectsVariable
  | UESet (Set Effect)
  | UEIntersection UnsolvedEffects UnsolvedEffects
  | UEUnion UnsolvedEffects UnsolvedEffects
  | UEFull
  deriving (Show, Eq, Generic)

seUnion :: SolvedEffects -> SolvedEffects -> SolvedEffects
seUnion (SESet e1) (SESet e2) = SESet $ Set.union e1 e2
seUnion (SESet e1) SEFull = SEFull
seUnion SEFull (SESet e2) = SEFull
seUnion SEFull SEFull = SEFull

seIntersection :: SolvedEffects -> SolvedEffects -> SolvedEffects
seIntersection (SESet e1) (SESet e2) = SESet $ Set.intersection e1 e2
seIntersection (SESet e1) SEFull = SESet e1
seIntersection SEFull (SESet e2) = SESet e2
seIntersection SEFull SEFull = SEFull

{-
Types for Types
-}

data SolvedType
  = STNumber -- 数値型
  | STBool -- 真偽値型
  | STHandler (SolvedType, SolvedEffects) (SolvedType, SolvedEffects) -- ハンドラ型 (引数の型 & 消費するエフェクト -> 戻り値の型 & 生成するエフェクト)
  | STFunction SolvedType (SolvedType, SolvedEffects)
  | STAlgebraicData TypeVariable -- 代数的データ型
  | STTuple ([] SolvedType) -- タプル型
  | STBottom -- 全ての部分型
  | STTop -- 全ての上位型
  deriving (Show, Eq, Generic)

newtype UnsolvedTypeVariable = UnsolvedTypeVariable Text deriving (Show, Eq, Generic, Ord)

-- | 型の定義
data UnsolvedType
  = UTVar UnsolvedTypeVariable -- 未解決の型変数
  | UTNum -- 数値型
  | UTBool -- 真偽値型
  | UTHandler (UnsolvedType, UnsolvedEffects) (UnsolvedType, UnsolvedEffects) -- ハンドラ型 (引数の型 & 消費するエフェクト -> 戻り値の型 & 生成するエフェクト)
  | UTFunction UnsolvedType (UnsolvedType, UnsolvedEffects)
  | UTAlgebraicData TypeVariable -- 代数的データ型
  | UTTuple ([] UnsolvedType) -- タプル型
  | UTBottom -- 全ての部分型
  | UTTop -- 全ての上位型
  deriving (Show, Eq, Generic)

{-
Types for Constraints

data LatticeAlgebra a
  = LAUnion (LatticeAlgebra a) (LatticeAlgebra a)
  | LAIntersection (LatticeAlgebra a) (LatticeAlgebra a)
  | LABottom
  | LATop
  | LAVar a
  deriving (Show, Eq, Generic)

computeLAM ::
  (Monad m) =>
  (b -> b -> m b) -> -- Union
  b -> -- Bottom
  (b -> b -> m b) -> -- Intersection
  b -> -- Top
  (a -> m b) -> -- Var
  LatticeAlgebra a ->
  m b
computeLAM uni bot inter top var = \case
  LAUnion a b -> do
    a' <- computeLAM uni bot inter top var a
    b' <- computeLAM uni bot inter top var b
    uni a' b'
  LAIntersection a b -> do
    a' <- computeLAM uni bot inter top var a
    b' <- computeLAM uni bot inter top var b
    inter a' b'
  LABottom -> return bot
  LATop -> return top
  LAVar a -> var a

computeLA ::
  (b -> b -> b) -> -- Union
  b -> -- Bottom
  (b -> b -> b) -> -- Intersection
  b -> -- Top
  (a -> b) -> -- Var
  LatticeAlgebra a ->
  b
computeLA uni bot inter top f = \case
  LAUnion a b -> uni (computeLA uni bot inter top f a) (computeLA uni bot inter top f b)
  LAIntersection a b -> inter (computeLA uni bot inter top f a) (computeLA uni bot inter top f b)
  LABottom -> bot
  LATop -> top
  LAVar a -> f a

-}

data ConstraintOperator
  = COEqual
  | COGreaterThanOrEqual
  | COLessThanOrEqual
  deriving (Show, Eq, Generic)

type TypeConstraint = (UnsolvedTypeVariable, UnsolvedType, ConstraintOperator)

type EffectConstraint = (UnsolvedEffectsVariable, UnsolvedEffects, ConstraintOperator)

data Constraints = Constraints
  { typeConstraints :: [] TypeConstraint -- And 結合
  , effectConstraints :: [] EffectConstraint -- And 結合
  }
  deriving (Show, Eq)

{-
Types for Bounds
-}

type TypeBound = ([] UnsolvedType, [] UnsolvedType)

type EffectBound = ([] UnsolvedEffects, [] UnsolvedEffects)

data Bounds = Bounds
  { typeBounds :: Map UnsolvedTypeVariable TypeBound
  , effectBounds :: Map UnsolvedEffectsVariable EffectBound
  }
  deriving (Show, Eq)

{-
Types for Substitutions
-}

data Substitution = Substitution
  { typeSubstitution :: Map UnsolvedTypeVariable SolvedType
  , effectSubstitution :: Map UnsolvedEffectsVariable SolvedEffects
  }
  deriving (Show, Eq)

{-
Types for Preferences
-}

data Preference = Minimize | Maximize deriving (Show, Eq, Generic)

dualPreference :: Preference -> Preference
dualPreference Minimize = Maximize
dualPreference Maximize = Minimize

multiplyPreference :: Preference -> Preference -> Preference
multiplyPreference Minimize Minimize = Maximize
multiplyPreference Maximize Maximize = Maximize
multiplyPreference Minimize Maximize = Minimize
multiplyPreference Maximize Minimize = Minimize

data Preferences = Preferences
  { typePreferences :: Map UnsolvedTypeVariable Preference
  , effectPreferences :: Map UnsolvedEffectsVariable Preference
  }

{-
Monad
-}

data TypeSolverState = TypeSolverState
  { substitution :: Substitution
  , preferences :: Preferences
  }

initialTypeSolverState :: Preferences -> TypeSolverState
initialTypeSolverState prefs =
  TypeSolverState
    { substitution = Substitution Map.empty Map.empty
    , preferences = prefs
    }

data TypeSolverError
  = -- マッチ不可
    TypeMismatch UnsolvedType UnsolvedType
  | -- 境界内の型不在
    TypeNotInBounds SolvedType SolvedType
  | -- 境界内のエフェクト不在
    EffectNotInBounds SolvedEffects SolvedEffects
  | -- 最大イテレーション数を超えた
    MaxIterationExceeded Int
  deriving (Show, Eq)

type TypeSolverM a = ExceptT TypeSolverError (State TypeSolverState) a

{-
functions
-}

parseUT :: UnsolvedType -> Maybe SolvedType
parseUT = \case
  UTVar v -> Nothing
  UTNum -> Just STNumber
  UTBool -> Just STBool
  UTHandler (t, e) (t', e') -> do
    t <- parseUT t
    t' <- parseUT t'
    e <- parseUE e
    e' <- parseUE e'
    return $ STHandler (t, e) (t', e')
  UTFunction t (t', e') -> do
    t <- parseUT t
    t' <- parseUT t'
    e' <- parseUE e'
    return $ STFunction t (t', e')
  UTAlgebraicData v -> Just $ STAlgebraicData v
  UTTuple ts -> do
    ts' <- mapM parseUT ts
    return $ STTuple ts'
  UTBottom -> Just STBottom
  UTTop -> Just STTop

parseUE :: UnsolvedEffects -> Maybe SolvedEffects
parseUE = \case
  UEVar v -> Nothing
  UESet e -> Just $ SESet e
  UEFull -> Just SEFull
  UEIntersection e1 e2 -> do
    e1' <- parseUE e1
    e2' <- parseUE e2
    return $ seIntersection e1' e2'
  UEUnion e1 e2 -> do
    e1' <- parseUE e1
    e2' <- parseUE e2
    return $ seUnion e1' e2'

serializeST :: SolvedType -> UnsolvedType
serializeST = \case
  STNumber -> UTNum
  STBool -> UTBool
  STHandler (t, e) (t', e') -> UTHandler (serializeST t, serializeSE e) (serializeST t', serializeSE e')
  STFunction t (t', e') -> UTFunction (serializeST t) (serializeST t', serializeSE e')
  STAlgebraicData v -> UTAlgebraicData v
  STTuple ts -> UTTuple $ map serializeST ts
  STBottom -> UTBottom
  STTop -> UTTop

serializeSE :: SolvedEffects -> UnsolvedEffects
serializeSE = \case
  SESet e -> UESet e
  SEFull -> UEFull

typeToSolvedType :: TypeWithMetadata -> SolvedType
typeToSolvedType (TypeWithMetadata typ _) = case typ of
  TNumber -> STNumber
  TBool -> STBool
  THandler (argT, effT) (retT, effR) -> STHandler (typeToSolvedType argT, typeToSolvedEffects effT) (typeToSolvedType retT, typeToSolvedEffects effR)
  TFunction argT (retT, effT) -> STFunction (typeToSolvedType argT) (typeToSolvedType retT, typeToSolvedEffects effT)
  TAlgebraicData (TypeVariableWithMetadata tv _) -> STAlgebraicData tv
  TTuple ts -> STTuple $ map typeToSolvedType ts

typeToSolvedEffects :: EffectsWithMetadata -> SolvedEffects
typeToSolvedEffects (EffectsWithMetadata effs _) = SESet $ Set.map (\(EffectWithMetadata eff _) -> eff) effs

unionST :: SolvedType -> SolvedType -> TypeSolverM SolvedType
unionST t1 t2 = case (t1, t2) of
  (STNumber, STNumber) -> return STNumber
  (STBool, STBool) -> return STBool
  (STHandler (t1, e1) (t2, e2), STHandler (t3, e3) (t4, e4)) -> do
    t1' <- intersectST t1 t3 -- Contravariance
    t2' <- unionST t2 t4
    e1' <- intersectSE e1 e3 -- Contravariance
    e2' <- unionSE e2 e4
    return $ STHandler (t1', e1') (t2', e2')
  (STFunction t1 (t2, e1), STFunction t3 (t4, e3)) -> do
    t1' <- intersectST t1 t3
    t2' <- unionST t2 t4
    e1' <- unionSE e1 e3
    return $ STFunction t1' (t2', e1')
  (STAlgebraicData name1, STAlgebraicData name2) ->
    if name1 == name2 then return $ STAlgebraicData name1 else throwError $ TypeMismatch (serializeST t1) (serializeST t2)
  (STTuple t1s, STTuple t2s) -> do
    unless (length t1s == length t2s) $ throwError $ TypeMismatch (serializeST t1) (serializeST t2)
    ts <- zipWithM unionST t1s t2s
    return $ STTuple ts
  (STBottom, _) -> return t2 -- Bottom is the bottom of the lattice
  (_, STBottom) -> return t1 -- Bottom is the bottom of the lattice
  (STTop, _) -> return STTop -- Top is the top of the lattice
  (_, STTop) -> return STTop -- Top is the top of the lattice
  _ -> throwError $ TypeMismatch (serializeST t1) (serializeST t2)

unionSE :: SolvedEffects -> SolvedEffects -> TypeSolverM SolvedEffects
unionSE e1 e2 = case (e1, e2) of
  (SESet e1, SESet e2) -> return $ SESet $ Set.union e1 e2
  (SESet e1, SEFull) -> return SEFull
  (SEFull, SESet e2) -> return SEFull
  (SEFull, SEFull) -> return SEFull

intersectST :: SolvedType -> SolvedType -> TypeSolverM SolvedType
intersectST t1 t2 = case (t1, t2) of
  (STNumber, STNumber) -> return STNumber
  (STBool, STBool) -> return STBool
  (STHandler (t1, e1) (t2, e2), STHandler (t3, e3) (t4, e4)) -> do
    t1' <- unionST t1 t3 -- Contravariance
    t2' <- intersectST t2 t4
    e1' <- intersectSE e1 e3 -- Contravariance
    e2' <- unionSE e2 e4
    return $ STHandler (t1', e1') (t2', e2')
  (STBottom, _) -> return STBottom -- Bottom is the bottom of the lattice
  (_, STBottom) -> return STBottom -- Bottom is the bottom of the lattice
  (STTop, _) -> return t2 -- Top is the top of the lattice
  (_, STTop) -> return t1 -- Top is the top of the lattice
  _ -> throwError $ TypeMismatch (serializeST t1) (serializeST t2)

intersectSE :: SolvedEffects -> SolvedEffects -> TypeSolverM SolvedEffects
intersectSE e1 e2 = case (e1, e2) of
  (SESet e1, SESet e2) -> return $ SESet $ Set.intersection e1 e2
  (SESet e1, SEFull) -> return $ SESet e1
  (SEFull, SESet e2) -> return $ SESet e2
  (SEFull, SEFull) -> return SEFull

parseTBounds :: TypeBound -> Maybe ([] SolvedType, [] SolvedType)
parseTBounds (ts1, ts2) = do
  ts1' <- mapM parseUT ts1
  ts2' <- mapM parseUT ts2
  return (ts1', ts2')

getIntersectedTBounds :: ([] SolvedType, [] SolvedType) -> TypeSolverM (SolvedType, SolvedType)
getIntersectedTBounds (ts1, ts2) = do
  ts1' <- foldM unionST STBottom ts1 -- Contravariance
  ts2' <- foldM intersectST STTop ts2 -- Covariance
  return (ts1', ts2')

parseEBounds :: EffectBound -> Maybe ([] SolvedEffects, [] SolvedEffects)
parseEBounds (es1, es2) = do
  es1' <- mapM parseUE es1
  es2' <- mapM parseUE es2
  return (es1', es2')

getIntersectedEBounds :: ([] SolvedEffects, [] SolvedEffects) -> TypeSolverM (SolvedEffects, SolvedEffects)
getIntersectedEBounds (es1, es2) = do
  es1' <- foldM unionSE (SESet Set.empty) es1 -- Contravariance
  es2' <- foldM intersectSE SEFull es2 -- Covariance
  return (es1', es2')

constraintsToBounds :: Constraints -> Bounds
constraintsToBounds (Constraints typeConstraints effectConstraints) =
  Bounds
    { typeBounds =
        foldr
          ( \(v, t, op) acc -> case op of
              COEqual ->
                Map.alter
                  ( \case
                      Nothing -> Just ([t], [t])
                      Just (tsMin, tsMax) -> Just (t : tsMin, t : tsMax)
                  )
                  v
                  acc
              COGreaterThanOrEqual ->
                Map.alter
                  ( \case
                      Nothing -> Just ([t], [])
                      Just (tsMin, tsMax) -> Just (t : tsMin, tsMax)
                  )
                  v
                  acc
              COLessThanOrEqual ->
                Map.alter
                  ( \case
                      Nothing -> Just ([], [t])
                      Just (tsMin, tsMax) -> Just (tsMin, t : tsMax)
                  )
                  v
                  acc
          )
          Map.empty
          typeConstraints
    , effectBounds =
        foldr
          ( \(v, e, op) acc -> case op of
              COEqual ->
                Map.alter
                  ( \case
                      Nothing -> Just ([e], [e])
                      Just (esMin, esMax) -> Just (e : esMin, e : esMax)
                  )
                  v
                  acc
              COGreaterThanOrEqual ->
                Map.alter
                  ( \case
                      Nothing -> Just ([e], [])
                      Just (esMin, esMax) -> Just (e : esMin, esMax)
                  )
                  v
                  acc
              COLessThanOrEqual ->
                Map.alter
                  ( \case
                      Nothing -> Just ([], [e])
                      Just (esMin, esMax) -> Just (esMin, e : esMax)
                  )
                  v
                  acc
          )
          Map.empty
          effectConstraints
    }

-- Whether t1 is a subtype of t2
isSubtypeST :: SolvedType -> SolvedType -> TypeSolverM Bool
isSubtypeST t1 t2 = case (t1, t2) of
  (STNumber, STNumber) -> return True
  (STBool, STBool) -> return True
  (STHandler (t1, e1) (t2, e2), STHandler (t3, e3) (t4, e4)) -> do
    tLeft <- isSubtypeST t3 t1 -- Contravariance
    eLeft <- isSubtypeSE e3 e1 -- Contravariance
    tRight <- isSubtypeST t2 t4 -- Covariance
    eRight <- isSubtypeSE e2 e4 -- Covariance
    return $ tLeft && eLeft && tRight && eRight
  (STFunction t1 (t2, e2), STFunction t3 (t4, e4)) -> do
    tLeft <- isSubtypeST t3 t1 -- Contravariance
    tRight <- isSubtypeST t2 t4 -- Covariance
    eRight <- isSubtypeSE e2 e4 -- Covariance
    return $ tLeft && tRight && eRight
  (STAlgebraicData name1, STAlgebraicData name2) -> return $ name1 == name2
  (STTuple t1s, STTuple t2s) -> do
    unless (length t1s == length t2s) $ throwError $ TypeMismatch (serializeST t1) (serializeST t2)
    bs <- zipWithM isSubtypeST t1s t2s
    return $ and bs
  (STBottom, _) -> return True
  (_, STBottom) -> return False
  (STTop, _) -> return False
  (_, STTop) -> return True
  _ -> throwError $ TypeMismatch (serializeST t1) (serializeST t2)

isSubtypeSE :: SolvedEffects -> SolvedEffects -> TypeSolverM Bool
isSubtypeSE e1 e2 = case (e1, e2) of
  (SESet e1, SESet e2) -> return $ Set.isSubsetOf e1 e2
  (SEFull, SESet e2) -> return False
  (_, SEFull) -> return True

getPreferSubstT :: UnsolvedTypeVariable -> (SolvedType, SolvedType) -> TypeSolverM SolvedType
getPreferSubstT v (t1, t2) = do
  isSubT <- isSubtypeST t1 t2
  unless isSubT $ throwError $ TypeNotInBounds t1 t2
  tPrefers <- gets (typePreferences . preferences)
  case Map.lookup v tPrefers of
    Just Minimize -> return t1
    Just Maximize -> return t2
    Nothing -> return t1 -- Default : Minimize

addSubstT :: UnsolvedTypeVariable -> SolvedType -> TypeSolverM ()
addSubstT v t = do
  modify $
    \s ->
      s{substitution = Substitution (Map.insert v t (typeSubstitution $ substitution s)) (effectSubstitution $ substitution s)}

getPreferSubstE :: UnsolvedEffectsVariable -> (SolvedEffects, SolvedEffects) -> TypeSolverM SolvedEffects
getPreferSubstE v (e1, e2) = do
  isSubE <- isSubtypeSE e1 e2
  unless isSubE $ throwError $ EffectNotInBounds e1 e2
  ePrefers <- gets (effectPreferences . preferences)
  case Map.lookup v ePrefers of
    Just Minimize -> return e1
    Just Maximize -> return e2
    Nothing -> return e1 -- Default : Minimize

addSubstE :: UnsolvedEffectsVariable -> SolvedEffects -> TypeSolverM ()
addSubstE v e = do
  modify $
    \s ->
      s{substitution = Substitution (typeSubstitution $ substitution s) (Map.insert v e (effectSubstitution $ substitution s))}

substituteT :: UnsolvedType -> TypeSolverM UnsolvedType
substituteT = \case
  UTNum -> return UTNum
  UTBool -> return UTBool
  UTVar v -> do
    substs <- gets (typeSubstitution . substitution)
    case Map.lookup v substs of
      Just t -> return $ (serializeST t)
      Nothing -> return $ UTVar v
  UTHandler (t, e) (t', e') -> do
    t' <- substituteT t
    e' <- substituteE e
    t'' <- substituteT t'
    e'' <- substituteE e'
    return $ UTHandler (t'', e'') (t', e') -- ここでは serialize しない
  UTFunction t (t', e') -> do
    t' <- substituteT t
    t'' <- substituteT t'
    e'' <- substituteE e'
    return $ UTFunction t'' (t', e'')
  UTAlgebraicData v -> return $ UTAlgebraicData v
  UTTuple ts -> do
    ts' <- mapM substituteT ts
    return $ UTTuple ts'
  UTBottom -> return UTBottom
  UTTop -> return UTTop

substituteE :: UnsolvedEffects -> TypeSolverM UnsolvedEffects
substituteE = \case
  UEVar v -> do
    substs <- gets (effectSubstitution . substitution)
    case Map.lookup v substs of
      Just e -> return $ serializeSE e
      Nothing -> return $ UEVar v
  UESet e -> return $ UESet e
  UEFull -> return UEFull
  UEIntersection e1 e2 -> do
    e1' <- substituteE e1
    e2' <- substituteE e2
    return $ UEIntersection e1' e2'
  UEUnion e1 e2 -> do
    e1' <- substituteE e1
    e2' <- substituteE e2
    return $ UEUnion e1' e2'

typeVariablesT :: UnsolvedType -> [UnsolvedTypeVariable]
typeVariablesT = \case
  UTVar v -> [v]
  UTHandler (t, e) (t', e') -> typeVariablesT t ++ typeVariablesT t'
  UTFunction t (t', e') -> typeVariablesT t ++ typeVariablesT t'
  UTTuple ts -> concatMap typeVariablesT ts
  _ -> []

typeVariablesE :: UnsolvedEffects -> [UnsolvedEffectsVariable]
typeVariablesE = \case
  UEVar v -> [v]
  UESet es -> []
  UEFull -> []
  UEIntersection e1 e2 -> typeVariablesE e1 ++ typeVariablesE e2
  UEUnion e1 e2 -> typeVariablesE e1 ++ typeVariablesE e2

addFreeConstraint :: Constraints -> Constraints
addFreeConstraint (Constraints typeConstraints effectConstraints) = do
  let
    allTypeVariables = Set.fromList $ concatMap (\(v, t, _) -> v : typeVariablesT t) typeConstraints
    allEffectVariables = Set.fromList $ concatMap (\(v, e, _) -> v : typeVariablesE e) effectConstraints
    bindedTypeVariables = Set.fromList $ map (\(v, t, _) -> v) typeConstraints
    bindedEffectVariables = Set.fromList $ map (\(v, e, _) -> v) effectConstraints
    freeTypeVariables = allTypeVariables `Set.difference` bindedTypeVariables
    freeEffectVariables = allEffectVariables `Set.difference` bindedEffectVariables
    freeTypeConstraintsLessthan = map (,UTTop,COLessThanOrEqual) $ Set.toList freeTypeVariables
    freeTypeConstraintsGreaterthan = map (,UTBottom,COGreaterThanOrEqual) $ Set.toList freeTypeVariables
    freeEffectConstraintsLessthan = map (,UEFull,COLessThanOrEqual) $ Set.toList freeEffectVariables
    freeEffectConstraintsGreaterthan = map (,UESet Set.empty,COGreaterThanOrEqual) $ Set.toList freeEffectVariables
    newConstraints =
      Constraints
        { typeConstraints =
            typeConstraints
              ++ freeTypeConstraintsLessthan
              ++ freeTypeConstraintsGreaterthan
        , effectConstraints =
            effectConstraints
              ++ freeEffectConstraintsLessthan
              ++ freeEffectConstraintsGreaterthan
        }
  newConstraints

solve :: Int -> Constraints -> Preferences -> Either TypeSolverError ()
solve maxItr constraints prefs = do
  let bounds = constraintsToBounds $ addFreeConstraint constraints
  void $ evalState (runExceptT $ go maxItr bounds) (initialTypeSolverState prefs)
 where
  go :: Int -> Bounds -> TypeSolverM Bounds
  go 0 bounds = throwError $ MaxIterationExceeded maxItr
  go _ bounds | Map.null (typeBounds bounds) && Map.null (effectBounds bounds) = return bounds
  go n (Bounds tBounds eBounds) = do
    traceM $ "↓\n\n"
    traceM $ "Iteration " ++ show n ++ ":\n"
    traceM $ "Type & effect bounds:\n"
    forM_ (Map.toList tBounds) $ \(v, (tsMin, tsMax)) -> do
      traceM $ show tsMin ++ " <: " ++ show v ++ " <: " ++ show tsMax
    forM_ (Map.toList eBounds) $ \(v, (esMin, esMax)) -> do
      traceM $ show esMin ++ " <: " ++ show v ++ " <: " ++ show esMax
    traceM "\n\n"
    -- Bound の中で parse できるものを列挙
    let
      parsedBoundsT = Map.mapMaybe (\(tsMin, tsMax) -> parseTBounds (tsMin, tsMax)) tBounds
      parsedBoundsE = Map.mapMaybe (\(esMin, esMax) -> parseEBounds (esMin, esMax)) eBounds
    -- 境界を intersect してひとつにする
    intersectedBoundsT <- mapM getIntersectedTBounds parsedBoundsT
    intersectedBoundsE <- mapM getIntersectedEBounds parsedBoundsE
    -- parse した bound で prefer なものを選ぶ
    preferTs <- mapM (\(v, (tsMin, tsMax)) -> (v,) <$> getPreferSubstT v (tsMin, tsMax)) (Map.toList intersectedBoundsT)
    preferEs <- mapM (\(v, (esMin, esMax)) -> (v,) <$> getPreferSubstE v (esMin, esMax)) (Map.toList intersectedBoundsE)
    -- 選んだ bound を addSubst する & bounds から削除して再帰
    forM_ preferTs (uncurry addSubstT)
    forM_ preferEs (uncurry addSubstE)
    traceM "New substitutions:\n"
    forM_ preferTs $ \(v, t) -> do
      traceM $ show v ++ " -> " ++ show t
    forM_ preferEs $ \(v, e) -> do
      traceM $ show v ++ " -> " ++ show e
    traceM "\n\n"
    -- Bound を更新 & Subst に移動したものは削除
    newTBounds <-
      traverse
        ( \(tsMins, tsMaxs) -> do
            substedMins <- mapM substituteT tsMins
            substedMaxs <- mapM substituteT tsMaxs
            return (substedMins, substedMaxs)
        )
        $ Map.filterWithKey (\k _ -> k `notElem` map fst preferTs) tBounds
    newEBounds <-
      traverse
        ( \(esMins, esMaxs) -> do
            substedMins <- mapM substituteE esMins
            substedMaxs <- mapM substituteE esMaxs
            return (substedMins, substedMaxs)
        )
        $ Map.filterWithKey (\k _ -> k `notElem` map fst preferEs) eBounds
    go (n - 1) (Bounds newTBounds newEBounds)

-}