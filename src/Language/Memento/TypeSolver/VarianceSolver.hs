{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.VarianceSolver
  ( VarianceExpr (..),
    VarianceSystem,
    generateVarianceEquations,
    solveVarianceSystem,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.TypeSolver.Types

-- | Variance expression representing how a type parameter appears
data VarianceExpr
  = VarE T.Text                -- ^ Variable reference (e.g., "List_T")
  | ConstE Variance            -- ^ Constant variance value
  | CombineE [VarianceExpr]    -- ^ Addition (+) - parameter appears multiple times
  | ComposeE [VarianceExpr]    -- ^ Multiplication (*) - parameter appears nested
  deriving (Eq, Show)

-- | System of variance equations mapping variable names to expressions
type VarianceSystem = Map.Map T.Text VarianceExpr

-- | Generate variance equations from data type definitions
generateVarianceEquations :: Map.Map T.Text TypeConstructorInfo -> Map.Map T.Text TypeScheme -> VarianceSystem
generateVarianceEquations typeCtors typeEnv = Map.fromList allEquations
  where
    -- Group constructors by their base type name
    groupedByType = groupConstructorsByType typeCtors
    
    -- Generate equations for each type
    allEquations = concatMap generateForType (Map.toList groupedByType)
    
    -- Extract base type name from constructor name (e.g., "TYPE_List" -> "List")
    getBaseName :: T.Text -> T.Text
    getBaseName name = case T.stripPrefix "TYPE_" name of
      Just base -> base
      Nothing -> name
    
    -- Group constructors by their type
    groupConstructorsByType :: Map.Map T.Text TypeConstructorInfo -> Map.Map T.Text [(T.Text, TypeConstructorInfo)]
    groupConstructorsByType ctors = 
      Map.foldrWithKey 
        (\ctorName info acc -> 
          let typeName = extractTypeName ctorName info
          in Map.insertWith (++) typeName [(ctorName, info)] acc
        ) 
        Map.empty 
        ctors
    
    -- Extract type name from constructor
    extractTypeName :: T.Text -> TypeConstructorInfo -> T.Text
    extractTypeName ctorName _ = getBaseName ctorName
    
    -- Get return type from a function type
    extractReturnType :: Type -> Type
    extractReturnType (TFunction _ ret) = extractReturnType ret
    extractReturnType t = t
    
    -- Generate equations for a single type
    generateForType :: (T.Text, [(T.Text, TypeConstructorInfo)]) -> [(T.Text, VarianceExpr)]
    generateForType (typeName, constructors) = 
      let typeParams = extractTypeParams constructors
      in map (generateForParam typeName constructors) typeParams
    
    -- Extract all type parameters from constructors
    extractTypeParams :: [(T.Text, TypeConstructorInfo)] -> [T.Text]
    extractTypeParams ctors = case ctors of
      [] -> []
      ((_, TypeConstructorInfo arity _ _):_) -> 
        -- Use T1, T2, etc. for now
        -- TODO: Extract actual parameter names from constructor types
        map (\i -> "T" <> T.pack (show i)) [1..arity]
    
    -- Generate equation for a single type parameter
    generateForParam :: T.Text -> [(T.Text, TypeConstructorInfo)] -> T.Text -> (T.Text, VarianceExpr)
    generateForParam typeName ctors paramName = 
      let varName = typeName <> "_" <> paramName
          -- Combine variance from all constructors
          ctorExprs = [analyzeParamInConstructor paramName info | (_, info) <- ctors]
          expr = case ctorExprs of
            [] -> ConstE Bivariant
            [single] -> single
            multiple -> CombineE multiple
      in (varName, expr)
    
    -- Analyze how a parameter is used in a constructor
    analyzeParamInConstructor :: T.Text -> TypeConstructorInfo -> VarianceExpr
    analyzeParamInConstructor paramName (TypeConstructorInfo _ _ ctorNames) =
      -- Look up constructor types from the type environment
      let ctorExprs = map (analyzeConstructor paramName) ctorNames
      in case ctorExprs of
        [] -> ConstE Bivariant
        [single] -> single
        multiple -> CombineE multiple
    
    -- Analyze a single constructor
    analyzeConstructor :: T.Text -> T.Text -> VarianceExpr
    analyzeConstructor paramName ctorName =
      case Map.lookup ctorName typeEnv of
        Just (TypeScheme generics ctorType) -> 
          -- The generics in TypeScheme are the actual parameter names used in the constructor
          -- We need to map our synthesized parameter name (T1, T2, etc.) to the actual generic name
          let paramIndex = read (T.unpack $ T.drop 1 paramName) - 1 -- Extract index from "T1" -> 0
              actualParamName = if paramIndex < length generics 
                                then generics !! paramIndex
                                else paramName -- Fallback
          in case extractConstructorArgTypes ctorType of
            Just argTypes -> 
              let argExprs = map (analyzeTypeVariance actualParamName) argTypes
              in case argExprs of
                [] -> ConstE Bivariant
                [single] -> single
                multiple -> CombineE multiple
            Nothing -> ConstE Bivariant
        Nothing -> ConstE Bivariant
    
    -- Extract argument types from a constructor function type
    extractConstructorArgTypes :: Type -> Maybe [Type]
    extractConstructorArgTypes (TFunction args _) = Just args
    extractConstructorArgTypes _ = Nothing

-- | Analyze variance of a type parameter in a type expression
analyzeTypeVariance :: T.Text -> Type -> VarianceExpr
analyzeTypeVariance paramName = go
  where
    go :: Type -> VarianceExpr
    go (TGeneric name) 
      | name == paramName = ConstE Covariant  -- Direct occurrence
      | otherwise = ConstE Bivariant          -- Different parameter
    
    go (TFunction args ret) = 
      -- Arguments are contravariant, return is covariant
      let argExprs = [ComposeE [ConstE Contravariant, go arg] | arg <- args]
          retExpr = go ret
      in case argExprs ++ [retExpr] of
        [] -> ConstE Bivariant
        [single] -> single
        multiple -> CombineE multiple
    
    go (TApplication typeName argTypes) =
      -- Compose with the variance of the applied type
      let baseVar = VarE (extractBaseName typeName <> "_" <> paramName)
          argExprs = map go argTypes
      in case argExprs of
        [] -> baseVar
        _ -> ComposeE (baseVar : argExprs)
      
    go (TUnion types) = 
      case map go (Set.toList types) of
        [] -> ConstE Bivariant
        [single] -> single
        multiple -> CombineE multiple
        
    go (TIntersection types) = 
      case map go (Set.toList types) of
        [] -> ConstE Bivariant
        [single] -> single
        multiple -> CombineE multiple
        
    go _ = ConstE Bivariant  -- Primitives, literals, type variables, etc.
    
    extractBaseName :: T.Text -> T.Text
    extractBaseName name = case T.stripPrefix "TYPE_" name of
      Just base -> base
      Nothing -> name

-- | Solve a system of variance equations using fixed-point iteration
solveVarianceSystem :: VarianceSystem -> Map.Map T.Text Variance
solveVarianceSystem equations = findFixedPoint initial
  where
    -- Start with all variables as Bivariant
    initial = Map.map (const Bivariant) equations
    
    -- One iteration: evaluate all equations with current assignments
    step :: Map.Map T.Text Variance -> Map.Map T.Text Variance
    step current = Map.mapWithKey (\var expr -> evalExpr current expr) equations
    
    -- Find fixed point
    findFixedPoint :: Map.Map T.Text Variance -> Map.Map T.Text Variance
    findFixedPoint current = 
      let next = step current
      in if current == next 
         then current 
         else findFixedPoint next
    
    -- Evaluate expression with current variable assignments
    evalExpr :: Map.Map T.Text Variance -> VarianceExpr -> Variance
    evalExpr env expr = case expr of
      VarE x -> Map.findWithDefault Bivariant x env
      ConstE v -> v
      CombineE [] -> Bivariant  -- Identity for combine
      CombineE [e] -> evalExpr env e
      CombineE (e:es) -> combineVariance (evalExpr env e) (evalExpr env (CombineE es))
      ComposeE [] -> Covariant  -- Identity for compose
      ComposeE [e] -> evalExpr env e
      ComposeE (e:es) -> composeVariance (evalExpr env e) (evalExpr env (ComposeE es))