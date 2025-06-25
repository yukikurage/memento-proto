{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.VarianceAnalysis
  ( VarianceExpr (..),
    VarianceSystem,
    VarianceAnalysis (..),
    analyzeVariances,
    solveVarianceSystem,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.Syntax
import qualified Language.Memento.Syntax.MType as SMType
import Language.Memento.Syntax.Tag (KType, KTypeVariable)
import qualified Language.Memento.Syntax.Variable as SVariable
import Language.Memento.TypeSolver.DataTypeAnalysis
import Language.Memento.TypeSolver.Types

-- | Variance expression representing how a type parameter appears
data VarianceExpr
  = VarE T.Text                -- ^ Variable reference (recursive type)
  | ConstE Variance            -- ^ Constant variance value
  | CombineE [VarianceExpr]    -- ^ Addition (+) - parameter appears multiple times
  | ComposeE [VarianceExpr]    -- ^ Multiplication (*) - parameter appears nested
  deriving (Eq, Show)

-- | System of variance equations mapping variable names to expressions
type VarianceSystem = Map.Map T.Text VarianceExpr

-- | Complete variance analysis result
data VarianceAnalysis = VarianceAnalysis
  { vaVariances :: Map.Map T.Text [Variance],  -- ^ Solved variances for each type
    vaEquations :: VarianceSystem              -- ^ Original equations (for debugging)
  }
  deriving (Show, Eq)

-- | Analyze variances for all data types
analyzeVariances :: DataTypeAnalysis -> VarianceAnalysis
analyzeVariances analysis =
  let equations = generateVarianceEquations analysis
      solution = solveVarianceSystem equations
      variances = extractVariancesFromSolution analysis solution
  in VarianceAnalysis
    { vaVariances = variances
    , vaEquations = equations
    }

-- | Generate variance equations from data type analysis
generateVarianceEquations :: DataTypeAnalysis -> VarianceSystem
generateVarianceEquations analysis = Map.fromList allEquations
  where
    allEquations = concatMap generateForDataType (Map.elems $ dtaDataTypes analysis)
    
    generateForDataType :: DataTypeInfo -> [(T.Text, VarianceExpr)]
    generateForDataType dt =
      [ (dtiName dt <> "_" <> param, generateForParam dt param)
      | param <- dtiTypeParams dt
      ]
    
    generateForParam :: DataTypeInfo -> T.Text -> VarianceExpr
    generateForParam dt param =
      let ctorExprs = map (analyzeConstructorForParam param) (dtiConstructors dt)
      in case ctorExprs of
        [] -> ConstE Bivariant
        [single] -> single
        multiple -> CombineE multiple
    
    analyzeConstructorForParam :: T.Text -> ConstructorInfo -> VarianceExpr
    analyzeConstructorForParam param ctor =
      let argExprs = map (analyzeTypeForParam param) (ciArgTypes ctor)
      in case argExprs of
        [] -> ConstE Bivariant
        [single] -> single
        multiple -> CombineE multiple
    
    analyzeTypeForParam :: T.Text -> AST KType -> VarianceExpr
    analyzeTypeForParam param typeAst = analyzeType param typeAst
    
    -- Core variance analysis for a type AST
    analyzeType :: T.Text -> AST KType -> VarianceExpr
    analyzeType param typeAst = case unMType (extractSyntax typeAst) of
      -- Primitive types don't contribute to variance
      SMType.TNumber -> ConstE Bivariant
      SMType.TInt -> ConstE Bivariant
      SMType.TBool -> ConstE Bivariant
      SMType.TString -> ConstE Bivariant
      SMType.TNever -> ConstE Bivariant
      SMType.TUnknown -> ConstE Bivariant
      
      -- Type variable - check if it's our target parameter
      SMType.TVar varAst ->
        let SVariable.TypeVar varName = unTypeVariable (extractSyntax varAst)
        in if varName == param
           then ConstE Covariant  -- Direct occurrence is covariant
           else ConstE Bivariant  -- Different parameter
      
      -- Function type - arguments are contravariant, return is covariant
      SMType.TFunction params retAst ->
        let argExprs = [ComposeE [ConstE Contravariant, analyzeType param argAst] 
                       | (_, argAst) <- params]  -- Extract types from (param, type) pairs
            retExpr = analyzeType param retAst
        in case argExprs ++ [retExpr] of
          [] -> ConstE Bivariant
          [single] -> single
          multiple -> CombineE multiple
      
      -- Union and intersection types maintain variance
      SMType.TUnion typeAsts ->
        let exprs = map (analyzeType param) typeAsts
        in case exprs of
          [] -> ConstE Bivariant
          [single] -> single
          multiple -> CombineE multiple
      
      SMType.TIntersection typeAsts ->
        let exprs = map (analyzeType param) typeAsts
        in case exprs of
          [] -> ConstE Bivariant
          [single] -> single
          multiple -> CombineE multiple
      
      -- Type application - need to compose with the applied type's variance
      SMType.TApplication baseAst argAsts ->
        let SVariable.TypeVar baseName = unTypeVariable (extractSyntax baseAst)
            -- Recursive reference to another type
            baseExpr = VarE (baseName <> "_" <> param)
            argExprs = map (analyzeType param) argAsts
        in case argExprs of
          [] -> baseExpr
          _ -> ComposeE (baseExpr : argExprs)

-- | Solve variance equations using fixed-point iteration
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

-- | Extract variance information organized by data type
extractVariancesFromSolution :: DataTypeAnalysis -> Map.Map T.Text Variance -> Map.Map T.Text [Variance]
extractVariancesFromSolution analysis solution = 
  Map.fromList
    [ (dtiName dt, extractVariancesForType dt)
    | dt <- Map.elems (dtaDataTypes analysis)
    ]
  where
    extractVariancesForType :: DataTypeInfo -> [Variance]
    extractVariancesForType dt =
      [ Map.findWithDefault Bivariant (dtiName dt <> "_" <> param) solution
      | param <- dtiTypeParams dt
      ]