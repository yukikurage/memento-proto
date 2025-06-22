{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript Hoisting Analysis for Memento
-- 
-- This module analyzes potential runtime errors due to JavaScript's hoisting behavior.
-- While Memento's type system allows forward references and recursion, JavaScript
-- evaluation order can cause runtime errors if functions are called before they're
-- defined in the generated code.
module Language.Memento.Analysis.Hoisting (
  HoistingWarning(..),
  HoistingAnalysis(..),
  analyzeHoisting,
  formatHoistingWarnings
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (intercalate)
import GHC.Base (List)

import Language.Memento.Data.HFix (unHFix)
import Language.Memento.Syntax
import qualified Language.Memento.Syntax.Definition as SDefinition
import qualified Language.Memento.Syntax.Expr as SExpr
import qualified Language.Memento.Syntax.Variable as SVariable
import qualified Language.Memento.Syntax.Program as SProgram
import Language.Memento.Syntax.Tag (KDefinition, KExpr, KVariable, KProgram)

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Warning about potential hoisting issues
data HoistingWarning = HoistingWarning
  { hwFunction :: T.Text        -- Function that has the issue
  , hwCalls :: T.Text          -- Function being called
  , hwIssue :: HoistingIssue   -- Type of issue
  , hwSeverity :: WarningSeverity
  } deriving (Show, Eq)

data HoistingIssue
  = CallBeforeDefinition        -- Function called before it's defined
  | PotentialTDZ               -- Potential Temporal Dead Zone issue
  deriving (Show, Eq)

data WarningSeverity 
  = Warning                    -- Potential issue
  | Error                      -- Definite runtime error
  deriving (Show, Eq)

-- | Result of hoisting analysis
data HoistingAnalysis = HoistingAnalysis
  { haWarnings :: [HoistingWarning]
  , haTopLevelCalls :: [(T.Text, Int)]  -- Functions called at top-level with call positions
  , haDefinitionOrder :: [T.Text]  -- Order functions are defined
  } deriving (Show)

-- ============================================================================
-- Main Analysis Function
-- ============================================================================

-- | Analyze a program for hoisting issues
analyzeHoisting :: AST KProgram -> HoistingAnalysis
analyzeHoisting ast = 
  case unProgram (extractSyntax ast) of
    SProgram.Program definitions -> 
      let definitionOrder = extractDefinitionOrder definitions
          topLevelCalls = findTopLevelCalls definitions
          functionDeps = buildFunctionDependencies definitions
          warnings = detectTransitiveHoistingIssues definitionOrder topLevelCalls functionDeps
      in HoistingAnalysis
          { haWarnings = warnings
          , haTopLevelCalls = topLevelCalls  
          , haDefinitionOrder = definitionOrder
          }

-- ============================================================================
-- Definition Order Analysis
-- ============================================================================

-- | Extract the order in which functions are defined
extractDefinitionOrder :: [AST KDefinition] -> [T.Text]
extractDefinitionOrder definitions = 
  [ name 
  | def <- definitions
  , Just name <- [extractFunctionName def]
  ]
 where
  extractFunctionName :: AST KDefinition -> Maybe T.Text
  extractFunctionName ast =
    case unDefinition (extractSyntax ast) of
      SDefinition.ValDef varAst _ _ _ -> 
        let SVariable.Var name = unVariable (extractSyntax varAst)
        in Just name
      _ -> Nothing  -- Skip data and type definitions

-- ============================================================================
-- Top-Level Call Analysis
-- ============================================================================

-- | Find all top-level calls with their calling definition positions
findTopLevelCalls :: [AST KDefinition] -> [(T.Text, Int)]
findTopLevelCalls definitions =
  concat 
    [ [ (calledFunc, pos) 
      | calledFunc <- Set.toList (findTopLevelCallsInDefinition def) ]
    | (def, pos) <- zip definitions [0..]
    ]

-- | Build function dependency graph: function -> functions it calls directly
buildFunctionDependencies :: [AST KDefinition] -> Map.Map T.Text (Set.Set T.Text)
buildFunctionDependencies definitions =
  Map.fromList 
    [ (funcName, findFunctionCallsInBody bodyAst)
    | def <- definitions
    , Just (funcName, bodyAst) <- [extractFunctionDefinition def]
    ]
 where
  extractFunctionDefinition :: AST KDefinition -> Maybe (T.Text, AST KExpr)
  extractFunctionDefinition ast =
    case unDefinition (extractSyntax ast) of
      SDefinition.ValDef varAst _typeParams _mType bodyAst -> 
        let SVariable.Var name = unVariable (extractSyntax varAst)
        in case unExpr (extractSyntax bodyAst) of
          SExpr.ELambda _ _ -> Just (name, bodyAst)  -- Function definition
          _ -> Nothing  -- Value definition
      _ -> Nothing  -- Data/Type definitions

  findFunctionCallsInBody :: AST KExpr -> Set.Set T.Text  
  findFunctionCallsInBody ast = 
    case unExpr (extractSyntax ast) of
      SExpr.ELambda _params bodyAst -> findAllCalls bodyAst
      _ -> Set.empty

  findAllCalls :: AST KExpr -> Set.Set T.Text
  findAllCalls ast = 
    case unExpr (extractSyntax ast) of
      SExpr.EVar varAst ->
        let SVariable.Var name = unVariable (extractSyntax varAst)
        in Set.singleton name
      SExpr.EApply funcAst argAsts ->
        let funcCalls = findAllCalls funcAst
            argCalls = Set.unions (map findAllCalls argAsts)
        in Set.union funcCalls argCalls
      SExpr.ELambda _params bodyAst ->
        findAllCalls bodyAst
      SExpr.EIf condAst thenAst elseAst ->
        Set.unions [findAllCalls condAst, findAllCalls thenAst, findAllCalls elseAst]
      SExpr.EBinOp _opAst leftAst rightAst ->
        Set.union (findAllCalls leftAst) (findAllCalls rightAst)
      SExpr.EBlock letAsts exprAst ->
        let letCalls = Set.unions (map findLetCallsAll letAsts)
            exprCalls = findAllCalls exprAst
        in Set.union letCalls exprCalls
      SExpr.EMatch scrutineeAsts cases ->
        let scrutineeCalls = Set.unions (map findAllCalls scrutineeAsts)
            caseCalls = Set.unions [findAllCalls caseExpr | (_, caseExpr) <- cases]
        in Set.union scrutineeCalls caseCalls
      SExpr.ELiteral _ ->
        Set.empty
   where
    findLetCallsAll letAst = 
      case unLet (extractSyntax letAst) of
        SExpr.Let _pat _mType exprAst -> findAllCalls exprAst

findTopLevelCallsInDefinition :: AST KDefinition -> Set.Set T.Text
findTopLevelCallsInDefinition ast =
    case unDefinition (extractSyntax ast) of
      SDefinition.ValDef _varAst _typeParams _mType bodyAst -> 
        -- Check if this is a function definition by examining the body
        -- If body is a lambda, it's a function definition (calls are deferred)
        -- If body is not a lambda, it's a value definition (calls execute immediately)
        case unExpr (extractSyntax bodyAst) of
          SExpr.ELambda _ _ -> Set.empty  -- Function definition - calls are deferred
          _ -> findImmediateCalls bodyAst  -- Value definition - calls execute immediately
      _ -> Set.empty  -- Data definitions don't have calls

-- | Find function calls that execute immediately (not deferred in lambda bodies)
findImmediateCalls :: AST KExpr -> Set.Set T.Text
findImmediateCalls ast = 
  case unExpr (extractSyntax ast) of
    SExpr.EVar varAst ->
      -- Variable reference alone is not a call
      Set.empty
    
    SExpr.EApply funcAst argAsts ->
      -- Function application is an immediate call
      let funcCalls = findFunctionCallsInExpr funcAst
          argCalls = Set.unions (map findArgumentCalls argAsts)
      in Set.union funcCalls argCalls
    
    SExpr.ELambda _params _bodyAst ->
      -- Lambda body calls are deferred, not immediate
      -- Exception: lambdas passed as arguments to immediate calls (handled in EApply)
      Set.empty
    
    SExpr.EIf condAst thenAst elseAst ->
      Set.unions [findImmediateCalls condAst, findImmediateCalls thenAst, findImmediateCalls elseAst]
    
    SExpr.EBinOp _opAst leftAst rightAst ->
      Set.union (findImmediateCalls leftAst) (findImmediateCalls rightAst)
    
    SExpr.EBlock letAsts exprAst ->
      let letCalls = Set.unions (map findLetCalls letAsts)
          exprCalls = findImmediateCalls exprAst
      in Set.union letCalls exprCalls
    
    SExpr.EMatch scrutineeAsts cases ->
      let scrutineeCalls = Set.unions (map findImmediateCalls scrutineeAsts)
          caseCalls = Set.unions [findImmediateCalls caseExpr | (_, caseExpr) <- cases]
      in Set.union scrutineeCalls caseCalls
    
    SExpr.ELiteral _ ->
      Set.empty
 where
  findLetCalls letAst = 
    case unLet (extractSyntax letAst) of
      SExpr.Let _pat _mType exprAst -> findImmediateCalls exprAst

-- | Extract function name from function call expression
findFunctionCallsInExpr :: AST KExpr -> Set.Set T.Text
findFunctionCallsInExpr ast =
  case unExpr (extractSyntax ast) of
    SExpr.EVar varAst ->
      let SVariable.Var name = unVariable (extractSyntax varAst)
      in Set.singleton name
    _ -> Set.empty  -- Only direct variable references are function calls

-- | Find calls in function arguments (lambdas might execute immediately)
findArgumentCalls :: AST KExpr -> Set.Set T.Text
findArgumentCalls ast =
  case unExpr (extractSyntax ast) of
    SExpr.ELambda _params bodyAst ->
      -- Lambda passed as argument might execute immediately
      findImmediateCalls bodyAst
    _ ->
      -- Non-lambda arguments: analyze normally
      findImmediateCalls ast

-- ============================================================================
-- Hoisting Issue Detection
-- ============================================================================

-- | Detect transitive hoisting issues
detectTransitiveHoistingIssues :: [T.Text] -> [(T.Text, Int)] -> Map.Map T.Text (Set.Set T.Text) -> [HoistingWarning]
detectTransitiveHoistingIssues definitionOrder topLevelCalls functionDeps =
  let functionPositions = Map.fromList (zip definitionOrder [0..])
  in concat
      [ detectTransitiveIssuesForCall calledFunc callPos functionPositions functionDeps
      | (calledFunc, callPos) <- topLevelCalls
      ]
 where
  detectTransitiveIssuesForCall :: T.Text -> Int -> Map.Map T.Text Int -> Map.Map T.Text (Set.Set T.Text) -> [HoistingWarning]
  detectTransitiveIssuesForCall calledFunc callPos positions deps =
    let reachableFuncs = findReachableFunctions calledFunc deps
    in [ HoistingWarning
          { hwFunction = "<top-level>"
          , hwCalls = reachableFunc
          , hwIssue = CallBeforeDefinition
          , hwSeverity = Error
          }
       | reachableFunc <- Set.toList reachableFuncs
       , case Map.lookup reachableFunc positions of
           Just defPos -> callPos < defPos  -- Transitively called before definition
           Nothing -> False  -- External function, assume it's fine
       ]

  -- Find all functions reachable from a given function through transitive calls
  findReachableFunctions :: T.Text -> Map.Map T.Text (Set.Set T.Text) -> Set.Set T.Text
  findReachableFunctions startFunc deps = go Set.empty [startFunc]
   where
    go visited [] = visited
    go visited (current:rest)
      | current `Set.member` visited = go visited rest
      | otherwise = 
          let newVisited = Set.insert current visited
              directCalls = Map.findWithDefault Set.empty current deps
              newToVisit = Set.toList directCalls ++ rest
          in go newVisited newToVisit

-- ============================================================================
-- Warning Formatting
-- ============================================================================

-- | Format hoisting warnings for display
formatHoistingWarnings :: HoistingAnalysis -> String
formatHoistingWarnings analysis = 
  if null (haWarnings analysis)
    then "No hoisting issues detected."
    else unlines 
      [ "JavaScript Hoisting Analysis:"
      , "==============================="
      , ""
      , intercalate "\n" (map formatWarning (haWarnings analysis))
      ]
 where
  formatWarning :: HoistingWarning -> String
  formatWarning warn = 
    let severityStr = case hwSeverity warn of
          Warning -> "WARNING"
          Error -> "ERROR"
        issueStr = case hwIssue warn of
          CallBeforeDefinition -> "calls function before it's defined"
          PotentialTDZ -> "potential temporal dead zone issue"
    in severityStr ++ ": Top-level code calls function '" ++ T.unpack (hwCalls warn) ++ 
       "' before it's defined"