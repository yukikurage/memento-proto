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
  , haDependencyGraph :: Map.Map T.Text (Set.Set T.Text)  -- function -> functions it calls
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
          dependencyGraph = buildDependencyGraph definitions
          warnings = detectHoistingIssues definitionOrder dependencyGraph
      in HoistingAnalysis
          { haWarnings = warnings
          , haDependencyGraph = dependencyGraph  
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
-- Dependency Graph Construction
-- ============================================================================

-- | Build dependency graph: function -> set of functions it calls
buildDependencyGraph :: [AST KDefinition] -> Map.Map T.Text (Set.Set T.Text)
buildDependencyGraph definitions =
  Map.fromList 
    [ (funcName, findFunctionCalls bodyAst)
    | def <- definitions
    , Just (funcName, bodyAst) <- [extractFunctionBody def]
    ]
 where
  extractFunctionBody :: AST KDefinition -> Maybe (T.Text, AST KExpr)
  extractFunctionBody ast =
    case unDefinition (extractSyntax ast) of
      SDefinition.ValDef varAst _ _ bodyAst -> 
        let SVariable.Var name = unVariable (extractSyntax varAst)
        in Just (name, bodyAst)
      _ -> Nothing  -- Skip data and type definitions

-- | Find all function calls in an expression
findFunctionCalls :: AST KExpr -> Set.Set T.Text
findFunctionCalls ast = 
  case unExpr (extractSyntax ast) of
    SExpr.EVar varAst ->
      let SVariable.Var name = unVariable (extractSyntax varAst)
      in Set.singleton name
    
    SExpr.EApply funcAst argAsts ->
      let funcCalls = findFunctionCalls funcAst
          argCalls = Set.unions (map findFunctionCalls argAsts)
      in Set.union funcCalls argCalls
    
    SExpr.ELambda _params bodyAst ->
      findFunctionCalls bodyAst
    
    SExpr.EIf condAst thenAst elseAst ->
      Set.unions [findFunctionCalls condAst, findFunctionCalls thenAst, findFunctionCalls elseAst]
    
    SExpr.EBinOp _opAst leftAst rightAst ->
      Set.union (findFunctionCalls leftAst) (findFunctionCalls rightAst)
    
    SExpr.EBlock letAsts exprAst ->
      let letCalls = Set.unions (map findLetCalls letAsts)
          exprCalls = findFunctionCalls exprAst
      in Set.union letCalls exprCalls
    
    SExpr.EMatch scrutineeAsts cases ->
      let scrutineeCalls = Set.unions (map findFunctionCalls scrutineeAsts)
          caseCalls = Set.unions [findFunctionCalls caseExpr | (_, caseExpr) <- cases]
      in Set.union scrutineeCalls caseCalls
    
    SExpr.ELiteral _ ->
      Set.empty
 where
  findLetCalls letAst = 
    case unLet (extractSyntax letAst) of
      SExpr.Let _pat _mType exprAst -> findFunctionCalls exprAst

-- ============================================================================
-- Hoisting Issue Detection
-- ============================================================================

-- | Detect hoisting issues based on definition order and dependencies
detectHoistingIssues :: [T.Text] -> Map.Map T.Text (Set.Set T.Text) -> [HoistingWarning]
detectHoistingIssues definitionOrder dependencyGraph =
  let functionPositions = Map.fromList (zip definitionOrder [0..])
  in concat 
      [ detectCallBeforeDefinition funcName calls functionPositions
      | (funcName, calls) <- Map.toList dependencyGraph
      ]
 where
  detectCallBeforeDefinition :: T.Text -> Set.Set T.Text -> Map.Map T.Text Int -> [HoistingWarning]
  detectCallBeforeDefinition caller calls positions =
    [ HoistingWarning
        { hwFunction = caller
        , hwCalls = callee
        , hwIssue = CallBeforeDefinition
        , hwSeverity = determineSeverity caller callee positions
        }
    | callee <- Set.toList calls
    , isCallBeforeDefinition caller callee positions
    ]
  
  isCallBeforeDefinition :: T.Text -> T.Text -> Map.Map T.Text Int -> Bool
  isCallBeforeDefinition caller callee positions =
    case (Map.lookup caller positions, Map.lookup callee positions) of
      (Just callerPos, Just calleePos) -> callerPos < calleePos
      _ -> False  -- If function not found, assume it's external
  
  determineSeverity :: T.Text -> T.Text -> Map.Map T.Text Int -> WarningSeverity
  determineSeverity caller callee _positions
    | caller == callee = Warning  -- Self-recursion is fine due to hoisting
    | otherwise = Error          -- Call before definition will fail

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
      , ""
      , "Definition Order: " ++ intercalate " -> " (map T.unpack (haDefinitionOrder analysis))
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
    in severityStr ++ ": Function '" ++ T.unpack (hwFunction warn) ++ 
       "' " ++ issueStr ++ " '" ++ T.unpack (hwCalls warn) ++ "'"