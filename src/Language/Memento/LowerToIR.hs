{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.LowerToIR where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Language.Memento.IR as IR
import qualified Language.Memento.TypeSolver.Core.Types as TST
import Language.Memento.TypeSolver.Core.Types (TypeScheme(..))
import Language.Memento.Syntax
import Language.Memento.Syntax.BinOp
import Language.Memento.Syntax.Definition
import Language.Memento.Syntax.Expr
import Language.Memento.Syntax.Literal
import Language.Memento.Syntax.MType
import Language.Memento.Syntax.Pattern
import Language.Memento.Syntax.Program
import Language.Memento.Syntax.Tag
import Language.Memento.Syntax.Variable

-- Main function to convert AST to primitive IR (no data types or pattern matching)
lowerProgram :: AST KProgram -> IR.IRProgram
lowerProgram astProgram = 
  let Program definitions = unProgram (extractSyntax astProgram)
      valueDefs = filterValueDefinitions definitions  -- Skip data definitions
      (functions, globals) = partitionValues valueDefs
  in IR.IRProgram
     { IR.irFunctions = functions
     , IR.irGlobals = globals
     , IR.irMainFunc = findMainFunction valueDefs
     }

-- Enhanced function that uses type solver results
lowerProgramWithTypes :: AST KProgram -> Map.Map Text TypeScheme -> IR.IRProgram
lowerProgramWithTypes astProgram typeEnv = 
  let Program definitions = unProgram (extractSyntax astProgram)
      valueDefs = filterValueDefinitions definitions  -- Skip data definitions
      (functions, globals) = partitionValuesWithTypes valueDefs typeEnv
  in IR.IRProgram
     { IR.irFunctions = functions
     , IR.irGlobals = globals
     , IR.irMainFunc = findMainFunction valueDefs
     }

-- Filter out data definitions, keep only value definitions  
filterValueDefinitions :: [AST KDefinition] -> [AST KDefinition]
filterValueDefinitions defs = 
  filter isValueDef defs
  where
    isValueDef ast = case unDefinition (extractSyntax ast) of
      ValDef {} -> True
      TypeDef {} -> True  -- Keep type aliases
      DataDef {} -> False  -- Skip data definitions

partitionValues :: [AST KDefinition] -> ([IR.IRFunction], [IR.IRGlobal])
partitionValues valueDefs = 
  let classifyValue ast = case unDefinition (extractSyntax ast) of
        ValDef _ _ _ expr -> case isLambdaExpr expr of
          True -> Left (lowerFunctionDef ast)
          False -> Right (lowerValueDef ast)
        _ -> Right (lowerValueDef ast)  -- Default to global
      classified = map classifyValue valueDefs
      functions = [f | Left f <- classified]
      globals = [g | Right g <- classified]
  in (functions, globals)

-- Enhanced partition that uses type solver results
partitionValuesWithTypes :: [AST KDefinition] -> Map.Map Text TypeScheme -> ([IR.IRFunction], [IR.IRGlobal])
partitionValuesWithTypes valueDefs typeEnv = 
  let classifyValue ast = case unDefinition (extractSyntax ast) of
        ValDef _ _ _ expr -> case isLambdaExpr expr of
          True -> Left (lowerFunctionDefWithTypes ast typeEnv)
          False -> Right (lowerValueDefWithTypes ast typeEnv)
        _ -> Right (lowerValueDefWithTypes ast typeEnv)  -- Default to global
      classified = map classifyValue valueDefs
      functions = [f | Left f <- classified]
      globals = [g | Right g <- classified]
  in (functions, globals)

isLambdaExpr :: AST KExpr -> Bool
isLambdaExpr astExpr = case unExpr (extractSyntax astExpr) of
  ELambda {} -> True
  _ -> False

lowerFunctionDef :: AST KDefinition -> IR.IRFunction
lowerFunctionDef astDef = 
  case unDefinition (extractSyntax astDef) of
    ValDef varName _typeParams typeAnnotation expr ->
      case unExpr (extractSyntax expr) of
        ELambda params bodyAST ->
          let Var name = unVariable (extractSyntax varName)
              irParams = map lowerLambdaParam params
              irReturnType = extractReturnType (extractSyntax typeAnnotation)
              irBody = lowerExpr bodyAST
          in IR.IRFunction name irParams irReturnType irBody
        _ -> error "Expected lambda expression in function definition"
    _ -> error "Expected ValDef"

extractReturnType :: Syntax AST KType -> IR.IRType
extractReturnType syntax = 
  case unMType syntax of
    TFunction _params returnType -> lowerType (extractSyntax returnType)
    _ -> IR.IRTUnknown


lowerValueDef :: AST KDefinition -> IR.IRGlobal
lowerValueDef astDef = 
  case unDefinition (extractSyntax astDef) of
    ValDef varName _typeParams typeAnnotation expr ->
      let Var name = unVariable (extractSyntax varName)
          irType = lowerType (extractSyntax typeAnnotation)
          irExpr = lowerExpr expr
      in IR.IRGlobal name irType irExpr
    _ -> error "Expected ValDef"

findMainFunction :: [AST KDefinition] -> Maybe Text
findMainFunction [] = Nothing
findMainFunction (def:rest) = 
  case unDefinition (extractSyntax def) of
    ValDef varName _ _ _ ->
      let Var name = unVariable (extractSyntax varName)
      in if name == "main" then Just name else findMainFunction rest
    _ -> findMainFunction rest

lowerType :: Syntax AST KType -> IR.IRType
lowerType syntax = 
  case unMType syntax of
    TNumber -> IR.IRTNumber
    TInt -> IR.IRTInt
    TBool -> IR.IRTBool
    TString -> IR.IRTString
    TFunction params returnType -> 
      let paramTypes = map (lowerType . extractSyntax . snd) params
          retType = lowerType (extractSyntax returnType)
      in IR.IRTFunction paramTypes retType
    TVar _ -> IR.IRTUnknown  -- Type variables become unknown in primitive IR
    TApplication _ _ -> IR.IRTUnknown  -- Type applications become unknown in primitive IR
    TUnknown -> IR.IRTUnknown
    TNever -> IR.IRTUnknown
    TLiteral _ -> IR.IRTUnknown
    TUnion _ -> IR.IRTUnknown
    TIntersection _ -> IR.IRTUnknown

lowerExpr :: AST KExpr -> IR.IRExpr
lowerExpr astExpr = 
  case unExpr (extractSyntax astExpr) of
    EVar varAST ->
      let Var name = unVariable (extractSyntax varAST)
      in IR.IRVar name
    
    ELiteral litAST ->
      IR.IRLiteral (lowerLiteral (extractSyntax litAST))
    
    EBinOp opAST leftAST rightAST ->
      let op = lowerBinOp (extractSyntax opAST)
          left = lowerExpr leftAST
          right = lowerExpr rightAST
      in IR.IRBinOp op left right
    
    EApply funcAST argsAST ->
      let func = lowerExpr funcAST
          args = map lowerExpr argsAST
      in IR.IRCall func args
    
    ELambda params bodyAST ->
      let irParams = map lowerLambdaParam params
          irReturnType = IR.IRTUnknown  -- Type inference needed
          irBody = lowerExpr bodyAST
      in IR.IRLambda irParams irReturnType irBody
    
    EIf condAST thenAST elseAST ->
      let cond = lowerExpr condAST
          thenExpr = lowerExpr thenAST
          elseExpr = lowerExpr elseAST
      in IR.IRIf cond thenExpr elseExpr
    
    EBlock letsAST bodyAST ->
      let lets = map lowerLet letsAST
          body = lowerExpr bodyAST
      in foldr (\(name, ty, expr) acc -> IR.IRLet name ty expr acc) body lets
    
    EMatch _ _ ->
      -- Pattern matching not supported in primitive IR
      error "Pattern matching not supported in primitive IR - should be desugared to if-then-else"

lowerLambdaParam :: (AST KPattern, Maybe (AST KType)) -> (Text, IR.IRType)
lowerLambdaParam (patAST, maybeTypeAST) = 
  case unPattern (extractSyntax patAST) of
    PVar varAST ->
      let Var name = unVariable (extractSyntax varAST)
          irType = case maybeTypeAST of
                     Just typeAST -> lowerType (extractSyntax typeAST)
                     Nothing -> IR.IRTUnknown
      in (name, irType)
    PWildcard ->
      -- Generate a dummy name for wildcard patterns
      let dummyName = "_wildcard"
          irType = case maybeTypeAST of
                     Just typeAST -> lowerType (extractSyntax typeAST)
                     Nothing -> IR.IRTUnknown
      in (dummyName, irType)
    PLiteral _ ->
      error "Literal patterns in lambda parameters not supported in primitive IR - use if-then-else instead"
    PCons _ _ ->
      error "Constructor patterns in lambda parameters not supported in primitive IR - use if-then-else instead"

lowerLet :: AST KLet -> (Text, IR.IRType, IR.IRExpr)
lowerLet astLet = 
  case unLet (extractSyntax astLet) of
    Let patAST maybeTypeAST exprAST ->
      case unPattern (extractSyntax patAST) of
        PVar varAST ->
          let Var name = unVariable (extractSyntax varAST)
              irType = case maybeTypeAST of
                         Just typeAST -> lowerType (extractSyntax typeAST)
                         Nothing -> IR.IRTUnknown
              irExpr = lowerExpr exprAST
          in (name, irType, irExpr)
        PWildcard ->
          -- Generate a dummy name for wildcard patterns in let bindings
          let dummyName = "_let_wildcard"
              irType = case maybeTypeAST of
                         Just typeAST -> lowerType (extractSyntax typeAST)
                         Nothing -> IR.IRTUnknown
              irExpr = lowerExpr exprAST
          in (dummyName, irType, irExpr)
        PLiteral _ ->
          error "Literal patterns in let bindings not supported in primitive IR - use if-then-else instead"
        PCons _ _ ->
          error "Constructor patterns in let bindings not supported in primitive IR - use if-then-else instead"


lowerLiteral :: Syntax AST KLiteral -> IR.IRLiteral
lowerLiteral syntax = 
  case unLiteral syntax of
    NumberLiteral n -> IR.IRNumberLit n
    IntLiteral i -> IR.IRNumberLit (fromIntegral i)  -- Convert int to number
    BoolLiteral b -> IR.IRBoolLit b
    StringLiteral s -> IR.IRStringLit s

lowerBinOp :: Syntax AST KBinOp -> IR.IRBinOp
lowerBinOp syntax = 
  case unBinOp syntax of
    Add -> IR.IRAdd
    Sub -> IR.IRSub
    Mul -> IR.IRMul
    Div -> IR.IRDiv
    Eq -> IR.IREq
    Lt -> IR.IRLt
    Gt -> IR.IRGt

-- Enhanced functions that use type solver results
lowerFunctionDefWithTypes :: AST KDefinition -> Map.Map Text TypeScheme -> IR.IRFunction
lowerFunctionDefWithTypes astDef typeEnv = 
  case unDefinition (extractSyntax astDef) of
    ValDef varName _typeParams _typeAnnotation expr ->
      case unExpr (extractSyntax expr) of
        ELambda params bodyAST ->
          let Var name = unVariable (extractSyntax varName)
              -- Get type from type solver instead of AST annotation
              defaultScheme = TypeScheme [] (TST.TFunction [] TST.TNumber)
              TypeScheme _ solvedType = Map.findWithDefault defaultScheme name typeEnv
              (irParams, irReturnType) = extractFunctionTypeParts solvedType params
              irBody = lowerExpr bodyAST
          in IR.IRFunction name irParams irReturnType irBody
        _ -> error "Expected lambda expression in function definition"
    _ -> error "Expected ValDef"

lowerValueDefWithTypes :: AST KDefinition -> Map.Map Text TypeScheme -> IR.IRGlobal
lowerValueDefWithTypes astDef typeEnv = 
  case unDefinition (extractSyntax astDef) of
    ValDef varName _typeParams _typeAnnotation expr ->
      let Var name = unVariable (extractSyntax varName)
          -- Get type from type solver instead of AST annotation
          defaultScheme = TypeScheme [] TST.TNumber
          TypeScheme _ solvedType = Map.findWithDefault defaultScheme name typeEnv
          irType = typeToIRType solvedType
          irExpr = lowerExpr expr
      in IR.IRGlobal name irType irExpr
    _ -> error "Expected ValDef"

-- Convert type solver Type to IR.IRType
typeToIRType :: TST.Type -> IR.IRType
typeToIRType TST.TNumber = IR.IRTNumber
typeToIRType TST.TString = IR.IRTString
typeToIRType TST.TBool = IR.IRTBool
typeToIRType (TST.TFunction params returnType) = 
  IR.IRTFunction (map typeToIRType params) (typeToIRType returnType)
typeToIRType (TST.TApplication _name _args) = IR.IRTUnknown
typeToIRType _ = IR.IRTUnknown

-- Extract function parameter types and return type from type solver result
extractFunctionTypeParts :: TST.Type -> [(AST KPattern, Maybe (AST KType))] -> ([(Text, IR.IRType)], IR.IRType)
extractFunctionTypeParts (TST.TFunction paramTypes returnType) lambdaParams =
  let paramNames = map extractParamName lambdaParams
      irParamTypes = map typeToIRType paramTypes
      irParams = zip paramNames irParamTypes
      irReturnType = typeToIRType returnType
  in (irParams, irReturnType)
extractFunctionTypeParts _ lambdaParams =
  -- Fallback to existing behavior if type doesn't match expected pattern
  let irParams = map lowerLambdaParam lambdaParams
  in (irParams, IR.IRTUnknown)

extractParamName :: (AST KPattern, Maybe (AST KType)) -> Text
extractParamName (patAST, _) =
  case unPattern (extractSyntax patAST) of
    PVar varAST ->
      let Var name = unVariable (extractSyntax varAST)
      in name
    PWildcard ->
      "_param_wildcard"  -- Generate dummy name for wildcard patterns
    PLiteral _ ->
      error "Literal patterns in function parameters not supported in primitive IR - use if-then-else instead"
    PCons _ _ ->
      error "Constructor patterns in function parameters not supported in primitive IR - use if-then-else instead"