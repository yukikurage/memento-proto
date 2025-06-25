{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.LowerToIR where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Language.Memento.IR as IR
import qualified Language.Memento.TypeSolver.Types as TST
import Language.Memento.TypeSolver.Types (TypeScheme(..))
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

-- Main function to convert AST to IR
lowerProgram :: AST KProgram -> IR.IRProgram
lowerProgram astProgram = 
  let Program definitions = unProgram (extractSyntax astProgram)
      (dataDefs, valueDefs) = partitionDefinitions definitions
      (functions, globals) = partitionValues valueDefs
  in IR.IRProgram
     { IR.irDataDefs = map lowerDataDef dataDefs
     , IR.irFunctions = functions
     , IR.irGlobals = globals
     , IR.irMainFunc = findMainFunction valueDefs
     }

-- Enhanced function that uses type solver results
lowerProgramWithTypes :: AST KProgram -> Map.Map Text TypeScheme -> IR.IRProgram
lowerProgramWithTypes astProgram typeEnv = 
  let Program definitions = unProgram (extractSyntax astProgram)
      (dataDefs, valueDefs) = partitionDefinitions definitions
      (functions, globals) = partitionValuesWithTypes valueDefs typeEnv
  in IR.IRProgram
     { IR.irDataDefs = map lowerDataDef dataDefs
     , IR.irFunctions = functions
     , IR.irGlobals = globals
     , IR.irMainFunc = findMainFunction valueDefs
     }

partitionDefinitions :: [AST KDefinition] -> ([AST KDefinition], [AST KDefinition])
partitionDefinitions defs = 
  let isDataDef ast = case unDefinition (extractSyntax ast) of
        DataDef {} -> True
        _ -> False
  in (filter isDataDef defs, filter (not . isDataDef) defs)

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

lowerDataDef :: AST KDefinition -> IR.IRDataDef
lowerDataDef astDef = 
  case unDefinition (extractSyntax astDef) of
    DataDef dataName constructors ->
      let Var typeName = unVariable (extractSyntax dataName)
          irConstructors = zipWith (lowerConstructor typeName) [0..] constructors
      in IR.IRDataDef typeName irConstructors
    _ -> error "Expected DataDef"

lowerConstructor :: Text -> Int -> ConstructorDef AST -> IR.IRConstructor
lowerConstructor _typeName tag (ConstructorDef ctorName _typeParams ctorType) =
  let Var name = unVariable (extractSyntax ctorName)
      fields = extractFieldTypes (extractSyntax ctorType)
  in IR.IRConstructor name fields tag

extractFieldTypes :: Syntax AST KType -> [IR.IRType]
extractFieldTypes syntax = 
  case unMType syntax of
    TFunction params _returnType -> map (lowerType . extractSyntax . snd) params
    _ -> []  -- Constructor with no fields

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
    TVar varAST -> 
      let TypeVar varName = unTypeVariable (extractSyntax varAST)
      in IR.IRTData varName  -- Type variables become data references
    TApplication baseAST _args ->
      let TypeVar baseName = unTypeVariable (extractSyntax baseAST)
      in IR.IRTData baseName  -- Polymorphic types become simple data references
    TUnknown -> IR.IRTUnknown
    TNever -> IR.IRTUnknown
    TLiteral _ -> IR.IRTUnknown  -- Literal types not supported in IR
    TUnion _ -> IR.IRTUnknown    -- Union types not supported in IR
    TIntersection _ -> IR.IRTUnknown  -- Intersection types not supported in IR

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
    
    EMatch exprsAST casesAST ->
      -- Simplified: only handle single expression matching
      case exprsAST of
        [exprAST] -> 
          let expr = lowerExpr exprAST
              cases = map lowerCase casesAST
          in IR.IRMatch expr cases
        _ -> error "Multi-expression matching not supported in IR"

lowerLambdaParam :: (AST KPattern, Maybe (AST KType)) -> (Text, IR.IRType)
lowerLambdaParam (patAST, maybeTypeAST) = 
  case unPattern (extractSyntax patAST) of
    PVar varAST ->
      let Var name = unVariable (extractSyntax varAST)
          irType = case maybeTypeAST of
                     Just typeAST -> lowerType (extractSyntax typeAST)
                     Nothing -> IR.IRTUnknown
      in (name, irType)
    _ -> error "Only variable patterns supported in lambda parameters"

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
        _ -> error "Only variable patterns supported in let bindings"

lowerCase :: ([(AST KPattern, Maybe (AST KType))], AST KExpr) -> IR.IRCase
lowerCase (patternsAST, exprAST) = 
  -- Simplified: only handle single pattern cases  
  case patternsAST of
    [(patAST, _maybeType)] ->
      let pattern = lowerPattern patAST
          body = lowerExpr exprAST
      in IR.IRCase pattern body
    _ -> error "Multi-pattern cases not supported in IR"

lowerPattern :: AST KPattern -> IR.IRPattern
lowerPattern astPat = 
  case unPattern (extractSyntax astPat) of
    PVar varAST ->
      let Var name = unVariable (extractSyntax varAST)
      in IR.IRPatVar name IR.IRTUnknown  -- Type inference needed
    
    PWildcard ->
      IR.IRPatWildcard IR.IRTUnknown
    
    PLiteral litAST ->
      IR.IRPatLiteral (lowerLiteral (extractSyntax litAST))
    
    PCons ctorAST argPatsAST ->
      let Var ctorName = unVariable (extractSyntax ctorAST)
          argPatterns = map lowerPattern argPatsAST
      in IR.IRPatConstructor ctorName argPatterns

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
typeToIRType (TST.TApplication name _args) = IR.IRTData name
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
    _ -> error "Only variable patterns supported in lambda parameters"