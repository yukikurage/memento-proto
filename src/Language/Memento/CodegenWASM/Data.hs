{-# LANGUAGE OverloadedStrings #-}
module Language.Memento.CodegenWASM.Data where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (elemIndex)
import Language.Memento.CodegenWASM.Types
import Language.Memento.CodegenWASM.Memory
import qualified Language.Memento.IR as IR

-- Data type compilation context
data DataContext = DataContext
  { dcConstructorTags :: Map Text Int      -- Constructor name to tag mapping
  , dcConstructorTypes :: Map Text [WASMType]  -- Constructor field types
  , dcTypeLayouts :: Map Text TypeLayout  -- Type memory layouts
  } deriving (Show)

-- Memory layout for a data type
data TypeLayout = TypeLayout
  { tlTypeName :: Text
  , tlConstructors :: [ConstructorLayout]
  , tlMaxSize :: Int  -- Maximum size among all constructors
  , tlAlignment :: Int
  } deriving (Show)

-- Memory layout for a constructor
data ConstructorLayout = ConstructorLayout
  { clName :: Text
  , clTag :: Int
  , clFields :: [FieldLayout]
  , clTotalSize :: Int
  } deriving (Show)

-- Field layout information
data FieldLayout = FieldLayout
  { flOffset :: Int
  , flSize :: Int
  , flType :: WASMType
  , flAlignment :: Int
  } deriving (Show)

-- Standard sizes and alignments
sizeOf :: WASMType -> Int
sizeOf I32 = 4
sizeOf I64 = 8
sizeOf F32 = 4
sizeOf F64 = 8

alignmentOf :: WASMType -> Int
alignmentOf = sizeOf  -- Simplified: alignment = size

-- Generate appropriate store instruction for a type
storeInstruction :: WASMType -> Int -> Int -> WASMInstruction
storeInstruction I32 align offset = I32Store align offset
storeInstruction I64 align offset = I64Store align offset  
storeInstruction F32 align offset = F32Store align offset
storeInstruction F64 align offset = F64Store align offset

-- Calculate aligned offset
alignOffset :: Int -> Int -> Int
alignOffset offset alignment = ((offset + alignment - 1) `div` alignment) * alignment

-- Build data context from IR data definitions
buildDataContext :: [IR.IRDataDef] -> DataContext
buildDataContext dataDefs =
  let allConstructors = concatMap extractConstructors dataDefs
      tagMap = Map.fromList $ zip (map (\(name, _, _) -> name) allConstructors) [0..]
      typeMap = Map.fromList $ map (\(name, fields, _) -> (name, map irTypeToWASM fields)) allConstructors
      layoutMap = Map.fromList $ map (buildTypeLayout tagMap) dataDefs
  in DataContext tagMap typeMap layoutMap

-- Extract constructors from data definition
extractConstructors :: IR.IRDataDef -> [(Text, [IR.IRType], Text)]
extractConstructors (IR.IRDataDef typeName constructors) =
  map (\(IR.IRConstructor name fields _) -> (name, fields, typeName)) constructors

-- Build memory layout for a data type
buildTypeLayout :: Map Text Int -> IR.IRDataDef -> (Text, TypeLayout)
buildTypeLayout tagMap (IR.IRDataDef typeName constructors) =
  let constructorLayouts = map (buildConstructorLayout tagMap) constructors
      maxSize = maximum $ map clTotalSize constructorLayouts
      alignment = maximum $ map (maximum . map flAlignment . clFields) constructorLayouts
  in (typeName, TypeLayout typeName constructorLayouts maxSize alignment)

-- Build memory layout for a constructor
buildConstructorLayout :: Map Text Int -> IR.IRConstructor -> ConstructorLayout
buildConstructorLayout tagMap (IR.IRConstructor name fields _) =
  let tag = Map.findWithDefault 0 name tagMap
      fieldTypes = map irTypeToWASM fields
      fieldLayouts = calculateFieldLayouts fieldTypes
      totalSize = objectHeaderSize + sum (map flSize fieldLayouts)
  in ConstructorLayout name tag fieldLayouts totalSize

-- Calculate field layouts with proper alignment
calculateFieldLayouts :: [WASMType] -> [FieldLayout]
calculateFieldLayouts fieldTypes =
  let go [] _ = []
      go (t:ts) offset =
        let size = sizeOf t
            alignment = alignmentOf t
            alignedOffset = alignOffset offset alignment
            field = FieldLayout alignedOffset size t alignment
        in field : go ts (alignedOffset + size)
  in go fieldTypes objectHeaderSize

-- Compile data constructor function
compileDataConstructor :: DataContext -> IR.IRConstructor -> WASMFunction
compileDataConstructor dataCtx ctor@(IR.IRConstructor name fields _) =
  let tag = Map.findWithDefault 0 name (dcConstructorTags dataCtx)
      fieldTypes = map irTypeToWASM fields
      paramNames = ["field" <> T.pack (show i) | i <- [0..length fields - 1]]
      params = zip paramNames fieldTypes
      -- Add obj_ptr local variable for memory allocation
      localVars = [("obj_ptr", I32)]
      
      -- Generate allocation and initialization code
      allocationCode = generateDataAllocation dataCtx ctor paramNames
      
  in WASMFunction
     { funcName = name
     , funcTypeIndex = 0  -- Will be assigned later
     , funcParams = params           -- Function parameters
     , funcLocals = localVars        -- Local variables
     , funcReturnType = I32          -- Data constructors return pointers
     , funcBody = allocationCode ++ [Return]
     }

-- Generate allocation code for data constructor
generateDataAllocation :: DataContext -> IR.IRConstructor -> [Text] -> [WASMInstruction]
generateDataAllocation dataCtx ctor@(IR.IRConstructor name fields _) paramNames =
  let tag = Map.findWithDefault 0 name (dcConstructorTags dataCtx)
      layout = buildConstructorLayout (dcConstructorTags dataCtx) ctor
      
      -- Allocate object (this already returns the object pointer on the stack)
      allocationInstr = allocateObject tag (clTotalSize layout)
      
      -- Store fields at correct offsets  
      fieldStoreInstr = concatMap (\(paramName, fieldLayout) ->
        [ LocalGet "obj_ptr"  -- Object pointer
        , LocalGet paramName  -- Field value on stack
        , storeInstruction (flType fieldLayout) (alignmentOf (flType fieldLayout)) (flOffset fieldLayout)
        ]) (zip paramNames (clFields layout))
        
  in allocationInstr ++    -- Allocate and get obj_ptr local
     fieldStoreInstr ++    -- Store all fields
     [LocalGet "obj_ptr"]  -- Return object pointer

-- Pattern matching compilation
compilePatternMatch :: DataContext -> Text -> IR.IRPattern -> ([WASMInstruction], [WASMInstruction])
compilePatternMatch dataCtx objVar pattern = case pattern of
  IR.IRPatVar varName _type ->
    -- Variable pattern always matches
    ([], [LocalGet objVar, LocalSet varName])
    
  IR.IRPatWildcard _type ->
    -- Wildcard always matches
    ([], [])
    
  IR.IRPatLiteral lit ->
    -- Literal pattern for primitive values
    compileNamedLiteralMatch objVar lit
    
  IR.IRPatConstructor ctorName argPatterns ->
    -- Constructor pattern matching
    compileConstructorMatch dataCtx objVar ctorName argPatterns

-- Compile literal pattern matching
compileNamedLiteralMatch :: Text -> IR.IRLiteral -> ([WASMInstruction], [WASMInstruction])
compileNamedLiteralMatch objVar lit =
  let literalValue = case lit of
        IR.IRNumberLit n -> [F64Const n]
        IR.IRIntLit i -> [I32Const i]
        IR.IRBoolLit True -> [I32Const 1]
        IR.IRBoolLit False -> [I32Const 0]
        IR.IRStringLit _ -> [I32Const 0]  -- TODO: String comparison
        
      conditionCode = [LocalGet objVar] ++ literalValue ++ [F64Ne]  -- Check inequality
  in (conditionCode, [])

-- Compile constructor pattern matching
compileConstructorMatch :: DataContext -> Text -> Text -> [IR.IRPattern] -> ([WASMInstruction], [WASMInstruction])
compileConstructorMatch dataCtx objVar ctorName argPatterns =
  let expectedTag = Map.findWithDefault 0 ctorName (dcConstructorTags dataCtx)
      
      -- Check constructor tag
      conditionCode = 
        [ LocalGet objVar
        , I32Load 4 tagOffset  -- Load tag from object header
        , I32Const expectedTag
        , I32Ne  -- Check if NOT equal (for branching logic)
        ]
      
      -- Extract fields if pattern matches
      bindingCode = concatMap (\(i, argPattern) ->
        case argPattern of
          IR.IRPatVar varName _type ->
            [ LocalGet objVar
            , I32Const (objectHeaderSize + i * 4)  -- Simplified field offset
            , I32Add
            , I32Load 4 0
            , LocalSet varName
            ]
          IR.IRPatWildcard _type -> []
          _ -> []  -- TODO: Handle nested patterns
        ) (zip [0..] argPatterns)
        
  in (conditionCode, bindingCode)

-- Exhaustivity checking for pattern matching
checkExhaustiveness :: DataContext -> Text -> [IR.IRPattern] -> Bool
checkExhaustiveness dataCtx typeName patterns =
  case Map.lookup typeName (dcTypeLayouts dataCtx) of
    Nothing -> False  -- Unknown type
    Just layout ->
      let allConstructors = map clName (tlConstructors layout)
          coveredConstructors = extractCoveredConstructors patterns
      in all (`elem` coveredConstructors) allConstructors

-- Extract constructor names covered by patterns
extractCoveredConstructors :: [IR.IRPattern] -> [Text]
extractCoveredConstructors patterns =
  concatMap extractConstructorName patterns
  where
    extractConstructorName (IR.IRPatConstructor name _) = [name]
    extractConstructorName _ = []

-- Generate exhaustiveness check
generateExhaustivenessCheck :: DataContext -> Text -> [IR.IRPattern] -> [WASMInstruction]
generateExhaustivenessCheck dataCtx typeName patterns =
  if checkExhaustiveness dataCtx typeName patterns
  then []  -- Exhaustive - no check needed
  else [Unreachable]  -- Non-exhaustive - trap

-- Optimize pattern matching with decision trees
data DecisionTree
  = DTLeaf IR.IRExpr  -- Final result
  | DTSwitch Text [(Int, DecisionTree)] (Maybe DecisionTree)  -- Switch on constructor tag
  | DTTest Text IR.IRLiteral DecisionTree DecisionTree  -- Test literal value
  deriving (Show)

-- Build decision tree from pattern cases
buildDecisionTree :: DataContext -> [(IR.IRPattern, IR.IRExpr)] -> DecisionTree
buildDecisionTree dataCtx cases = case cases of
  [(IR.IRPatVar _ _, expr)] -> DTLeaf expr  -- Single variable pattern
  [(IR.IRPatWildcard _, expr)] -> DTLeaf expr  -- Single wildcard pattern
  _ -> groupByConstructor dataCtx cases

-- Group cases by constructor for efficient switching
groupByConstructor :: DataContext -> [(IR.IRPattern, IR.IRExpr)] -> DecisionTree
groupByConstructor dataCtx cases =
  let constructorCases = mapMaybe extractConstructorCase cases
      otherCases = filter (not . isConstructorPattern . fst) cases
      
      groupedCases = groupByConstructorName constructorCases
      switchCases = map (\(ctorName, subCases) ->
        let tag = Map.findWithDefault 0 ctorName (dcConstructorTags dataCtx)
            subTree = buildDecisionTree dataCtx subCases
        in (tag, subTree)
        ) groupedCases
        
      defaultCase = if null otherCases
                   then Nothing
                   else Just (buildDecisionTree dataCtx otherCases)
                   
  in DTSwitch "obj" switchCases defaultCase

-- Helper functions for decision tree building
extractConstructorCase :: (IR.IRPattern, IR.IRExpr) -> Maybe (Text, [(IR.IRPattern, IR.IRExpr)])
extractConstructorCase (IR.IRPatConstructor name args, expr) = 
  Just (name, [(IR.IRPatVar ("arg" <> T.pack (show i)) IR.IRTUnknown, expr) | i <- [0..length args - 1]])
extractConstructorCase _ = Nothing

isConstructorPattern :: IR.IRPattern -> Bool
isConstructorPattern (IR.IRPatConstructor _ _) = True
isConstructorPattern _ = False

groupByConstructorName :: [(Text, [(IR.IRPattern, IR.IRExpr)])] -> [(Text, [(IR.IRPattern, IR.IRExpr)])]
groupByConstructorName cases = 
  Map.toList $ Map.fromListWith (++) cases

-- Compile decision tree to WASM instructions
compileDecisionTree :: DataContext -> DecisionTree -> [WASMInstruction]
compileDecisionTree dataCtx tree = case tree of
  DTLeaf expr -> 
    -- TODO: Compile expression
    [F64Const 0]  -- Placeholder
    
  DTSwitch objVar switchCases defaultCase ->
    let loadTag = [LocalGet objVar, I32Load 4 tagOffset]
        caseLabels = map (("case_" <>) . T.pack . show . fst) switchCases
        defaultLabel = "default"
        
        jumpTable = [BrTable caseLabels defaultLabel]
        
        caseBlocks = concatMap (\(tag, subTree) ->
          let caseLabel = "case_" <> T.pack (show tag)
              caseCode = compileDecisionTree dataCtx subTree
          in [Block caseLabel F64 (caseCode ++ [Return])]
          ) switchCases
          
        defaultBlock = case defaultCase of
          Just subTree -> [Block defaultLabel F64 (compileDecisionTree dataCtx subTree ++ [Return])]
          Nothing -> [Block defaultLabel F64 [Unreachable]]
          
    in loadTag ++ jumpTable ++ caseBlocks ++ defaultBlock
    
  DTTest objVar literal thenTree elseTree ->
    let testCode = case literal of
          IR.IRNumberLit n -> [LocalGet objVar, F64Const n, F64Eq]
          IR.IRIntLit i -> [LocalGet objVar, I32Const i, I32Eq]
          IR.IRBoolLit True -> [LocalGet objVar, I32Const 1, I32Eq]
          IR.IRBoolLit False -> [LocalGet objVar, I32Const 0, I32Eq]
          IR.IRStringLit _ -> [I32Const 0]  -- TODO: String comparison
          
        thenCode = compileDecisionTree dataCtx thenTree
        elseCode = compileDecisionTree dataCtx elseTree
        
    in testCode ++ [If F64 thenCode elseCode]

-- Helper function to extract Maybe values
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs