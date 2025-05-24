{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Definitions (
  generateDefinition,
  generateConstructorWrapperFunctions,
  getConstructorArity_cg,
  isDataDef,
  isValDef,
  generateConstructorsForAdt,
  generateSingleConstructorWrapper
) where

import qualified Data.Text as T
import Language.Memento.Syntax (Definition (..), ConstructorDef (..), Type (..))
import Language.Memento.Codegen.Expressions (generateExpr) -- For ValDef body

-- Helper to check if a Definition is ValDef
isValDef :: Definition -> Bool
isValDef (ValDef _ _ _) = True
isValDef _ = False

-- Helper to check if a Definition is DataDef
isDataDef :: Definition -> Bool
isDataDef (DataDef _ _) = True
isDataDef _ = False

-- Helper to get constructor arity for codegen
getConstructorArity_cg :: Type -> Int
getConstructorArity_cg typ =
  case typ of
    TFunction _ (retType, _) -> 1 + getConstructorArity_cg retType
    _ -> 0

generateSingleConstructorWrapper :: ConstructorDef -> T.Text
generateSingleConstructorWrapper (ConstructorDef consName consTypeSyntax) =
  let arity = getConstructorArity_cg consTypeSyntax
      argNames = map (\i -> T.pack ("arg" ++ show i)) [1 .. arity]
      jsArgs = T.intercalate ", " argNames
      -- Construct the payload part of the array: "arg1, arg2, ..."
      -- If no args, this is empty. If args, it starts with a comma.
      jsPayload = if null argNames then T.empty else T.cons ',' (T.intercalate ", " argNames)
   in T.concat
        [ "const "
        , consName
        , " = "
        , T.concat (map (rgName -> T.pack "(" `T.append` argName `T.append` T.pack ") => ") argNames)
        , "ret(["" -- Opening escaped quote for constructor name string literal in JS
        , consName
        , """ -- Closing escaped quote for constructor name string literal in JS
        , jsPayload -- Will be empty or like ", arg1, arg2"
        , "]);"
        ]

generateConstructorsForAdt :: Definition -> [T.Text]
generateConstructorsForAdt (DataDef _ consDefs) =
  map generateSingleConstructorWrapper consDefs
generateConstructorsForAdt _ = [] -- Should not happen if filtered by isDataDef

generateConstructorWrapperFunctions :: [Definition] -> T.Text
generateConstructorWrapperFunctions defs =
  let dataDefs = filter isDataDef defs
   in T.intercalate "
" (concatMap generateConstructorsForAdt dataDefs)

generateDefinition :: Definition -> T.Text
generateDefinition (ValDef name _ expr) =
  -- Ignoring type and effects for codegen
  -- The result of generateExpr is a computation; it needs to be wrapped if it's a top-level def.
  -- Original code: T.concat ["const ", name, " = (", generateExpr expr, ")[1];"]
  -- This implies the generated expr is immediately invoked or is a string that needs to be evaluated.
  -- generateExpr returns a string like "ret(...)" or "bind(...)".
  -- If `main = ret(42)`, then `const main = (ret(42))[1];` -> `const main = (["ret", 42])[1];` -> `const main = 42;`
  -- This seems plausible for ValDefs that are simple values.
  -- If it's a function `val f : number -> number := x -> x + 1;`
  -- generateExpr (Lambda "x" Nothing (BinOp Add (Var "x") (Number 1))) would be:
  -- ret((x) => { return bind(ret(x), (v1) => bind(ret(1), (v2) => ret(v1 + v2))) })
  -- So, `const f = (ret((x) => { ... }))[1];` which is `const f = (x) => { ... };`
  -- This is correct.
  -- However, the original code `nameToExecute,"()"` in finalExecutionBlock implies that ValDefs are functions to be called.
  -- `const main = (ret(42))[1];` makes `main` = 42. Then `main()` is an error.
  -- If `ValDef` is meant to produce a computation that needs to be run, like `main = do Add 1 2`,
  -- `generateExpr expr` yields the string for that computation.
  -- `const main = nameToExecute();` where `nameToExecute` is the text from `generateExpr expr`.
  -- Let's re-evaluate `finalExecutionBlock` in `Codegen.hs` later.
  -- For now, stick to the original ValDef generation.
  -- The `console.log(globalHandler(main()));` suggests `main` itself should be a function that returns a computation.
  -- So, `ValDef name _ expr` should be `const name = () => generateExpr expr;`
  -- Let's make this change as it seems more consistent with the execution model.
  T.concat ["const ", name, " = () => ", generateExpr expr, ";"]
generateDefinition _ = T.empty -- DataDef and EffectDef don't generate top-level JS in this function
