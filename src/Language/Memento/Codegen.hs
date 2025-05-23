{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Syntax (BinOp (..), Clause (..), ConstructorDef (..), Definition (..), Expr (..), HandlerClause (..), Pattern (..), Program (..), Type (..)) -- Updated imports

baseDefinitions :: Text
baseDefinitions =
  T.unlines
    [ "// ベース関数群"
    , "const ret = (x) => [\"ret\", x];"
    , ""
    , "// モナディックなbind関数"
    , "const bind = (c, f) => {"
    , "  if (c[0] === \"ret\") {"
    , "    return f(c[1]);"
    , "  } else {"
    , "    return [\"op\", c[1], c[2], (x) => bind(c[3](x), f)];"
    , "  }"
    , "};"
    , ""
    , "// グローバルエフェクトハンドラ"
    , "const globalHandler = (c) => {"
    , "  if (c[0] === \"ret\") {"
    , "    return [\"Success\", c[1]];"
    , "  } else {"
    , "    switch(c[1] /* name */) {"
    , "      case \"throw\":"
    , "        return [\"Failure\", c[2]];"
    , "      default:"
    , "        return [\"Error\", \"Unknown operation: \" + c[1]];"
    , "    }"
    , "  }"
    , "};"
    ]

-- | JavaScriptコードの生成
generateJS :: Program -> Text
generateJS (Program definitions) =
  T.concat
    [ "'use strict';\n\n"
    , baseDefinitions
    , "\n\n// Generated ADT Constructor functions\n"
    , generateConstructorWrapperFunctions definitions
    , "\n\n// Generated Value definitions\n"
    , -- EffectDef and DataDef (other than their constructor wrappers) do not produce direct top-level JS definitions here.
      -- ValDefs are assumed to be computations that result in a value after global handling.
      T.intercalate "\n\n" (map generateDefinition (filter isValDef definitions))
    , finalExecutionBlock
    ]
 where
  isValDef :: Definition -> Bool
  isValDef (ValDef _ _ _) = True
  isValDef _ = False

  generateDefinition :: Definition -> Text
  generateDefinition (ValDef name _ expr) =
    -- Ignoring type and effects for codegen
    T.concat ["const ", name, " = (", generateExpr expr, ")[1];"]

  finalExecutionBlock :: Text
  finalExecutionBlock =
    let valDefs = filter isValDef definitions
     in if null valDefs
          then "\n\n// No ValDefs found to execute."
          else
            let lastDefName = (\(ValDef name _ _) -> name) (last valDefs)
                mainDefExists = any (\(ValDef name _ _) -> name == "main") valDefs
                nameToExecute = if mainDefExists then "main" else lastDefName
             in T.concat ["\n\nconsole.log(globalHandler(", nameToExecute, "()));"]

-- Helper to get constructor arity for codegen
getConstructorArity_cg :: Type -> Int
getConstructorArity_cg typ =
  case typ of
    TFunction argType (retType, _) -> 1 + getConstructorArity_cg retType
    _ -> 0

generateConstructorWrapperFunctions :: [Definition] -> Text
generateConstructorWrapperFunctions defs =
  let dataDefs = filter isDataDef defs
   in T.intercalate "\n" (concatMap generateConstructorsForAdt dataDefs)

isDataDef :: Definition -> Bool
isDataDef (DataDef _ _) = True
isDataDef _ = False

generateConstructorsForAdt :: Definition -> [Text]
generateConstructorsForAdt (DataDef _ consDefs) =
  map generateSingleConstructorWrapper consDefs
generateConstructorsForAdt _ = [] -- Should not happen if filtered by isDataDef

generateSingleConstructorWrapper :: ConstructorDef -> Text
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
        , T.concat (map (\argName -> T.pack "(" `T.append` argName `T.append` T.pack ") => ") argNames)
        , "ret([\""
        , consName
        , "\"" -- Closing quote for constructor name string literal
        , jsPayload -- Will be empty or like ", arg1, arg2"
        , "]);"
        ]

{- | 式のJavaScriptコードの生成
新しいセマンティクス: エフェクトシステムを使用したコード生成
-}
generateExpr :: Expr -> Text
generateExpr = \case
  -- 基本値
  Var name -> T.concat ["ret(", name, ")"] -- MODIFIED
  Number n -> T.concat ["ret(", T.pack $ show n, ")"]
  Bool b -> T.concat ["ret(", T.pack $ if b then "true" else "false", ")"]
  -- 二項演算子
  BinOp op e1 e2 ->
    let expr1 = generateExpr e1
        expr2 = generateExpr e2
     in T.unlines
          [ "bind(" <> expr1 <> ","
          , "  (v1) => bind(" <> expr2 <> ","
          , "    (v2) => ret(v1 " <> generateBinOp op <> " v2)"
          , "  )"
          , ")"
          ]
  -- [if cond then x else y] = bind(cond, (c) => c ? x : y)
  If cond then_ else_ ->
    let condExpr = generateExpr cond
        thenExpr = generateExpr then_
        elseExpr = generateExpr else_
     in T.unlines
          [ "bind(" <> condExpr <> ","
          , "  (c) => c"
          , "    ? " <> thenExpr
          , "    : " <> elseExpr
          , ")"
          ]
  -- [Lambda x. c] = ret((x) => [c])
  Lambda name _ body ->
    let bodyExpr = generateExpr body
     in T.unlines
          [ "ret((" <> name <> ") => {"
          , "  return " <> bodyExpr <> ";"
          , "})"
          ]
  HandleApply handler arg ->
    let handlerExpr = generateExpr handler
        argExpr = generateExpr arg
     in T.unlines
          [ "bind("
          , handlerExpr
          , ", (h) => h("
          , argExpr
          , "))"
          ]
  -- [Apply f x] = bind([x], (v) => bind([f], (f) => f(v)))
  Apply func arg ->
    let funcExpr = generateExpr func
        argExpr = generateExpr arg
     in T.unlines
          [ "bind("
          , argExpr
          , ", (v) => bind("
          , funcExpr
          , ", (f) => f(v)))"
          ]
  -- do演算 - エフェクトを表現
  Do name ->
    T.concat ["ret((v) => [\"op\", \"", name, "\", v, (v) => ret(v)])"]
  -- Match expression
  Match adtType clauses ->
    let matchArg = "_matched_val_" -- Name for the JS function argument
        generatedClauses = map (generateClause matchArg) clauses
        fallbackLogic = T.concat [T.pack "console.error('Non-exhaustive patterns for ", T.pack (show adtType), T.pack " with value:', ", T.pack matchArg, T.pack "); ", T.pack "throw new Error('Pattern match failure: Non-exhaustive or malformed ADT value for ", T.pack (show adtType), T.pack "');"]
     in T.unlines
          [ T.pack "ret((" <> T.pack matchArg <> T.pack ") => {"
          , T.pack "  let _result;"
          , T.intercalate
              (T.pack " else ")
              ( map
                  ( \(cond, bindingsBlock, branchBlock) ->
                      T.unlines
                        [ T.concat [T.pack "  if (", cond, T.pack ") {"]
                        , bindingsBlock
                        , T.concat [T.pack "    _result = ", branchBlock, T.pack ";"]
                        , T.pack "  }"
                        ]
                  )
                  generatedClauses
              )
          , T.pack "  else {"
          , T.pack "    " <> fallbackLogic
          , T.pack "  }"
          , T.pack "  return _result;"
          , T.pack "})"
          ]
  Handle _ handlerClauses ->
    let (opClauses, returnClauses) = partitionHandlerClauses handlerClauses

        -- Generate JS for the return clause (assuming exactly one for simplicity here, TypeChecker ensures at least one)
        -- If multiple return clauses were possible and meaningful, logic would need to select/merge.
        -- For now, take the first one. The type checker should ensure all return clauses unify to the same type.
        returnClauseJs = case returnClauses of
          (HandlerReturnClause retVar bodyExpr : _) ->
            let bodyJs = generateExpr bodyExpr
             in T.unlines
                  [ "    const " <> retVar <> " = _handled_val_[1];" -- Bind retVar to the unwrapped value
                  , "    return " <> bodyJs <> ";" -- Execute body
                  ]
          _ -> "// Should be caught by type checker: No return clause found\n    return [\"op\", \"error\", \"No return clause\", (v) => ret(v)];" -- Fallback, should not happen

        -- Generate JS for operator clauses as a switch or if-else-if chain
        opClausesJs =
          if null opClauses
            then T.pack "      // No operator clauses\n"
            else
              T.concat $
                map
                  ( \(HandlerClause opName argVar contVar bodyExpr) ->
                      let bodyJs = generateExpr bodyExpr
                       in T.unlines
                            [ "      if (_op_name_ === \"" <> opName <> "\") {"
                            , "        const " <> argVar <> " = _op_arg_;"
                            , "        const " <> contVar <> " = _op_cont_;"
                            , "        return " <> bodyJs <> ";"
                            , "      }"
                            ]
                  )
                  opClauses

        -- Default case for operations not handled by specific clauses
        defaultOpCaseJs = "      else {\n        return [\"op\", _op_name_, _op_arg_, _op_cont_]; // Re-perform unhandled op\n      }"
     in T.unlines
          [ "ret(function _handle_(_handled_val_){"
          , "  if (_handled_val_[0] === \"ret\") {"
          , returnClauseJs
          , "  } else { // Operation"
          , "    const _op_name_ = _handled_val_[1];"
          , "    const _op_arg_ = _handled_val_[2];"
          , "    const _op_cont_ = (v) => _handle_(_handled_val_[3](v));"
          , opClausesJs
          , defaultOpCaseJs
          , "  }"
          , "})"
          ]

-- Helper to partition HandlerClauses (if not already available or to keep logic local)
-- This is a simple version; a more robust one might be in Syntax or a Util module.
partitionHandlerClauses :: [HandlerClause] -> ([HandlerClause], [HandlerClause])
partitionHandlerClauses clauses = go clauses ([], [])
 where
  go [] acc = acc
  go (c : cs) (ops, rets) = case c of
    HandlerClause{} -> go cs (ops ++ [c], rets)
    HandlerReturnClause{} -> go cs (ops, rets ++ [c])

generateClause :: String -> Clause -> (Text, Text, Text) -- (condition, bindingsJs, branchJs)
generateClause matchArgName (Clause pattern branchExpr) =
  -- _adtName is not used here
  let (condition, bindings) = generatePattern (T.pack matchArgName) pattern
      bindingsJs = generateBindings bindings
      branchJs = generateExpr branchExpr
   in (condition, bindingsJs, branchJs)

generatePattern :: Text -> Pattern -> (Text, [(Text, Text)]) -- (condition, [(varName, valueAccess)])
generatePattern scrutineeExpr (PConstructor consName varNames) =
  let tagCheck = T.concat [scrutineeExpr, T.pack "[0] === \"", consName, T.pack "\""]
      arity = length varNames
      arityCheck = T.concat [scrutineeExpr, T.pack ".length === ", T.pack (show (arity + 1))]
      fullCondition = T.concat [tagCheck, T.pack " && ", arityCheck]
      bindings = map (\(i, varName) -> (varName, T.concat [scrutineeExpr, T.pack "[", T.pack (show (i + 1)), T.pack "]"])) (zip [0 ..] varNames)
   in (fullCondition, bindings)
generatePattern scrutineeExpr (PVar varName) =
  (T.pack "true", [(varName, scrutineeExpr)])
generatePattern _ PWildcard =
  (T.pack "true", [])

generateBindings :: [(Text, Text)] -> Text
generateBindings bindings =
  if null bindings
    then T.empty
    else T.unlines (map (\(var, val) -> T.concat [T.pack "    const ", var, T.pack " = ", val, T.pack ";"]) bindings)

-- | 二項演算子の生成
generateBinOp :: BinOp -> Text
generateBinOp = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Eq -> "==="
  Lt -> "<"
  Gt -> ">"