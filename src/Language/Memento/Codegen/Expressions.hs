{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Expressions (
  generateExpr,
  generateBinOp,
  generateClause,
  generatePattern,
  generateBindings,
  partitionHandlerClausesForCodegen, -- Renamed to avoid conflict if Syntax.partitionHandlerClauses is different
) where

import qualified Data.Text as T
import Language.Memento.Syntax (BinOp (..), Clause (..), Expr (..), HandlerClause (..), Pattern (..), Type (..)) -- Added Type for Match

-- | Helper to partition HandlerClauses (specific to Codegen's needs)
partitionHandlerClausesForCodegen :: [HandlerClause] -> ([HandlerClause], [HandlerClause])
partitionHandlerClausesForCodegen clauses = go clauses ([], [])
 where
  go [] acc = acc
  go (c : cs) (ops, rets) = case c of
    HandlerClause{} -> go cs (ops ++ [c], rets)
    HandlerReturnClause{} -> go cs (ops, rets ++ [c])

generatePattern :: T.Text -> Pattern -> ([T.Text], [(T.Text, T.Text)]) -- (condition, [(varName, valueAccess)])
generatePattern scrutineeExpr = \case
  PNumber n ->
    ([scrutineeExpr, " === ", T.pack (show n)], [])
  PBool b ->
    ([scrutineeExpr, " === ", T.pack (if b then "true" else "false")], [])
  PTuple subPatterns ->
    let processSubPattern index pat =
          let elementScrutinee = T.concat [scrutineeExpr, "[", T.pack (show index), "]"]
           in generatePattern elementScrutinee pat

        results = zipWith processSubPattern [0 ..] subPatterns

        subConditions = map fst results
        subBindingsList = map snd results

        allBindings = concat subBindingsList
     in (concat subConditions, allBindings)
  PConstructor consName pattern ->
    -- Assuming ADT representation: [constructorName, value]
    let
      condition = T.concat [scrutineeExpr, "[0] === \"", consName, "\""]
      (subCondition, bindings) = generatePattern (T.concat [scrutineeExpr, "[1]"]) pattern
     in
      ([condition] <> subCondition, bindings)
  PVar varName ->
    ([], [(varName, scrutineeExpr)])
  PWildcard ->
    ([], [])

generateBindings :: [(T.Text, T.Text)] -> T.Text
generateBindings bindings =
  if null bindings
    then T.empty
    else T.unlines (map (\(var, val) -> T.concat [T.pack "    const ", var, T.pack " = ", val, T.pack ";"]) bindings)

generateClause :: String -> Clause -> ([T.Text], T.Text, T.Text) -- (condition, bindingsJs, branchJs)
generateClause matchArgName (Clause pattern branchExpr) =
  let (condition, bindings) = generatePattern (T.pack matchArgName) pattern
      bindingsJs = generateBindings bindings
      branchJs = generateExpr branchExpr -- generateExpr is from this module
   in (condition, bindingsJs, branchJs)

generateExpr :: Expr -> T.Text
generateExpr = \case
  Var name -> T.concat ["ret(", name, ")"]
  Number n -> T.concat ["ret(", T.pack $ show n, ")"]
  Bool b -> T.concat ["ret(", T.pack $ if b then "true" else "false", ")"]
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
  Tuple es ->
    let exprs = map generateExpr es
        exprsWithIndex = zip exprs [0 ..]
        tupleVal idx = "_tuple_val_" <> T.pack (show idx) <> "_"
        buildTupleExpr :: (T.Text, Int) -> T.Text -> T.Text
        buildTupleExpr (expr, idx) acc =
          "bind(" <> expr <> ", (" <> tupleVal idx <> ") =>" <> acc <> ")"
     in foldr
          buildTupleExpr
          ( "ret(["
              <> T.intercalate
                ","
                (map (\(_, idx) -> tupleVal idx) exprsWithIndex)
              <> "])"
          )
          exprsWithIndex
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
  Lambda pattern _ body ->
    let lambdaArgName = "_lambda_arg" -- A fixed name for the lambda's argument
        (conditionJs, bindingsList) = generatePattern lambdaArgName pattern
        bindingsJs = generateBindings bindingsList
        bodyJs = generateExpr body
     in T.unlines
          [ "ret(function(" <> lambdaArgName <> ") {"
          , "  if (!(" <> (if null conditionJs then "true" else T.intercalate " && " conditionJs) <> ")) {"
          , "    throw new Error('Lambda argument pattern mismatch. Value: ' + JSON.stringify(" <> lambdaArgName <> "));"
          , "  }"
          , bindingsJs
          , "  return " <> bodyJs <> ";"
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
  Do name ->
    T.concat ["ret((v) => [\"op\", \"", name, "\", v, (v) => ret(v)])"]
  Match adtType clauses ->
    -- adtType is Type from Syntax.hs
    let matchArg = "_matched_val_"
        generatedClauses = map (generateClause matchArg) clauses
        -- Use T.pack (show adtType) for error message
        fallbackLogic = T.concat ["console.error('Non-exhaustive patterns for ", T.pack (show adtType), " with value: ', ", T.pack matchArg, "); ", "throw new Error('Pattern match failure: Non-exhaustive or malformed ADT value for ", T.pack (show adtType), "');"]
     in T.unlines
          [ T.pack "ret((" <> T.pack matchArg <> T.pack ") => {"
          , T.pack "  let _result;"
          , T.intercalate
              (T.pack " else ")
              ( map
                  ( \(conds, bindingsBlock, branchBlock) ->
                      T.unlines
                        [ T.concat [T.pack "  if (", if null conds then "true" else T.intercalate " && " conds, T.pack ") {"]
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
    -- First arg of Handle is Type, ignored in codegen
    let (opClauses, returnClauses) = partitionHandlerClausesForCodegen handlerClauses
        returnClauseJs = case returnClauses of
          (HandlerReturnClause retVar bodyExpr : _) ->
            let bodyJs = generateExpr bodyExpr
             in T.unlines
                  [ "    const " <> retVar <> " = _handled_val_[1];"
                  , "    return " <> bodyJs <> ";"
                  ]
          _ -> "// Should be caught by type checker: No return clause found\n"
        opClausesJs =
          if null opClauses
            then T.pack "      // No operator clauses\n"
            else
              T.concat $
                map
                  ( \(HandlerClause opName argVar contVar bodyExpr) ->
                      let bodyJs = generateExpr bodyExpr
                       in T.unlines
                            [ T.concat ["      if (_op_name_ === \"", opName, "\") {"]
                            , "        const " <> argVar <> " = _op_arg_;"
                            , "        const " <> contVar <> " = _op_cont_;"
                            , "        return " <> bodyJs <> ";"
                            , "      }"
                            ]
                  )
                  opClauses
        defaultOpCaseJs = "      else {\n        return [\"op\", _op_name_, _op_arg_, _op_cont_];\n      }"
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

generateBinOp :: BinOp -> T.Text
generateBinOp = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Eq -> "==="
  Lt -> "<"
  Gt -> ">"
