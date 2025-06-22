{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Codegen.Expressions where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (List)
import Language.Memento.Codegen.BinOps (genBinOp)
import Language.Memento.Codegen.Core (genConstDef, genConstDefRaw, genVariable)
import Language.Memento.Codegen.Literal (genLiteral)
import Language.Memento.Codegen.Pattern (genPattern)
import Language.Memento.Data.HFix (HFix (unHFix))
import Language.Memento.Data.HProduct
import Language.Memento.Syntax (AST, unExpr, unLet)
import Language.Memento.Syntax.Expr (Expr (..), Let (Let))
import Language.Memento.Syntax.Tag (KBinOp, KExpr, KLet, KPattern, KType)

genExpr :: AST KExpr -> Text
genExpr astE = case unHFix astE of
  meta :*: stx -> case unExpr stx of
    EVar v -> genVariable v
    ELiteral l -> genLiteral l
    ELambda ps e -> genLambda ps e
    EApply e es -> genApply e es
    EMatch es ps -> genMatch es ps
    EIf e e1 e2 -> genIf e e1 e2
    EBinOp op e1 e2 -> genBinOpExpr op e1 e2
    EBlock lets e -> genBlock lets e

genLambda :: List (AST KPattern, Maybe (AST KType)) -> AST KExpr -> Text
genLambda ps e =
  let
    genArg :: Int -> Text
    genArg n = "_ARG_" <> T.pack (show n) <> "_"
    args = zipWith (\(p, mt) n -> genArg n) ps [0 ..]
   in
    "("
      <> T.intercalate ", " args
      <> ") => {"
      <> T.unlines
        ( map
            (uncurry genConstDef)
            ( concat $
                zipWith
                  ( \(p, mt) n ->
                      snd $ -- 定義だけ取り出す
                        genPattern p (genArg n) -- _ARG_n_ をもとにしてパターンを解釈
                  )
                  ps
                  [0 ..]
            )
        )
      <> "return "
      <> genExpr e
      <> ";}"

genApply :: AST KExpr -> List (AST KExpr) -> Text
genApply e es = genExpr e <> "(" <> T.intercalate ", " (map genExpr es) <> ")"

genLet :: AST KPattern -> Maybe (AST KType) -> AST KExpr -> Int -> Text
genLet p t e n =
  let
    -- 一旦 const _ARG_n_ = e としてから、パターンでのスプリットを導入
    -- すると返ってくる定義は (pred, bindings) で bindings = [(v, _ARG_n_.hoge) ...] みたいな形になる
    -- 返ってきた bindings が一つの場合は最初の _ARG_n_ は無駄であるのでそれは使わず、パターンに直接 e を流したものを採用する。
    (pred, bindings) = genPattern p ("_ARG_" <> T.pack (show n) <> "_")
   in
    case bindings of
      [b] ->
        let
          (pred', bindings') = genPattern p $ genExpr e
         in
          T.unlines $ map (uncurry genConstDef) bindings'
      _ ->
        T.unlines $
          (genConstDefRaw ("_ARG_" <> T.pack (show n) <> "_") (genExpr e))
            : map (uncurry genConstDef) bindings

genMatch :: List (AST KExpr) -> List (List (AST KPattern, Maybe (AST KType)), AST KExpr) -> Text
genMatch es clauses =
  let
    -- Generate names for each scrutinee expression
    argName :: Int -> Text
    argName n = "_ARG_" <> T.pack (show n) <> "_"

    -- const _ARG_n_ = <expr>;
    scrutineeDefs :: [Text]
    scrutineeDefs = zipWith (\e n -> genConstDefRaw (argName n) (genExpr e)) es [0 ..]

    -- Generate code for each clause
    genClause :: ((List (AST KPattern, Maybe (AST KType)), AST KExpr), Int) -> Text
    genClause ((patsWithTypes, bodyExpr), _) =
      let
        -- Extract only patterns, ignore types
        patterns = map fst patsWithTypes

        -- For each (pattern, scrutinee), get (conds, binds)
        results = zipWith (\p n -> genPattern p (argName n)) patterns [0 ..]
        conds = concatMap fst results
        bindings = concatMap snd results

        condExpr :: Text
        condExpr = T.intercalate " && " conds

        bindingsCode :: Text
        bindingsCode = T.unlines $ map (uncurry genConstDef) bindings

        bodyCode = "return " <> genExpr bodyExpr <> ";"
       in
        -- Assemble clause depending on whether we have conditions

        if null conds
          then bindingsCode <> bodyCode
          else
            "if (" <> condExpr <> ") {\n" <> bindingsCode <> bodyCode <> "}\n"

    clausesCode = T.concat $ map genClause (zip clauses [0 ..])
   in
    "(() => {\n"
      <> T.unlines scrutineeDefs
      <> clausesCode
      <> "throw new Error(\"Non-exhaustive pattern match\");\n" -- fallback
      <> "})()"

genIf :: AST KExpr -> AST KExpr -> AST KExpr -> Text
genIf e e1 e2 =
  "(" <> genExpr e <> " ? " <> genExpr e1 <> " : " <> genExpr e2 <> ")"

genBinOpExpr :: AST KBinOp -> AST KExpr -> AST KExpr -> Text
genBinOpExpr op e1 e2 =
  "(" <> genExpr e1 <> " " <> genBinOp op <> " " <> genExpr e2 <> ")"

genBlock :: List (AST KLet) -> AST KExpr -> Text
genBlock lets lastE =
  let
    genedLets =
      zipWith
        ( \letAST n -> case unHFix letAST of
            meta :*: letStx -> case unLet letStx of
              Let p t e -> genLet p t e n
        )
        lets
        [0 ..]
   in
    "(() => {" <> T.unlines genedLets <> "return " <> genExpr lastE <> ";" <> "})()"