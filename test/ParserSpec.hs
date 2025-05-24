{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)
import Data.Text (Text)
import qualified Data.Set as Set

import Language.Memento.Syntax
import Language.Memento.Parser (program, definitionParser, expr) -- Assuming 'expr' can parse a standalone handle expression for testing

-- Helper to run parser for a definition
parseDef :: Text -> Either (String, String) Definition
parseDef input = case parse definitionParser "" input of
    Left bundle -> Left ("Parser error", errorBundlePretty bundle)
    Right def -> Right def

-- Helper to run parser for an expression
parseExpr :: Text -> Either (String, String) Expr
parseExpr input = case parse expr "" input of
    Left bundle -> Left ("Parser error", errorBundlePretty bundle)
    Right ex -> Right ex

spec :: Spec
spec = describe "Parser" $ do
    describe "effectDefinitionParser" $ do
        it "parses a simple effect definition" $ do
            let input = "effect MyEffect [ Op1 : number -> bool ];"
            let expected = EffectDef "MyEffect" [OperatorDef "Op1" (TFunction TNumber TBool Set.empty)]
            parseDef input `shouldBe` Right expected

        it "parses an effect definition with multiple operators" $ do
            let input = "effect AnotherFx [ OpA : bool -> bool, OpB : number -> number ];"
            let expected = EffectDef "AnotherFx" [
                    OperatorDef "OpA" (TFunction TBool TBool Set.empty),
                    OperatorDef "OpB" (TFunction TNumber TNumber Set.empty)
                    ]
            parseDef input `shouldBe` Right expected

        it "parses an effect definition with no operators" $ do
            let input = "effect EmptyEffect [ ];"
            let expected = EffectDef "EmptyEffect" []
            parseDef input `shouldBe` Right expected
        
        it "parses an effect definition with complex operator types" $ do
            let input = "effect ComplexFx [ OpC : (number -> bool) -> number ];"
            let opType = TFunction (TFunction TNumber TBool Set.empty) TNumber Set.empty
            let expected = EffectDef "ComplexFx" [OperatorDef "OpC" opType]
            parseDef input `shouldBe` Right expected

    describe "handlerExprParser" $ do
        it "parses a handle expression with a return clause" $ do
            let input = "handle <MyEffect> (do SomeOp) [ val_x -> val_x ]"
            let expected = Handle
                    (Set.singleton (Effect "MyEffect"))
                    (Do "SomeOp")
                    [HandlerReturnClause "val_x" (Var "val_x")]
            parseExpr input `shouldBe` Right expected

        it "parses a handle expression with an operator clause and a return clause" $ do
            let input = "handle <MyEffect> (do SomeOp) [ (MyOp arg k_v) -> (k_v arg), val_x -> val_x ]"
            let expected = Handle
                    (Set.singleton (Effect "MyEffect"))
                    (Do "SomeOp")
                    [ HandlerClause "MyOp" "arg" "k_v" (Apply (Var "k_v") (Var "arg"))
                    , HandlerReturnClause "val_x" (Var "val_x")
                    ]
            parseExpr input `shouldBe` Right expected
        
        it "parses a handle expression with multiple operator clauses" $ do
            let input = "handle <MyEffect> (do SomeOp) [ (OpA a kA) -> a, (OpB b kB) -> b, val_ret -> val_ret ]"
            let expected = Handle
                    (Set.singleton (Effect "MyEffect"))
                    (Do "SomeOp")
                    [ HandlerClause "OpA" "a" "kA" (Var "a")
                    , HandlerClause "OpB" "b" "kB" (Var "b")
                    , HandlerReturnClause "val_ret" (Var "val_ret")
                    ]
            parseExpr input `shouldBe` Right expected

        it "parses a handle expression with empty effects list (no effects handled)" $ do
            let input = "handle <> (10) [ val_x -> val_x ]"
            let expected = Handle
                    Set.empty
                    (Number 10)
                    [HandlerReturnClause "val_x" (Var "val_x")]
            parseExpr input `shouldBe` Right expected

        it "parses a handle expression with multiple effects in the handled set" $ do
            let input = "handle <EffectA, EffectB> (do OpX) [ val_r -> val_r ]"
            let expected = Handle
                    (Set.fromList [Effect "EffectA", Effect "EffectB"])
                    (Do "OpX")
                    [HandlerReturnClause "val_r" (Var "val_r")]
            parseExpr input `shouldBe` Right expected
        
        it "parses nested handle expressions" $ do
            let input = "handle <E1> (handle <E2> (1) [val_x -> val_x]) [val_y -> val_y]"
            let innerHandle = Handle (Set.singleton (Effect "E2")) (Number 1) [HandlerReturnClause "val_x" (Var "val_x")]
            let expected = Handle (Set.singleton (Effect "E1")) innerHandle [HandlerReturnClause "val_y" (Var "val_y")]
            parseExpr input `shouldBe` Right expected
            
        it "fails to parse handle expression without clauses" $ do
            let input = "handle <E1> (1) []"
            parseExpr input `shouldSatisfy` isLeft

    describe "patternParser (via Match expression)" $ do
        let dummyType = TAlgebraicData "D" -- Dummy type for the match scrutinee
        let dummyExpr = Number 0 -- Dummy expression for the clause body

        it "parses a PNumber (integer) pattern" $ do
            let input = "match D [ 123 -> (0) ]"
            let expectedPattern = PNumber 123.0
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PNumber pattern or wrong structure: " <> show res

        it "parses a PNumber (float) pattern" $ do
            let input = "match D [ 45.67 -> (0) ]"
            let expectedPattern = PNumber 45.67
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PNumber (float) pattern or wrong structure: " <> show res

        it "parses a PBool (true) pattern" $ do
            let input = "match D [ true -> (0) ]"
            let expectedPattern = PBool True
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PBool (true) pattern or wrong structure: " <> show res

        it "parses a PBool (false) pattern" $ do
            let input = "match D [ false -> (0) ]"
            let expectedPattern = PBool False
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PBool (false) pattern or wrong structure: " <> show res

        it "parses an empty PTuple pattern" $ do
            let input = "match D [ () -> (0) ]"
            let expectedPattern = PTuple []
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PTuple () pattern or wrong structure: " <> show res
        
        it "parses a single-element PTuple pattern with PVar" $ do
            -- `patternParser `sepBy` symbol ","` means `(x)` is not a tuple
            -- but `(x,)` would be if the grammar supported trailing commas for single element tuples.
            -- The current parser for tuples `parens (patternParser `sepBy` symbol ",")`
            -- means `(x)` will be parsed as `x` inside parens, not a tuple.
            -- `(x, y)` is fine. `()` is fine.
            -- For a single element tuple to be distinct, language often uses `(x,)`.
            -- The current parser will parse `(x)` as just `PVar "x"` if used in a clause like `(x) -> 0`.
            -- If `match D [ (x) -> (0) ]` is intended to be `PTuple [PVar "x"]`, the parser needs adjustment.
            -- Based on `PTuple <$> parens (patternParser `sepBy` symbol ",")`, `(x)` is NOT a tuple.
            -- It will parse `(x, y)` correctly. For single element tuple `(x,)` is common.
            -- Assuming `(x)` is not a tuple for now as per `sepBy`.
            -- A tuple like `(x, )` is not standard in many `sepBy` uses unless explicitly handled.
            -- Let's test a PTuple with one element, which implies the parser for `clauseParser`
            -- (`pat <- parens patternParser`) or the overall structure allows it.
            -- The `patternParser` itself produces `PTuple [PVar "x"]` if input is `(x)`.
            -- No, `parens (patternParser `sepBy` symbol ",")` means `(x)` is not parsed by `sepBy` as a list.
            -- It would be `patternParser` inside `parens`.
            -- If `patternParser` is `(PVar "x")`, then `parens patternParser` would be `(PVar "x")`.
            -- The task is to test PTuple. `(x,y)` is a PTuple. `()` is PTuple [].
            -- What about `(x)` as a tuple? The `sepBy` combinator usually yields a list. If `patternParser` parses `x`,
            -- then `patternParser \`sepBy\` symbol ","` applied to input `x` (without parens) yields `[PVar "x"]`.
            -- Then `parens (...)` makes it `PTuple [PVar "x"]`.
            -- So, `(x)` should parse as `PTuple [PVar "x"]`. Let's test this.
            let input = "match D [ (x) -> (0) ]"
            let expectedPattern = PTuple [PVar "x"]
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PTuple (x) pattern or wrong structure: " <> show res


        it "parses a multi-element PTuple pattern" $ do
            let input = "match D [ (1, true, x) -> (0) ]"
            let expectedPattern = PTuple [PNumber 1.0, PBool True, PVar "x"]
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PTuple (1, true, x) pattern or wrong structure: " <> show res

        it "parses a nested PTuple pattern" $ do
            let input = "match D [ ((1, true), x) -> (0) ]"
            let expectedPattern = PTuple [PTuple [PNumber 1.0, PBool True], PVar "x"]
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PTuple ((1, true), x) pattern or wrong structure: " <> show res
        
        it "parses PWildcard inside PTuple" $ do
            let input = "match D [ (_, true) -> (0) ]"
            let expectedPattern = PTuple [PWildcard, PBool True]
            case parseExpr input of
                Right (Match _ [Clause pat _]) -> pat `shouldBe` expectedPattern
                res -> expectationFailure $ "Failed to parse PTuple (_, true) pattern or wrong structure: " <> show res

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
