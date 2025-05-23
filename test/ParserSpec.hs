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
            
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
