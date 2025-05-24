{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse, parseTest, eof, ParseErrorBundle, ParseError)
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Void (Void)

import Language.Memento.Syntax
import Language.Memento.Parser (program, definitionParser, expr, patternParser) -- Added patternParser
import Language.Memento.Parser.Core (
    lowerCamelCaseIdentifier,
    pascalCaseIdentifier,
    snakeCaseIdentifier,
    reservedWords, -- for testing reserved word rejection
    Parser,
    sc -- to consume trailing spaces if any
    )

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

-- Helper for testing core identifier parsers
parsesTo :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
parsesTo p input expected = parse (p <* eof) "" input `shouldParse` expected

shouldFail :: (Show a) => Parser a -> Text -> Expectation
shouldFail p input = parse (p <* eof) "" input `shouldFailOn` undefined -- We don't care about specific error for these tests

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

spec :: Spec
spec = describe "Parser" $ do
    -- Tests for Core Identifier Parsers
    describe "Core Identifier Parsers" $ do
        let rWords = reservedWords -- assuming reservedWords is exported from Core
        
        describe "lowerCamelCaseIdentifier" $ do
            it "parses valid lowerCamelCase identifiers" $ do
                lowerCamelCaseIdentifier `parsesTo` "myVariable" $ "myVariable"
                lowerCamelCaseIdentifier `parsesTo` "a" $ "a"
                lowerCamelCaseIdentifier `parsesTo` "anotherVar123" $ "anotherVar123"
            it "rejects PascalCase identifiers" $ do
                lowerCamelCaseIdentifier `shouldFail` "MyVariable"
            it "rejects snake_case identifiers" $ do
                lowerCamelCaseIdentifier `shouldFail` "my_variable"
            it "rejects identifiers starting with a number" $ do
                lowerCamelCaseIdentifier `shouldFail` "1variable"
            mapM_ (\rw -> it ("rejects reserved word: " ++ show rw) $ lowerCamelCaseIdentifier `shouldFail` rw) rWords

        describe "pascalCaseIdentifier" $ do
            it "parses valid PascalCase identifiers" $ do
                pascalCaseIdentifier `parsesTo` "MyVariable" $ "MyVariable"
                pascalCaseIdentifier `parsesTo` "A" $ "A"
                pascalCaseIdentifier `parsesTo` "AnotherVar123" $ "AnotherVar123"
            it "rejects lowerCamelCase identifiers" $ do
                pascalCaseIdentifier `shouldFail` "myVariable"
            it "rejects snake_case identifiers" $ do
                pascalCaseIdentifier `shouldFail` "my_variable"
            it "rejects identifiers starting with a lowercase letter or number" $ do
                pascalCaseIdentifier `shouldFail` "mVariable"
                pascalCaseIdentifier `shouldFail` "1Variable"
            mapM_ (\rw -> it ("rejects reserved word: " ++ show rw) $ pascalCaseIdentifier `shouldFail` rw) rWords

        describe "snakeCaseIdentifier" $ do
            it "parses valid snake_case identifiers" $ do
                snakeCaseIdentifier `parsesTo` "my_variable" $ "my_variable"
                snakeCaseIdentifier `parsesTo` "a" $ "a"
                snakeCaseIdentifier `parsesTo` "another_var_123" $ "another_var_123"
                snakeCaseIdentifier `parsesTo` "var1" $ "var1"
            it "rejects PascalCase identifiers" $ do
                snakeCaseIdentifier `shouldFail` "MyVariable"
            it "rejects lowerCamelCase identifiers" $ do
                snakeCaseIdentifier `shouldFail` "myVariable"
            it "rejects leading underscores" $ do
                snakeCaseIdentifier `shouldFail` "_my_variable"
            it "rejects trailing underscores" $ do
                snakeCaseIdentifier `shouldFail` "my_variable_"
            it "rejects consecutive underscores" $ do
                snakeCaseIdentifier `shouldFail` "my__variable"
            it "rejects identifiers starting with a number" $ do
                snakeCaseIdentifier `shouldFail` "1_variable"
            mapM_ (\rw -> it ("rejects reserved word: " ++ show rw) $ snakeCaseIdentifier `shouldFail` rw) rWords
            
    describe "effectDefinitionParser" $ do -- This is an existing describe block
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
        
        -- New Naming Convention Tests for effectDefinitionParser
        it "rejects snake_case effect name" $ do
            let input = "effect my_effect [ Op1 : number -> bool ];"
            parseDef input `shouldSatisfy` isLeft
        it "rejects lowerCamelCase effect name" $ do
            let input = "effect myEffect [ Op1 : number -> bool ];"
            parseDef input `shouldSatisfy` isLeft
        it "rejects snake_case operator name in effect definition" $ do
            let input = "effect MyEffect [ op_1 : number -> bool ];"
            parseDef input `shouldSatisfy` isLeft
        it "rejects lowerCamelCase operator name in effect definition" $ do
            let input = "effect MyEffect [ opOne : number -> bool ];"
            parseDef input `shouldSatisfy` isLeft

    describe "Definition Parsing (Naming Conventions)" $ do
        describe "valDefinitionParser" $ do
            it "parses val definition with snake_case variable name" $ do
                let input = "val my_var : number := 10;"
                let expected = ValDef "my_var" TNumber (Number 10)
                parseDef input `shouldBe` Right expected
            it "rejects val definition with PascalCase variable name" $ do
                let input = "val MyVar : number := 10;"
                parseDef input `shouldSatisfy` isLeft
            it "rejects val definition with lowerCamelCase variable name" $ do
                let input = "val myVar : number := 10;"
                parseDef input `shouldSatisfy` isLeft

        describe "dataDefinitionParser" $ do
            it "parses data definition with PascalCase type and constructor names" $ do
                -- Adjusted Nil to be `Nil : MyList` as per constructorDefinitionParser structure
                let input = "data MyList [ Nil : MyList, Cons : number -> MyList ];"
                let nilType = TAlgebraicData "MyList"
                let consType = TFunction TNumber (TAlgebraicData "MyList") Set.empty
                let expected = DataDef "MyList" [ConstructorDef "Nil" nilType, ConstructorDef "Cons" consType]
                parseDef input `shouldBe` Right expected

            it "rejects data definition with snake_case type name" $ do
                let input = "data my_list [ Nil, Cons : number -> my_list ];"
                parseDef input `shouldSatisfy` isLeft
            it "rejects data definition with lowerCamelCase type name" $ do
                let input = "data myList [ Nil, Cons : number -> myList ];"
                parseDef input `shouldSatisfy` isLeft
            it "rejects data definition with snake_case constructor name" $ do
                let input = "data MyList [ nil_constructor, Cons : number -> MyList ];"
                parseDef input `shouldSatisfy` isLeft
            it "rejects data definition with lowerCamelCase constructor name" $ do
                let input = "data MyList [ nilConstructor, Cons : number -> MyList ];"
                parseDef input `shouldSatisfy` isLeft
    
    describe "Type Parsing Conventions" $ do
        it "parses algebraic data type with PascalCase name in type expression" $ do
            parseExpr "MyType" `shouldParse` TAlgebraicData "MyType"
        it "rejects snake_case algebraic data type name in type expression" $ do
            parse expr "" "my_type" `shouldFailOn` undefined
        it "rejects lowerCamelCase algebraic data type name in type expression" $ do
            parse expr "" "myType" `shouldFailOn` undefined

        it "parses effect type with PascalCase name in function type signature" $ do
            let input = "number -> bool with <MyEffect>"
            let expected = TFunction TNumber TBool (Set.singleton (Effect "MyEffect"))
            parseExpr input `shouldParse` expected
        it "rejects snake_case effect name in function type signature" $ do
            parse expr "" "number -> bool with <my_effect>" `shouldFailOn` undefined
        it "rejects lowerCamelCase effect name in function type signature" $ do
            parse expr "" "number -> bool with <myEffect>" `shouldFailOn` undefined

    describe "Pattern Parsing Conventions" $ do
        -- patternParser is now imported
        it "parses PConstructor with PascalCase constructor and snake_case variable" $ do
            parse (patternParser <* eof) "" "MyCons my_var" `shouldParse` PConstructor "MyCons" "my_var"
        it "rejects PConstructor with snake_case constructor" $ do
            parse (patternParser <* eof) "" "my_cons my_var" `shouldFailOn` undefined
        it "rejects PConstructor with lowerCamelCase constructor" $ do
            parse (patternParser <* eof) "" "myCons my_var" `shouldFailOn` undefined
        it "rejects PConstructor with PascalCase variable" $ do
            parse (patternParser <* eof) "" "MyCons MyVar" `shouldFailOn` undefined
        it "rejects PConstructor with lowerCamelCase variable" $ do
            parse (patternParser <* eof) "" "MyCons myVar" `shouldFailOn` undefined

        it "parses PVar with snake_case variable name" $ do
            parse (patternParser <* eof) "" "my_var" `shouldParse` PVar "my_var"
        it "rejects PVar with PascalCase variable name" $ do
            parse (patternParser <* eof) "" "MyVar" `shouldFailOn` undefined
        it "rejects PVar with lowerCamelCase variable name" $ do
            -- This case is tricky because lowerCamelCase without underscores can look like snake_case "a"
            -- but "myVar" should fail if it's not a valid snake_case (which it isn't if it's meant to be multi-word)
            -- The snake_case parser should already prevent this if it's strict.
            -- If lowerCamelCase (e.g. "fooBar") is rejected by snake_case parser, this test is valid.
             parse (patternParser <* eof) "" "myVar" `shouldFailOn` undefined


    describe "Expression Parsing Conventions" $ do
        describe "Variable Expressions (Var)" $ do
            it "parses Var with snake_case name" $ do
                parseExpr "my_variable" `shouldParse` Var "my_variable"
            it "rejects Var with PascalCase name" $ do
                -- This should fail because PascalCase is for constructors or types, not variables
                -- It might parse as TAlgebraicData if not in a context expecting a Var,
                -- or fail if `Var` specifically uses `snakeCaseIdentifier`.
                parseExpr "MyVariable" `shouldFailOn` undefined
            it "rejects Var with lowerCamelCase name" $ do
                parseExpr "myVariable" `shouldFailOn` undefined
        
        describe "Lambda Expressions" $ do
            it "parses Lambda with snake_case argument name" $ do
                parseExpr "my_arg -> my_arg" `shouldParse` Lambda "my_arg" Nothing (Var "my_arg")
            it "rejects Lambda with PascalCase argument name" $ do
                parseExpr "MyArg -> MyArg" `shouldFailOn` undefined
            it "rejects Lambda with lowerCamelCase argument name" $ do
                parseExpr "myArg -> myArg" `shouldFailOn` undefined

        describe "HandlerClause Expressions" $ do
            it "parses HandlerClause with PascalCase opName and snake_case args/k" $ do
                -- Simplified input for direct parsing if possible, or use within handle
                let input = "(OpName arg_name k_name) -> k_name arg_name"
                -- This requires HandlerClause to be parsable directly or a helper.
                -- Using a full handle expression for now.
                let fullInput = "handle <Eff> (do X) [ " <> input <> " ]"
                let expectedClause = HandlerClause "OpName" "arg_name" "k_name" (Apply (Var "k_name") (Var "arg_name"))
                -- We are testing the clause parsing, so we check if the clause is correctly formed
                -- within a larger valid structure.
                parseExpr fullInput `shouldSatisfy` (\res -> case res of
                    Right (Handle _ _ [cl]) -> cl == expectedClause
                    _ -> False)

            it "rejects HandlerClause with snake_case opName" $ do
                let fullInput = "handle <Eff> (do X) [ (op_name arg_name k_name) -> k_name arg_name ]"
                parseExpr fullInput `shouldFailOn` undefined
            it "rejects HandlerClause with PascalCase arg_name" $ do
                let fullInput = "handle <Eff> (do X) [ (OpName ArgName k_name) -> k_name ArgName ]"
                parseExpr fullInput `shouldFailOn` undefined
            it "rejects HandlerClause with PascalCase k_name" $ do
                let fullInput = "handle <Eff> (do X) [ (OpName arg_name KName) -> KName arg_name ]"
                parseExpr fullInput `shouldFailOn` undefined
        
        describe "Do Expressions" $ do
            it "parses Do with PascalCase effect operator name" $ do
                parseExpr "do MyEffectOp" `shouldParse` Do "MyEffectOp"
            it "rejects Do with snake_case effect operator name" $ do
                parseExpr "do my_effect_op" `shouldFailOn` undefined
            it "rejects Do with lowerCamelCase effect operator name" $ do
                parseExpr "do myEffectOp" `shouldFailOn` undefined

    describe "handlerExprParser" $ do -- This is an existing describe block
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
