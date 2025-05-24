{-# LANGUAGE OverloadedStrings #-}
module CodegenSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist)

import Language.Memento.Parser (parseProgram)
import Language.Memento.Syntax (Program)
import Language.Memento.TypeChecker (typeCheckProgram)
import Language.Memento.Codegen (generateJS)

-- Configuration
examplesDir :: FilePath
examplesDir = "examples"

goldenDir :: FilePath
goldenDir = "test" </> "golden"

-- Helper to run the full pipeline and get JS output
processFileToJs :: FilePath -> IO (Either String Text)
processFileToJs mementoFilePath = do
    fileExists <- doesFileExist mementoFilePath
    if not fileExists
    then return $ Left $ "Memento file not found: " ++ mementoFilePath
    else do
        content <- TIO.readFile mementoFilePath
        case parseProgram content of
            Left errBundle -> return $ Left $ "Parse error: " ++ show errBundle -- Simplified error display
            Right parsedProgram ->
                case typeCheckProgram parsedProgram of
                    Left typeErr -> return $ Left $ "Type error: " ++ show typeErr
                    Right _typedProgram -> -- Assuming type checking passed, proceed to codegen
                        return $ Right $ generateJS parsedProgram

spec :: Spec
spec = describe "Codegen" $ do
    describe "Golden Tests" $ do
        let mementoFile = "effect_handler_simple"
        let mementoFilePath = examplesDir </> mementoFile <.> "mmt"
        let goldenFilePath = goldenDir </> mementoFile <.> "golden.js"

        it ("generates correct JavaScript for " ++ mementoFile) $ do
            result <- processFileToJs mementoFilePath
            case result of
                Left err -> expectationFailure err
                Right actualJs -> do
                    goldenFileExists <- doesFileExist goldenFilePath
                    if goldenFileExists
                    then do
                        expectedJs <- TIO.readFile goldenFilePath
                        actualJs `shouldBe` expectedJs
                    else do
                        -- Golden file doesn't exist, create it for the first run or update.
                        -- In a real test suite, you might want a flag to enable/disable this.
                        TIO.writeFile goldenFilePath actualJs
                        expectationFailure $ "Golden file created: " ++ goldenFilePath ++ ". Verify its contents and re-run tests."
                        -- Alternatively, succeed on first run:
                        -- pendingWith $ "Golden file created: " ++ goldenFilePath ++ ". Verify its contents."

-- To make this test pass after the first run, you'll need to:
-- 1. Manually inspect the generated `test/golden/effect_handler_simple.golden.js`.
-- 2. If it's correct, subsequent test runs will compare against it.
-- 3. If the Memento compiler's JS output changes for this file, the test will fail,
--    and you'll need to update the golden file (potentially by deleting it and re-running to regenerate).

    describe "Match Expression Codegen with New Patterns" $ do
        -- Helper to create a simple program with one ValDef "main" for a Match expression
        let createMatchProgram matchScrutineeType clauses expectedResultType =
                Program [ValDef "main" (TFunction matchScrutineeType expectedResultType Set.empty) (Match matchScrutineeType clauses)]

        it "generates correct JS for PNumber pattern" $ do
            let prog = createMatchProgram TNumber
                    [ Clause (PNumber 123.0) (Number 1)
                    , Clause PWildcard (Number 0)
                    ] TNumber
            let js = generateJS prog
            -- Check for the condition and the overall structure
            js `shouldContain` "_matched_val_ === 123"
            js `shouldContain` "if (_matched_val_ === 123)"
            -- We can't easily test execution here without a JS runner,
            -- so we check for key parts of the generated code.

        it "generates correct JS for PBool true pattern" $ do
            let prog = createMatchProgram TBool
                    [ Clause (PBool True) (Number 1)
                    , Clause (PBool False) (Number 0)
                    ] TNumber
            let js = generateJS prog
            js `shouldContain` "_matched_val_ === true"
            js `shouldContain` "if (_matched_val_ === true)"
            js `shouldContain` "else if (_matched_val_ === false)"

        it "generates correct JS for PTuple pattern with literals" $ do
            let tupleType = TTuple [TNumber, TBool]
            let tuplePattern = PTuple [PNumber 42.0, PBool True]
            let prog = createMatchProgram tupleType
                    [ Clause tuplePattern (Number 1)
                    , Clause PWildcard (Number 0)
                    ] TNumber
            let js = generateJS prog
            js `shouldContain` "Array.isArray(_matched_val_) && _matched_val_.length === 2 && _matched_val_[0] === 42 && _matched_val_[1] === true"

        it "generates correct JS for PTuple pattern with PVar binding" $ do
            let tupleType = TTuple [TNumber, TBool]
            let tuplePattern = PTuple [PVar "x", PBool True]
            -- Uses 'x' in the body, expecting it to be bound to _matched_val_[0]
            let prog = createMatchProgram tupleType
                    [ Clause tuplePattern (Var "x") 
                    , Clause PWildcard (Number 0)
                    ] TNumber
            let js = generateJS prog
            -- Condition for PVar is "true", so it's part of the larger tuple condition
            js `shouldContain` "Array.isArray(_matched_val_) && _matched_val_.length === 2 && true && _matched_val_[1] === true"
            js `shouldContain` "const x = _matched_val_[0];"
            -- The body `generateExpr (Var "x")` will produce `ret(x)`.
            js `shouldContain` "_result = ret(x);"


        it "generates correct JS for PTuple pattern with PWildcard" $ do
            let tupleType = TTuple [TNumber, TBool]
            let tuplePattern = PTuple [PWildcard, PBool False]
            let prog = createMatchProgram tupleType
                    [ Clause tuplePattern (Number 1)
                    , Clause PWildcard (Number 0)
                    ] TNumber
            let js = generateJS prog
            -- Condition for PWildcard is "true"
            js `shouldContain` "Array.isArray(_matched_val_) && _matched_val_.length === 2 && true && _matched_val_[1] === false"

        it "generates correct JS for nested PTuple patterns" $ do
            let nestedTupleType = TTuple [TTuple [TNumber, TBool], TNumber]
            let nestedTuplePattern = PTuple [PTuple [PNumber 1.0, PBool True], PVar "y"]
            let prog = createMatchProgram nestedTupleType
                    [ Clause nestedTuplePattern (Var "y")
                    , Clause PWildcard (Number 0)
                    ] TNumber
            let js = generateJS prog
            -- Expected condition for outer tuple: Array.isArray(_matched_val_) && _matched_val_.length === 2
            -- Expected condition for inner tuple (_matched_val_[0]): Array.isArray(_matched_val_[0]) && _matched_val_[0].length === 2 && _matched_val_[0][0] === 1 && _matched_val_[0][1] === true
            -- Expected condition for PVar "y" (_matched_val_[1]): true
            let expectedCondition = "Array.isArray(_matched_val_) && _matched_val_.length === 2 && Array.isArray(_matched_val_[0]) && _matched_val_[0].length === 2 && _matched_val_[0][0] === 1 && _matched_val_[0][1] === true && true"
            js `shouldContain` expectedCondition
            js `shouldContain` "const y = _matched_val_[1];"
            js `shouldContain` "_result = ret(y);"
