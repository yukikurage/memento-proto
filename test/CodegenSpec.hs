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
