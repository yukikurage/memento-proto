{-# LANGUAGE OverloadedStrings #-}
module TestUtils (
  evalProgram,
  evalMain,
  evalMainToJsValueString,
  expectRuntimeError,
  Value(..), -- Simplified representation for testing
  parseAndTypeCheck,
  compileToJs
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempFile)
import Control.Monad (unless)

import Language.Memento.Parser (parseProgram)
import Language.Memento.Syntax (Program, Expr(Var), Definition(ValDef), TypeError, Program(..))
import Language.Memento.TypeChecker (typeCheckProgram, AdtInfo, EffectInfo)
import Language.Memento.Codegen (generateJS)

import Test.Hspec (expectationFailure, shouldContain)
import qualified Data.Map as Map

-- Simplified Value representation for test assertions
data Value = VNumber Double | VBool Bool | VTuple [Value] | VString T.Text | VAdt T.Text (Maybe Value) deriving (Show, Eq)

-- Placeholder for actual JS execution
-- This would run the JS code and return its stdout
-- For now, it will just return a placeholder or try to run node
runJS :: T.Text -> IO (ExitCode, T.Text, T.Text)
runJS jsCode = withSystemTempFile "memento_test.js" $ \filePath handle -> do
  TIO.hPutStr handle jsCode
  -- Ensure handle is closed before Node.js tries to read it
  -- hClose handle -- withSystemTempFile handles closing
  readProcessWithExitCode "node" [filePath] ""

parseAndTypeCheck :: T.Text -> Either T.Text Program
parseAndTypeCheck code =
  case parseProgram "test_input" code of
    Left err -> Left (T.pack $ show err)
    Right program ->
      case typeCheckProgram program of
        Left typeErr -> Left (T.pack $ show typeErr)
        Right _checkedEnv -> Right program -- Return the program if successful

compileToJs :: Program -> T.Text
compileToJs = generateJS

-- Evaluates the 'main' ValDef in a program and returns its JS string output
evalMainToJsValueString :: T.Text -> IO T.Text
evalMainToJsValueString code = do
  program <- case parseAndTypeCheck code of
    Left errMsg -> expectationFailure (T.unpack $ "Parse/TypeCheck Error: " <> errMsg) >> error "failed" -- Stop test
    Right p -> return p
  
  let jsCode = compileToJs program
  (exitCode, stdout, stderr) <- runJS jsCode
  
  unless (exitCode == ExitSuccess) $
    expectationFailure (T.unpack $ "JS Execution Error:\nExit Code: " <> T.pack (show exitCode) <> "\nStderr: " <> stderr <> "\nStdout: " <> stdout)
  
  -- The JS output is expected to be the result of console.log, which includes a newline.
  return $ T.strip stdout -- Strip newline for direct comparison

-- Evaluates a program and expects a runtime error containing a specific message
expectRuntimeError :: T.Text -> String -> IO ()
expectRuntimeError code expectedErrorMsg = do
  program <- case parseAndTypeCheck code of
    Left errMsg -> expectationFailure (T.unpack $ "Parse/TypeCheck Error: " <> errMsg) >> error "failed"
    Right p -> return p

  let jsCode = compileToJs program
  (exitCode, stdout, stderr) <- runJS jsCode

  case exitCode of
    ExitSuccess -> expectationFailure $ "Expected runtime error, but JS executed successfully. Stdout: " ++ T.unpack stdout
    ExitFailure _ -> do
      let combinedOutput = stdout <> "\n" <> stderr
      unless (T.isInfixOf (T.pack expectedErrorMsg) combinedOutput) $
        expectationFailure $ "Expected error message '" ++ expectedErrorMsg ++ "' not found in JS output.\nOutput:\n" ++ T.unpack combinedOutput
      -- If we are here, it failed as expected with the right message (or part of it)
      return ()


-- Placeholder for a more structured evaluation if needed later
evalProgram :: T.Text -> IO () -- For now, just prints JS
evalProgram code = do
  case parseProgram "test_input" code of
    Left err -> print err
    Right program -> do
      case typeCheckProgram program of
        Left typeErr -> print typeErr
        Right _ -> do
          let js = generateJS program
          TIO.putStrLn js
          (exitCode, stdout, stderr) <- runJS js
          TIO.putStrLn "JS Exit Code:"
          print exitCode
          TIO.putStrLn "JS Stdout:"
          TIO.putStrLn stdout
          TIO.putStrLn "JS Stderr:"
          TIO.putStrLn stderr

evalMain :: T.Text -> IO ()
evalMain = evalProgram -- For now, same as evalProgram, assumes 'main' is called by default JS scaffold
