module Main where

import Control.Monad (forM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Memento.Analysis.Hoisting (analyzeHoisting, formatHoistingWarnings)
import Language.Memento.Codegen (generateJS)
import Language.Memento.CodegenJS (generateJSFromIR)
import Language.Memento.CodegenWASM (generateWASM)
import Language.Memento.LowerToIR (lowerProgram, lowerProgramWithTypes)
import Language.Memento.Parser (parseProgramText)
import Language.Memento.TypeSolver (inferTypes)

import Language.Memento.TypeSolver.Core.Types (formatTypeEnv, formatTypeError)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

data Backend = JavaScript | WebAssembly deriving (Show, Eq)

parseBackend :: String -> Backend
parseBackend "--backend=wasm" = WebAssembly
parseBackend "--backend=js" = JavaScript
parseBackend _ = JavaScript -- Default to JavaScript

main :: IO ()
main = do
  args <- getArgs
  let (backendArgs, fileArgs) = partition (isBackendArg) args
      backend = case backendArgs of
        (arg : _) -> parseBackend arg
        [] -> JavaScript
  case fileArgs of
    [inputFile, outputFile] -> compileFile backend inputFile (Just outputFile)
    [inputFile] -> compileFile backend inputFile Nothing
    _ -> putStrLn "Usage: memento-proto [--backend=js|--backend=wasm] <input-file.mmt> [output-file]"
 where
  isBackendArg arg = "--backend=" `isPrefixOf` arg
  isPrefixOf prefix str = take (length prefix) str == prefix
  partition pred xs = (filter pred xs, filter (not . pred) xs)

compileFile :: Backend -> String -> Maybe String -> IO ()
compileFile backend inputFile maybeOutputFile = do
  let baseName = takeBaseName inputFile
      (outputDir, outputExt) = case backend of
        JavaScript -> ("dist/js", ".js")
        WebAssembly -> ("dist/wasm", ".wat")
      outputFile = case maybeOutputFile of
        Just file -> file
        Nothing -> outputDir </> baseName ++ outputExt

  -- Create output directory
  createDirectoryIfMissing True (takeDirectory outputFile)

  input <- TIO.readFile inputFile
  case parseProgramText input of
    Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
    Right program -> do
      case inferTypes program of
        Left err -> hPutStrLn stderr $ "Type error: " ++ T.unpack (formatTypeError err)
        Right typeEnv -> do
          putStrLn "Type checking successful"
          putStrLn $ "Type environment: \n" ++ T.unpack (formatTypeEnv typeEnv)

          -- Run hoisting analysis
          let hoistingAnalysis = analyzeHoisting program
          putStrLn ""
          putStrLn (formatHoistingWarnings hoistingAnalysis)

          -- Generate code based on backend (both now use IR)
          let irProgram = lowerProgramWithTypes program typeEnv
          case backend of
            JavaScript -> do
              let jsCode = generateJSFromIR irProgram
              TIO.writeFile outputFile jsCode
            WebAssembly -> do
              let wasmCode = generateWASM irProgram
              TIO.writeFile outputFile wasmCode

          putStrLn $ "Output written to: " ++ outputFile
 where
  printTypeInfo (name, typ) = do
    putStrLn $ "Definition " ++ T.unpack name ++ ":"
    putStrLn $ "  Type: " ++ show typ
