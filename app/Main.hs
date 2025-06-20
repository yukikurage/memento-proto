module Main where

import Control.Monad (forM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Memento.Codegen (generateJS)
import Language.Memento.Parser (parseProgramText)
import Language.Memento.TypeSolver (typeCheckAST, typeCheckProgram)

-- import Language.Memento.TypeChecker (typeCheckProgram)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      input <- TIO.readFile inputFile
      case parseProgramText input of
        Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
        Right program -> do
          case typeCheckAST program of -- Real AST type checking
            Left err -> hPutStrLn stderr $ "Type error: " ++ err
            Right typeEnv -> do
              putStrLn "Type checking successful"
              putStrLn $ "Type environment: " ++ show typeEnv
          let jsCode = generateJS program
          TIO.writeFile outputFile jsCode
    [inputFile] -> do
      let baseName = takeBaseName inputFile
          jsDir = "dist/js"
          outputFile = jsDir </> baseName ++ ".js"

      -- 出力ディレクトリを作成
      createDirectoryIfMissing True jsDir

      input <- TIO.readFile inputFile
      case parseProgramText input of
        Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
        Right program -> do
          case typeCheckAST program of -- Real AST type checking
            Left err -> hPutStrLn stderr $ "Type error: " ++ err
            Right typeEnv -> do
              putStrLn "Type checking successful"
              putStrLn $ "Type environment: " ++ show typeEnv
          let jsCode = generateJS program
          TIO.writeFile outputFile jsCode
          putStrLn $ "Output written to: " ++ outputFile
    _ -> putStrLn "Usage: memento-proto <input-file.mmt> [output-file.js]"
 where
  printTypeInfo (name, typ) = do
    putStrLn $ "Definition " ++ T.unpack name ++ ":"
    putStrLn $ "  Type: " ++ show typ
