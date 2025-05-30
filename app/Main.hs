module Main where

import Control.Monad (forM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Memento.Codegen (generateJS)
import Language.Memento.Parser (parseProgram)
import Language.Memento.TypeChecker (typeCheckProgram)
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
      case parseProgram input of
        Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
        Right program -> do
          case typeCheckProgram program of
            Left err -> hPutStrLn stderr $ "Type error: " ++ show err
            Right typeSolverResult -> case typeSolverResult of
              Left err -> hPutStrLn stderr $ "Type error: " ++ show err
              Right () -> do
                putStrLn "Type checking successful"
                let jsCode = generateJS program
                TIO.writeFile outputFile jsCode
    [inputFile] -> do
      let baseName = takeBaseName inputFile
          jsDir = "dist/js"
          outputFile = jsDir </> baseName ++ ".js"

      -- 出力ディレクトリを作成
      createDirectoryIfMissing True jsDir

      input <- TIO.readFile inputFile
      case parseProgram input of
        Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
        Right program -> do
          case typeCheckProgram program of
            Left err -> hPutStrLn stderr $ "Type error: " ++ show err
            Right typeSolverResult -> case typeSolverResult of
              Left err -> hPutStrLn stderr $ "Type error: " ++ show err
              Right () -> do
                putStrLn $ "Type checking successful"
                let jsCode = generateJS program
                TIO.writeFile outputFile jsCode
                putStrLn $ "Output written to: " ++ outputFile
    _ -> putStrLn "Usage: memento-proto <input-file.mmt> [output-file.js]"
 where
  printTypeInfo (name, typ) = do
    putStrLn $ "Definition " ++ T.unpack name ++ ":"
    putStrLn $ "  Type: " ++ show typ