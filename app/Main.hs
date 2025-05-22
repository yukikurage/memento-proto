module Main where

import Control.Monad (forM)
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
      putStrLn $ "Input:\n" ++ T.unpack input
      case parseProgram input of
        Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
        Right exprs -> do
          putStrLn $ "Parsed: " ++ show exprs
          case typeCheckProgram exprs of
            Left err -> hPutStrLn stderr $ "Type error: " ++ show err
            Right types -> do
              putStrLn $ "Types: " ++ show types
              let jsCode = generateJS exprs
              putStrLn $ "Generated JS:\n" ++ T.unpack jsCode
              TIO.writeFile outputFile jsCode
    [inputFile] -> do
      let baseName = takeBaseName inputFile
          jsDir = "dist/js"
          outputFile = jsDir </> baseName ++ ".js"

      -- 出力ディレクトリを作成
      createDirectoryIfMissing True jsDir

      input <- TIO.readFile inputFile
      putStrLn $ "Input:\n" ++ T.unpack input
      case parseProgram input of
        Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
        Right exprs -> do
          putStrLn $ "Parsed: " ++ show exprs
          case typeCheckProgram exprs of
            Left err -> hPutStrLn stderr $ "Type error: " ++ show err
            Right types -> do
              putStrLn $ "Types: " ++ show types
              let jsCode = generateJS exprs
              putStrLn $ "Generated JS:\n" ++ T.unpack jsCode
              TIO.writeFile outputFile jsCode
              putStrLn $ "Output written to: " ++ outputFile
    _ -> putStrLn "Usage: memento-proto <input-file.mmt> [output-file.js]"