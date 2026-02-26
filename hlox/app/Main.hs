module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, stdout)

main :: IO ()
main = greet'

greet' :: IO ()
greet' = do
  args <- getArgs
  case args of
    [] -> putStrLn "No input file provided."
    [input] -> putStrLn $ "Input file: " <> input
    _ -> do
      putStrLn "Usage: hlox [input_file]"
      exitWith (ExitFailure 64)

greet :: IO ()
greet = do
  cwd <- getCurrentDirectory
  putStrLn $ "Current directory: " <> cwd
  putStr "Input file: "
  hFlush stdout
  input <- getLine
  putStrLn input
