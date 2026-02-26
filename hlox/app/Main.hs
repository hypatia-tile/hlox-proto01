module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (readFile)

main :: IO ()
main = do
  info
  lox

lox :: IO ()
lox = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [input] -> runFile input
    _ -> do
      putStrLn "Usage: hlox [input_file]"
      exitWith (ExitFailure 64)

runPrompt :: IO ()
runPrompt = do
  putStrLn "Welcome to hlox REPL. Type 'exit' to quit."
  putStrLn "TODO: Implement REPL functionality."

runFile :: String -> IO ()
runFile path = do
  putStrLn $ "Running file: " <> path
  content <- readFile path
  putStrLn "File content:"
  putStrLn content
  putStrLn "TODO: Implement file execution functionality."

info :: IO ()
info = do
  cwd <- getCurrentDirectory
  putStrLn $ "Current directory: " <> cwd
