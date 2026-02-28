{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Interp.ReadFile (runFile)
import Interp.Repl
import InterpError (InterpError)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

type Result a = Either InterpError a

main :: IO ()
main = do
  info
  lox

-- | Entry point for the hlox application.
--   If no arguments are passed, it starts the REPL.
--   If a single argument is passed, it treats it as a file path and runs the file.
--   Otherwise, it prints usage information and exits with an error code.
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
  repl

info :: IO ()
info = do
  cwd <- getCurrentDirectory
  putStrLn $ "Current directory: " <> cwd
