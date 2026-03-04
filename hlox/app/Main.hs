{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Interp.ReadFile (runFile)
import Interp.Repl
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

-- | Entry point for the hlox application.
--   If no arguments are passed, it starts the REPL.
--   If a single argument is passed, it treats it as a file path and runs the file.
--   Otherwise, it prints usage information and exits with an error code.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [input] -> runFile input
    _ -> do
      putStrLn "Usage: hlox [input_file]"
      exitWith (ExitFailure 64)
