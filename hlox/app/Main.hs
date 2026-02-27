{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, catch)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)
import System.IO.Error (isEOFError)

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
  repl

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  result <- getLineSafe
  case result of
    Nothing -> putStrLn "\nBye!"
    Just line -> do
      putStrLn $ line
      repl

getLineSafe :: IO (Maybe String)
getLineSafe = catch (Just <$> getLine) handler
  where
    handler :: IOException -> IO (Maybe String)
    handler e
      | isEOFError e = return Nothing -- EOF -> Nothing
      | otherwise = ioError e -- re-throw other errors

runFile :: String -> IO ()
runFile path = do
  putStrLn $ "Running file: " <> path
  withFile path ReadMode $ \handle -> do
    content <- hGetContents handle
    putStrLn content

info :: IO ()
info = do
  cwd <- getCurrentDirectory
  putStrLn $ "Current directory: " <> cwd
