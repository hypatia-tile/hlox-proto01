module Interp.ReadFile where

import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)
import Interp.Lex.Lexer (lexer)

runFile :: String -> IO ()
runFile path = do
  putStrLn $ "Running file: " <> path
  withFile path ReadMode $ \handle -> do
    content <- hGetContents handle
    print $ lexer content
