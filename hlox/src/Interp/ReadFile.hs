module Interp.ReadFile where

import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)
import Interp.Lex.Lexer (lexer)
import Control.Monad.Writer.Strict
import Control.Monad (mapM_)

runFile :: String -> IO ()
runFile path = do
  putStrLn $ "Running file: " <> path
  withFile path ReadMode $ \handle -> do
    content <- hGetContents handle
    let (tokens, logs) = runWriter (lexer content)
    mapM_ print logs
    mapM_ print tokens
