module Interp.ReadFile where

import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)
import Interp.Lex.Lexer (lexer, LexResult(..))
import Control.Monad (mapM_)

runFile :: String -> IO ()
runFile path = do
  putStrLn $ "Running file: " <> path
  withFile path ReadMode $ \handle -> do
    content <- hGetContents handle
    let result = lexer content
    let toks = tokens result
    let errs = errors result
    mapM_ print errs
    mapM_ print toks
