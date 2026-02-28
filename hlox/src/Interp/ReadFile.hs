module Interp.ReadFile where

import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)

runFile :: String -> IO ()
runFile path = do
  putStrLn $ "Running file: " <> path
  withFile path ReadMode $ \handle -> do
    content <- hGetContents handle
    putStrLn content
