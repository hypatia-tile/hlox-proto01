module Interp.Repl where

import Control.Exception (IOException, catch)
import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)
import System.IO.Error (isEOFError)

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
  where
    getLineSafe :: IO (Maybe String)
    getLineSafe = catch (Just <$> getLine) handler
    handler :: IOException -> IO (Maybe String)
    handler e
      | isEOFError e = return Nothing -- EOF -> Nothing
      | otherwise = ioError e -- re-throw other errors
