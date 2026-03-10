module Interp.Repl where

import Control.Exception (IOException, catch)
import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)
import System.IO.Error (isEOFError)
import Interp.Lex.Lexer (lexer)
import Control.Monad (mapM_)
import Control.Monad.Writer.Strict

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  result <- getLineSafe
  case result of
    Nothing -> do
      putStrLn "\nBye!"
    Just line -> do
      let (tokens, logs) = runWriter (lexer line)
      mapM_ print tokens
      mapM_ print logs
      repl
  where
    getLineSafe :: IO (Maybe String)
    getLineSafe = catch (Just <$> getLine) handler
    handler :: IOException -> IO (Maybe String)
    handler e
      | isEOFError e = return Nothing -- EOF -> Nothing
      | otherwise = ioError e -- re-throw other errors
