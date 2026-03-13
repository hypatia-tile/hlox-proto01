module Interp.Repl where

import Control.Exception (IOException, catch)
import System.IO (IOMode (ReadMode), hFlush, hGetContents, stdout, withFile)
import System.IO.Error (isEOFError)
import Interp.Lex.Lexer (lexer, LexResult(..))
import Control.Monad (mapM_)

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  result <- getLineSafe
  case result of
    Nothing -> do
      putStrLn "\nBye!"
    Just line -> do
      let lexResult = lexer line
      let toks = tokens lexResult
      let errs = errors lexResult
      mapM_ print toks
      mapM_ print errs
      repl
  where
    getLineSafe :: IO (Maybe String)
    getLineSafe = catch (Just <$> getLine) handler
    handler :: IOException -> IO (Maybe String)
    handler e
      | isEOFError e = return Nothing -- EOF -> Nothing
      | otherwise = ioError e -- re-throw other errors
