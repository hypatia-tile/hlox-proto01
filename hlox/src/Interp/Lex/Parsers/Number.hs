module Interp.Lex.Parsers.Number
  ( parseNumber,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict
import Data.Char (isDigit)
import Interp.Data.Lexer
import Interp.Data.Token
import Interp.Lex.Primitives

-- | Parse a number literal (integer or floating point)
parseNumber :: ParserM LexerVal
parseNumber = do
  originPos <- currentPos <$> get
  (firstChar, _) <- matchC isDigit
  (rest, lastPosition) <- munchNumDot
  return $ TokenWithRange (TokNumber . read $ firstChar : rest) originPos lastPosition
  where
    munchNumDot :: ParserM (String, Position)
    munchNumDot = do
      pos <- currentPos <$> get
      isNum <- peek isDigit
      if isNum
        then do
          (c, _) <- advance
          (s, pos') <- munchNumDot
          return (c : s, pos')
        else munchAfterDot <|> return ("", pos)
    munchAfterDot :: ParserM (String, Position)
    munchAfterDot = do
      (d, _) <- matchC ('.' ==)
      (c, _) <- matchC isDigit
      (s, pos) <- munch isDigit
      return (d : c : s, pos)
