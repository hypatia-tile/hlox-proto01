module Interp.Lex.Parsers.String
  ( parseString,
  )
where

import Interp.Data.Lexer
import Interp.Data.Token
import Interp.Lex.Primitives

-- | Parse a string literal enclosed in double quotes
parseString :: Lexer LexerVal
parseString = do
  (firstChar, pos) <- advance
  if firstChar == '"'
    then do
      (tokStr, lastPos) <- munchString
      return $ TokenWithRange (TokString tokStr) pos lastPos
    else fail "Not match a string"
  where
    munchString :: Lexer (String, Position)
    munchString = do
      (c, po) <- advance
      if c == '"'
        then return ("", po)
        else do
          (s, po') <- munchString
          return (c : s, po')
