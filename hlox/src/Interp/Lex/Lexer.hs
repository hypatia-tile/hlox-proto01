module Interp.Lex.Lexer (lexer, TokenWithRange (..)) where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict
import Interp.Data.Lexer
import Interp.Lex.Parsers.Identifier
import Interp.Lex.Parsers.Number
import Interp.Lex.Parsers.Operator
import Interp.Lex.Parsers.String
import Interp.Lex.Trivia

-- | Main lexer function that converts source code into a list of tokens
lexer :: String -> [LexerVal]
lexer source =
  lexerHelper (skipTrivia *> parser) (newLexerState source)
  where
    lexerHelper :: Lexer LexerVal -> LexerState -> [LexerVal]
    lexerHelper lexer sourceState = case runStateT lexer sourceState of
      Nothing -> []
      Just (token, newState) -> token : lexerHelper lexer newState

-- | Main parser that tries all token parsers in order
parser :: Lexer LexerVal
parser =
  parseDouble
    <|> parseSingle
    <|> parseString
    <|> parseNumber
    <|> parseIdent
