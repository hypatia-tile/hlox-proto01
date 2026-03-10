module Interp.Lex.Lexer (lexer, TokenWithRange (..)) where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Interp.Data.Lexer
import Interp.Data.Token
import Interp.Lex.Parsers.Identifier
import Interp.Lex.Parsers.Number
import Interp.Lex.Parsers.Operator
import Interp.Lex.Parsers.String
import Interp.Lex.Primitives
import Interp.Lex.Trivia

-- | Main lexer function that converts source code into a list of tokens
lexer :: String -> LexerM [LexerVal]
lexer source =
  lexerHelper (skipTrivia *> parser) (newLexerState source)
  where
    lexerHelper :: ParserM LexerVal -> LexerState -> LexerM [LexerVal]
    lexerHelper lexer sourceState = case runStateT lexer sourceState of
      Nothing -> do
        let Just newState = execStateT skipTrivia sourceState
        case runStateT advance newState of 
          Nothing ->  return [] -- End of input, return empty token list
          -- Monad (WriterT [LexError] Identity)
          Just ((c, newPos), newState') -> do
            let unknownToken = TokError $ "Unknown character: " <> [c]
            let errorPos = currentPos newState
            let lexError = LexError "Unknown character" errorPos
            tell [lexError] -- Log the error
            (TokenWithRange unknownToken errorPos errorPos :) <$> lexerHelper lexer newState'
      Just (token, newState) -> (token :) <$> lexerHelper lexer newState

-- | Main parser that tries all token parsers in order
parser :: ParserM LexerVal
parser =
  parseDouble
    <|> parseSingle
    <|> parseString
    <|> parseNumber
    <|> parseIdent
