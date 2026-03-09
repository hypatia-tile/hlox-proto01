module Interp.Lex.Trivia
  ( skipTrivia,
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Interp.Data.Lexer
import Interp.Lex.Primitives

-- | Skip all trivia (whitespace and comments)
skipTrivia :: Lexer ()
skipTrivia = do
  st0 <- get
  modify skipWhiteSpace
  modify skipComment
  st1 <- get
  when (st0 /= st1) skipTrivia

-- | Skip whitespace characters
skipWhiteSpace :: LexerState -> LexerState
skipWhiteSpace lexerState = case getC (source lexerState) of
  Nothing -> lexerState
  Just ('\n', rest) -> skipWhiteSpace . posNewLine $ (lexerState {source = rest})
  Just (c, rest) ->
    if isSpace c
      then skipWhiteSpace (posAddCol 1 (lexerState {source = rest}))
      else lexerState

-- | Skip line comments starting with //
skipComment :: LexerState -> LexerState
skipComment lexerState = case commentLine lexerState of
  Nothing -> lexerState
  Just newState -> newState
  where
    commentLine :: LexerState -> Maybe LexerState
    commentLine state = do
      (c1, rest1) <- getC (source state)
      (c2, rest2) <- getC rest1
      if c1 == '/' && c2 == '/'
        then return $ posNewLine state {source = discardUntilNewline rest2}
        else Nothing
    discardUntilNewline :: String -> String
    discardUntilNewline [] = []
    discardUntilNewline (x : xs)
      | x == '\n' = xs
      | otherwise = discardUntilNewline xs
