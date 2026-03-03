module Interp.Lex.Lexer where

import Control.Monad
import Control.Monad.State.Lazy
import Interp.Data.Token

data TokenWithPosition = TokenWithPosition
  { token :: Token,
    tokenStart :: Position,
    tokenEnd :: Position
  }

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show)

data LexerState = LexerState
  { source :: String,
    pos :: Position
  }
  deriving (Show)

test :: IO ()
test = do
  print $ parseS "Hello"
  print $ parseS ":Hello"
  print $ parseS "((Hello"
  print $ parseS ")(Hello"

parseS :: String -> Maybe (Token, LexerState)
parseS src = singleCharToken' . newLexerState $ src

newLexerState :: String -> LexerState
newLexerState src = LexerState src (Position 0 0)

addCol :: Int -> Position -> Position
addCol n p = p {column = column p + n}

addLine :: Int -> Position -> Position
addLine n p = p {line = line p + n}

singleCharToken' :: LexerState -> Maybe (Token, LexerState)
singleCharToken' source = do
  (c, rest) <- getC source
  tok <- singleCharToken c
  return (tok, rest {pos = addCol 1 (pos rest)})

singleCharToken :: Char -> Maybe Token
singleCharToken '(' = Just TokLeftParen
singleCharToken ')' = Just TokRightParen
singleCharToken '{' = Just TokLeftBrace
singleCharToken '}' = Just TokRightBrace
singleCharToken ',' = Just TokComma
singleCharToken '.' = Just TokDot
singleCharToken '-' = Just TokMinus
singleCharToken '+' = Just TokPlus
singleCharToken ';' = Just TokSemicolon
singleCharToken '*' = Just TokStar
singleCharToken '!' = Just TokBang
singleCharToken '=' = Just TokEqual
singleCharToken '>' = Just TokGreater
singleCharToken '<' = Just TokLess
singleCharToken _ = Nothing

getC :: LexerState -> Maybe (Char, LexerState)
getC state =
  let src = source state
   in case src of
        [] -> Nothing
        x : xs -> Just $ (x, state {source = xs})
