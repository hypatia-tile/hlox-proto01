module Interp.Lex.Lexer where

import Control.Monad
import Control.Monad.State.Lazy
import Interp.Data.Token

type LexerVal = Token

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
  print $ parseS ";Hello"
  print $ parseS ",Hello"
  print $ parseS ":Hello"
  print $ parseS ".Hello"
  print $ parseS "(Hello"
  print $ parseS ")(Hello"
  print $ parseS "!Hello"
  print $ parseS "!=Hello"
  print $ parseS "=Hello"
  print $ parseS "==Hello"
  print $ parseS ">Hello"
  print $ parseS ">=Hello"
  print $ parseS "<Hello"
  print $ parseS "<=Hello"

parseS :: String -> Maybe (LexerVal, LexerState)
parseS src = singleOrDoubleCharToken . newLexerState $ src

newLexerState :: String -> LexerState
newLexerState src = LexerState src (Position 0 0)

addCol :: Int -> Position -> Position
addCol n p = p {column = column p + n}

addLine :: Int -> Position -> Position
addLine n p = p {line = line p + n}

singleOrDoubleCharToken :: LexerState -> Maybe (LexerVal, LexerState)
singleOrDoubleCharToken lexerState = do
  (x,y) <- getC lexerState
  matchTok x y
  where
    matchTok :: Char -> LexerState -> Maybe (LexerVal, LexerState)
    matchTok '!' = Just . weighTok TokBang TokBangEqual ('=' ==)
    matchTok '=' = Just . weighTok TokEqual TokEqualEqual ('=' ==)
    matchTok '>' = Just . weighTok TokGreater TokGreaterEqual ('=' ==)
    matchTok '<' = Just . weighTok TokLess TokLessEqual ('=' ==)
    matchTok _ = \_ -> singleCharToken lexerState
    weighTok :: Token -> Token -> (Char -> Bool) -> LexerState -> (LexerVal, LexerState)
    weighTok tok1 tok2 pred st = case getC st of
      Nothing -> (tok1, st { pos = addCol 1 (pos st) })
      Just (c, r) -> 
        if pred c
        then (tok2, r { pos = addCol 2 (pos r) })
        else (tok1, st { pos = addCol 1 (pos st) })

singleCharToken :: LexerState -> Maybe (LexerVal, LexerState)
singleCharToken source = do
  (c, rest) <- getC source
  tok <- matchTok c
  return (tok, rest {pos = addCol 1 (pos rest)})
  where
    matchTok :: Char -> Maybe Token
    matchTok '(' = Just TokLeftParen
    matchTok ')' = Just TokRightParen
    matchTok '{' = Just TokLeftBrace
    matchTok '}' = Just TokRightBrace
    matchTok ',' = Just TokComma
    matchTok '.' = Just TokDot
    matchTok '-' = Just TokMinus
    matchTok '+' = Just TokPlus
    matchTok ';' = Just TokSemicolon
    matchTok '*' = Just TokStar
    matchTok _ = Nothing

getC :: LexerState -> Maybe (Char, LexerState)
getC state =
  let src = source state
   in case src of
        [] -> Nothing
        x : xs -> Just $ (x, state {source = xs})
