module Interp.Lex.Lexer where

import Control.Monad
import Control.Monad.State.Lazy
import Interp.Data.Token

class HasPosition a where
  posLine :: a -> Int
  posCol :: a -> Int
  posNewLine :: a -> a
  posAddCol :: Int -> a -> a

instance HasPosition Position where
  posLine = line
  posCol = column
  posNewLine pos = pos {line = line pos + 1, column = 0}
  posAddCol len pos = pos {column = column pos + len}

instance HasPosition LexerState where
  posLine = line . currentPos
  posCol = column . currentPos
  posNewLine state = state {currentPos = posNewLine (currentPos state)}
  posAddCol len state = state {currentPos = posAddCol len (currentPos state)}

type LexerVal = TokenWithPosition

makeVal :: Token -> Position -> Int -> LexerVal
makeVal tok pos len =
  TokenWithPosition
    { token = tok,
      tokenStart = pos,
      tokenEnd = posAddCol (len - 1) pos
    }

data TokenWithPosition = TokenWithPosition
  { token :: Token,
    tokenStart :: Position,
    tokenEnd :: Position
  }
  deriving (Show)

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show)

data LexerState = LexerState
  { source :: String,
    currentPos :: Position
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

singleOrDoubleCharToken :: LexerState -> Maybe (LexerVal, LexerState)
singleOrDoubleCharToken lexerState = do
  (x, y) <- getC lexerState
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
      Nothing -> (makeVal tok1 (currentPos lexerState) 1, posAddCol 1 st)
      Just (c, r) ->
        if pred c
          then (makeVal tok2 (currentPos lexerState) 2, posAddCol 2 r)
          else (makeVal tok1 (currentPos lexerState) 1, posAddCol 1 st)

singleCharToken :: LexerState -> Maybe (LexerVal, LexerState)
singleCharToken source = do
  (c, rest) <- getC source
  tok <- matchTok c
  return (makeVal tok (currentPos source) 1, posAddCol 1 rest)
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
