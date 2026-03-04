module Interp.Lex.Lexer where

import Control.Monad
import Control.Monad.State.Lazy
import Interp.Data.Token
import Data.Char (isSpace)

infixl 9 ?:
(?:) :: Maybe a -> Maybe a -> Maybe a
Nothing ?: fallback = fallback
(Just x) ?: _ = Just x

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
  print $ parseS "  ;Hello"
  print $ parseS "  ,Hello"
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
  print $ parseS "\n\n<Hello"
  print $ parseS "  <=Hello"

parseS :: String -> Maybe (LexerVal, LexerState)
parseS src = parseSingleOrDouble . newLexerState $ src

newLexerState :: String -> LexerState
newLexerState src = LexerState src (Position 0 0)

parseSlashOrComment :: LexerState -> Maybe (LexerVal, LexerState)
parseSlashOrComment lexerState =
  parseSingleOrDouble lexerState
    ?: do
      (firstChar, restSource) <- getC lexerState
      if firstChar == '/'
      then
        let
          fromCur = \token n -> makeVal token (currentPos lexerState) n
         in
          undefined
      else Nothing
  where
    go :: LexerState -> Maybe (LexerVal, LexerState)
    go state = case getC state of
      Just ('/', rest) ->
        let (_ignored, rest') = sep '\n' rest
         in undefined
    sep = undefined

parseSingleOrDouble :: LexerState -> Maybe (LexerVal, LexerState)
parseSingleOrDouble = skipWhiteSpace $ \lexerState ->
  parseSingle lexerState ?: do
      (x, y) <- getC lexerState
      matchTok x y (currentPos lexerState)
  where
    matchTok :: Char -> LexerState -> Position -> Maybe (LexerVal, LexerState)
    matchTok '!' pos = Just . weighTok TokBang TokBangEqual ('=' ==) pos
    matchTok '=' pos = Just . weighTok TokEqual TokEqualEqual ('=' ==) pos
    matchTok '>' pos = Just . weighTok TokGreater TokGreaterEqual ('=' ==) pos
    matchTok '<' pos = Just . weighTok TokLess TokLessEqual ('=' ==) pos
    matchTok _  _ = \_ -> Nothing
    weighTok :: Token -> Token -> (Char -> Bool) -> LexerState -> Position -> (LexerVal, LexerState)
    weighTok tok1 tok2 pred st oldPos = case getC st of
      Nothing -> (makeVal tok1 oldPos 1, posAddCol 1 st)
      Just (c, r) ->
        if pred c
          then (makeVal tok2 oldPos 2, posAddCol 2 r)
          else (makeVal tok1 oldPos 1, posAddCol 1 st)

parseSingle :: LexerState -> Maybe (LexerVal, LexerState)
parseSingle= skipWhiteSpace $ \source -> do
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

skipWhiteSpace :: (LexerState -> Maybe (LexerVal, LexerState)) -> LexerState -> Maybe (LexerVal, LexerState)
skipWhiteSpace lexer lexerState = case getC lexerState of
  Nothing -> Nothing
  Just ('\n', rest) -> skipWhiteSpace lexer (posNewLine rest)
  Just (c, rest) ->
    if isSpace c
    then skipWhiteSpace lexer (posAddCol 1 rest)
    else lexer lexerState


getC :: LexerState -> Maybe (Char, LexerState)
getC state =
  let src = source state
   in case src of
        [] -> Nothing
        x : xs -> Just $ (x, state {source = xs})
