module Interp.Lex.Primitives
  ( advance,
    peek,
    matchC,
    munch,
    getC,
  )
where

import Control.Monad.State.Strict
import Interp.Data.Lexer

-- | Get the next character from the source string
getC :: String -> Maybe (Char, String)
getC [] = Nothing
getC (x : xs) = Just (x, xs)

-- | Advance the lexer by one character, updating position
advance :: ParserM (Char, Position)
advance = StateT $ \lexerState -> do
  (c, rest) <- getC . source $ lexerState
  let newPos = if c == '\n' then posNewLine else posAddCol 1
  return ((c, currentPos lexerState), newPos (lexerState {source = rest}))

-- | Peek at the next character without consuming it
peek :: (Char -> Bool) -> ParserM Bool
peek prop = StateT $ \state ->
  case getC . source $ state of
    Nothing -> return (False, state)
    Just (c, _) -> return (prop c, state)

-- | Match a character that satisfies a predicate
matchC :: (Char -> Bool) -> ParserM (Char, Position)
matchC prop = do
  state <- get
  (c', pos) <- advance
  if prop c'
    then return (c', pos)
    else fail "does not suffice condition"

-- | Consume characters while they satisfy a predicate
munch :: (Char -> Bool) -> ParserM (String, Position)
munch prop = do
  pos <- currentPos <$> get
  hasNext <- peek prop
  if hasNext
    then do
      (c, pos') <- advance
      (s, lastPos) <- munch prop
      return (c : s, lastPos)
    else return ("", pos)
