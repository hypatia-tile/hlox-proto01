module Interp.Data.Lexer where

import Control.Monad.State.Strict
import Interp.Data.Token

type Lexer a = StateT LexerState Maybe a

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

type LexerVal = TokenWithRange

data TokenWithRange = TokenWithRange
  { token :: Token,
    tokenStart :: Position,
    tokenEnd :: Position
  }

instance Show TokenWithRange where
  show (TokenWithRange token tokenStart tokenEnd) =
    show tokenStart <> "-" <> show tokenEnd <> ": " <> show token

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Eq)

instance Show Position where
  show (Position line col) =
    "(" <> show line <> "," <> show col <> ")"

data LexerState = LexerState
  { source :: String,
    currentPos :: Position
  }
  deriving (Eq, Show)

newLexerState :: String -> LexerState
newLexerState src = LexerState src (Position 0 0)
