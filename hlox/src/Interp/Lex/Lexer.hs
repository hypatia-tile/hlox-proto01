module Interp.Lex.Lexer (lexer, TokenWithRange (..)) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State.Lazy
import Data.Char (isDigit, isSpace)
import qualified Data.Char as C
import Interp.Data.Token

type Lexer a= StateT LexerState Maybe a

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

lexer :: String -> [LexerVal]
lexer source =
  lexerHelper (skip parser) (newLexerState source)
  where
    lexerHelper :: Lexer LexerVal -> LexerState -> [LexerVal]
    lexerHelper lexer sourceState = case runStateT lexer sourceState of
      Nothing -> []
      Just (token, newState) -> token : lexerHelper lexer newState

newLexerVal :: Token -> Position -> Position -> LexerVal
newLexerVal = TokenWithRange

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

instance Show Position where
  show (Position line col) =
    "(" <> show line <> "," <> show col <> ")"

data LexerState = LexerState
  { source :: String,
    currentPos :: Position
  }
  deriving (Show)

newLexerState :: String -> LexerState
newLexerState src = LexerState src (Position 0 0)

parser :: Lexer LexerVal
parser =
  parseDouble
    <|> parseSingle
    <|> parseString
    <|> parseNumber
    <|> parseIdent

parseIdent :: Lexer LexerVal
parseIdent = do
  originPos <- currentPos <$> get
  (ident, lastPosition) <- munchIdent
  let token = case reservedTokens ident of
        Just tok -> tok
        Nothing -> TokIdentifier ident
  return $ TokenWithRange token originPos lastPosition
  where
    munchIdent :: Lexer (String, Position)
    munchIdent = do
      (firstChar, restState) <- advance
      if isAlpha firstChar
        then do
          (s, pos) <- munchAlpha firstChar
          return (s, pos)
        else fail "First character does not match to identifier"
    munchAlpha :: Char -> Lexer (String, Position)
    munchAlpha ch = do
      state <- get
      let base = ([ch], currentPos state)
      if null . source $ state
        then return base
        else do
          (c, pos) <- advance
          if isAlphaNum c
            then do
              (c', pos') <- munchAlpha c
              return (ch:c', pos')
            else return base
    isAlpha :: Char -> Bool
    isAlpha c = C.isAlpha c || c == '_'
    isAlphaNum c = C.isAlphaNum c || c == '_'

parseNumber :: Lexer LexerVal
parseNumber = do
  originPos <- currentPos <$> get
  (firstChar, _) <- matchC isDigit
  (rest, lastPosition) <- munchNumDot
  return $ TokenWithRange (TokNumber . read $ firstChar:rest) originPos lastPosition
  where
    munchNumDot :: Lexer (String, Position)
    munchNumDot = do
      pos <- currentPos <$> get
      isNum <- peek isDigit
      if isNum
        then do
          (c, _) <- advance
          (s, pos') <- munchNumDot
          return (c:s, pos')
        else do
          isDot <- peek ('.' ==)
          if isDot
            then do
              (c, _) <- advance
              (s, pos') <- munch isDigit
              return (c:s, pos')
            else return ("", pos)

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
          return (c:s, po')

parseSingle :: Lexer LexerVal
parseSingle = do
  (c, pos) <- advance
  case matchTok c of
    Just tok -> return $ TokenWithRange tok pos pos
    Nothing -> fail "No single character token matches"
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
    matchTok '!' = Just TokBang
    matchTok '=' = Just TokEqual
    matchTok '>' = Just TokGreater
    matchTok '<' = Just TokLess
    matchTok '/' = Just TokSlash
    matchTok _ = Nothing

parseDouble :: Lexer LexerVal
parseDouble = do
  prepos <- currentPos <$> get
  (c, _) <- advance
  (_, pos) <- matchC ('=' ==)
  case withEqual c of
    Just tok -> return $ TokenWithRange tok prepos pos
    Nothing -> fail "first character does not match double"
  where
    withEqual :: Char -> Maybe Token
    withEqual '!' = Just TokBangEqual
    withEqual '=' = Just TokEqualEqual
    withEqual '>' = Just TokGreaterEqual
    withEqual '<' = Just TokLessEqual
    withEqual _ = Nothing

skip :: Lexer LexerVal -> Lexer LexerVal
skip lexer = do
  modify skipWhiteSpace
  modify skipComment
  lexer

skipWhiteSpace :: LexerState -> LexerState
skipWhiteSpace lexerState = case getC (source lexerState) of
  Nothing -> lexerState
  Just ('\n', rest) -> skipWhiteSpace . posNewLine $ (lexerState {source = rest})
  Just (c, rest) ->
    if isSpace c
      then skipWhiteSpace (posAddCol 1 (lexerState {source = rest}))
      else lexerState

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

matchC :: (Char -> Bool) -> Lexer (Char, Position)
matchC prop = do
  state <- get
  (c', pos) <- advance
  if prop c'
    then return (c', pos)
    else fail "does not suffice condition"

munch :: (Char -> Bool) -> Lexer (String, Position)
munch prop = do
  pos <- currentPos <$> get
  hasNext <- peek prop
  if hasNext
    then do
      (c, pos') <- advance
      (s, lastPos) <- munch prop
      return (c:s, lastPos)
    else return ("", pos)

peek :: (Char -> Bool) -> Lexer Bool
peek prop = StateT $ \state ->
  case getC . source $ state of
    Nothing -> return (False, state)
    Just (c, _) -> return (prop c, state)

advance :: Lexer (Char, Position)
advance = StateT $ \lexerState -> do
  (c, rest) <- getC . source $ lexerState
  let newPos = if c == '\n' then posNewLine else posAddCol 1
  return ((c, currentPos lexerState), newPos (lexerState { source = rest }))

getC :: String -> Maybe (Char, String)
getC [] = Nothing
getC (x:xs) = Just (x, xs)

