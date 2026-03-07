module Interp.Lex.Lexer (lexer, TokenWithPosition (..)) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Char (isDigit, isSpace)
import qualified Data.Char as C
import Interp.Data.Token

infixl 9 ?:

(?:) :: Lexer a -> Lexer a -> Lexer a
StateT lexer ?: StateT fallback = StateT $ \state ->
  case lexer state of
    Nothing -> fallback state
    x -> x

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

type LexerVal = TokenWithPosition

lexer :: String -> [LexerVal]
lexer source =
  lexerHelper (skip parser) (newLexerState source)
  where
    lexerHelper :: Lexer LexerVal -> LexerState -> [LexerVal]
    lexerHelper lexer sourceState = case runStateT lexer sourceState of
      Nothing -> []
      Just (token, newState) -> token : lexerHelper lexer newState

newLexerVal :: Token -> Position -> Position -> LexerVal
newLexerVal = TokenWithPosition

-- | Effectively make new LexerVal with LexerState
makeValWithState :: Position -> (Token, Int) -> String -> (LexerVal, LexerState)
makeValWithState basePos (tok, len) restStr =
  (makeVal' basePos (tok, len), LexerState restStr (posAddCol len basePos))
  where
    makeVal' :: Position -> (Token, Int) -> LexerVal
    makeVal' pos (tok, len) =
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

instance Show TokenWithPosition where
  show (TokenWithPosition token tokenStart tokenEnd) =
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
    ?: parseSingle
    ?: parseString
    ?: parseNumber
    ?: parseIdent

parseIdent :: Lexer LexerVal
parseIdent = StateT $ \lexerState -> do
  (firstChar, _restSource) <- getC (source lexerState)
  if isAlpha firstChar || firstChar == '_'
    then
      let originalPos = currentPos lexerState
          (identStr, rest) = munchIdent (source lexerState)
          len = length identStr
       in do
            tok <- getIdent identStr
            return $ makeValWithState originalPos (tok, len) rest
    else Nothing
  where
    munchIdent :: String -> (String, String)
    munchIdent [] = ("", "")
    munchIdent (x : xs) =
      if isAlpha x
        then let (x', xs') = munchIdent' xs in (x : x', xs')
        else ("", x : xs)
      where
        munchIdent' :: String -> (String, String)
        munchIdent' [] = ("", "")
        munchIdent' (x : xs) =
          if isAlphaNum x
            then let (x', xs') = munchIdent' xs in (x : x', xs')
            else ("", x : xs)
    isAlpha :: Char -> Bool
    isAlpha c = C.isAlpha c || c == '_'
    isAlphaNum c = C.isAlphaNum c || c == '_'
    getIdent :: String -> Maybe Token
    getIdent src = case reservedTokens src of
      Nothing ->
         if hasAlpha src
        then Just $ TokIdentifier src
        else Nothing
      x -> x
      where
        hasAlpha :: String -> Bool
        hasAlpha [] = False
        hasAlpha (c : cs) = C.isAlpha c || hasAlpha cs

parseNumber :: Lexer LexerVal
parseNumber = do
  originPos <- currentPos <$> get
  (firstChar, _) <- matchC' isDigit
  (rest, posision) <- munchNumDot
  return $ TokenWithPosition (TokNumber . read $ firstChar:rest) originPos posision
  where
    munchNumDot :: Lexer (String, Position)
    munchNumDot = do
      state <- get
      case peek state of
        Nothing -> return ("", currentPos state)
        Just ('.', pos) -> do
          advance
          (s, pos') <- munchNum
          return ('.':s, pos')
        Just (c, pos) ->
           if isDigit c
             then do
               advance
               (s, pos') <- munchNumDot
               return (c:s, pos')
             else return ("", pos)
    munchNum :: Lexer (String, Position)
    munchNum = do
      state <- get
      case peek state of
        Nothing -> return ("", currentPos state)
        Just (c, pos) -> do
          if isDigit c
            then do
              advance
              (s, pos') <- munchNum
              return (c:s, pos')
            else
              return ("", pos)

parseString :: Lexer LexerVal
parseString = do
  (firstChar, pos) <- advance
  if firstChar == '"'
    then do
      (tokStr, lastPos) <- munchString
      return $ TokenWithPosition (TokString tokStr) pos lastPos
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
    Just tok -> return $ TokenWithPosition tok pos pos
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
  pos <- matchC '='
  case withEqual c of
    Just tok -> return $ TokenWithPosition tok prepos pos
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

matchC' :: (Char -> Bool) -> Lexer (Char, Position)
matchC' prop = do
  state <- get
  (c', pos) <- advance
  if prop c'
    then return (c', pos)
    else fail "does not suffice condition"

matchC :: Char -> Lexer Position
matchC c = do
  state <- get
  (c', pos) <- advance
  if c == c'
    then return pos
    else fail "does not match character"

advance :: Lexer (Char, Position)
advance = StateT $ \lexerState -> do
  (c, rest) <- getC . source $ lexerState
  let newPos = if c == '\n' then posNewLine else posAddCol 1
  return ((c, currentPos lexerState), newPos (lexerState { source = rest }))

peek :: LexerState -> Maybe (Char, Position)
peek lexerState = do
  (c, rest) <- getC . source $ lexerState
  return (c, currentPos lexerState)

getC :: String -> Maybe (Char, String)
getC state =
  let src = state
   in case src of
        [] -> Nothing
        x : xs -> Just (x, xs)
