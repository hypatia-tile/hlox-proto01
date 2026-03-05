module Interp.Lex.Lexer (lexer, TokenWithPosition(..)) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Char (isDigit, isSpace)
import qualified Data.Char as C
import Interp.Data.Token

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

lexer :: String -> [LexerVal]
lexer source =
  lexerHelper (skip parseIdent) (newLexerState source)
  where
    lexerHelper :: (LexerState -> Maybe (LexerVal, LexerState)) -> LexerState -> [LexerVal]
    lexerHelper lexer sourceState = case lexer sourceState of
      Nothing -> []
      Just (token, newState) -> token : lexerHelper lexer newState

newLexerVal :: Token -> Position -> Position -> LexerVal
newLexerVal = TokenWithPosition

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

parseIdent :: LexerState -> Maybe (LexerVal, LexerState)
parseIdent lexerState =
  parseNumber lexerState
    ?: do
      (firstChar, _restSource) <- getC lexerState
      if isAlpha firstChar || firstChar == '_'
        then
          let
            originalPos = currentPos lexerState
            (identStr, rest) = munchIdent (source lexerState)
            len = length identStr
           in do
            tok <- getIdent identStr
            return (makeVal tok originalPos len, posAddCol len (lexerState {source = rest }))
        else Nothing
  where
    munchIdent :: String -> (String, String)
    munchIdent [] = ("", "")
    munchIdent (x:xs) =
      if isAlpha x
      then let (x', xs') = munchIdent' xs in (x:x', xs')
      else ("", x:xs)
      where
        munchIdent' :: String -> (String, String)
        munchIdent' [] = ("", "")
        munchIdent' (x:xs) = 
          if isAlphaNum x
            then let (x', xs') = munchIdent' xs in (x:x', xs')
            else ("", x:xs)
    isAlpha :: Char -> Bool
    isAlpha c = C.isAlpha c || c == '_'
    isAlphaNum c = C.isAlphaNum c || c == '_'
    getIdent :: String -> Maybe Token
    getIdent src =
      reservedTokens src
        ?: if hasAlpha src
          then Just $ TokIdentifier src
          else Nothing
      where
        hasAlpha :: String -> Bool
        hasAlpha [] = False
        hasAlpha (c:cs) = isAlpha c || hasAlpha cs

parseNumber :: LexerState -> Maybe (LexerVal, LexerState)
parseNumber lexerState =
  parseString lexerState
    ?: do
      (firstChar, _restSource) <- getC lexerState
      if isDigit firstChar
        then munchNumber (currentPos lexerState) lexerState
        else Nothing
  where
    munchNumber :: Position -> LexerState -> Maybe (LexerVal, LexerState)
    munchNumber originalPos numState =
      let (numPart, restSrc) = sepWhileNumDot (source numState)
          len = length numPart
       in if null numPart
            then Nothing
            else Just $ (makeVal (TokNumber (read numPart)) originalPos len, posAddCol (len) (numState {source = restSrc}))
    sepWhileNumDot :: String -> (String, String)
    sepWhileNumDot [] = ("", "")
    sepWhileNumDot (c : rest)
      | isDigit c = let (c', rest') = sepWhileNumDot rest in (c : c', rest')
      | c == '.' = case sepWhileNum rest of
          ("", _) -> ("", c : rest)
          (c', rest') -> (c : c', rest')
      | otherwise = ("", c : rest)
    sepWhileNum :: String -> (String, String)
    sepWhileNum [] = ("", "")
    sepWhileNum (c : rest)
      | isDigit c = let (c', rest') = sepWhileNum rest in (c : c', rest')
      | otherwise = ("", c : rest)

parseString :: LexerState -> Maybe (LexerVal, LexerState)
parseString lexerState =
  parseSingleOrDouble lexerState
    ?: do
      (firstChar, restSource) <- getC lexerState
      if firstChar == '"'
        then munchString (currentPos lexerState) restSource
        else Nothing
  where
    munchString :: Position -> LexerState -> Maybe (LexerVal, LexerState)
    munchString originalPos strState = do
      (strPart, restSrc) <- sepWithStr (source strState)
      let newPos = calcPos strPart (posAddCol 1 originalPos)
      return (newLexerVal (TokString strPart) originalPos newPos, (strState {source = restSrc, currentPos = posAddCol 1 newPos}))
    sepWithStr :: String -> Maybe (String, String)
    sepWithStr str = case str of
      [] -> Nothing
      '"' : rest -> Just ([], rest)
      c : rest -> do
        (s, r) <- sepWithStr rest
        return (c : s, r)
    calcPos :: String -> Position -> Position
    calcPos [] pos = pos
    calcPos ('\n' : rest) pos = calcPos rest (posNewLine pos)
    calcPos (_ : rest) pos = calcPos rest (posAddCol 1 pos)

parseSingleOrDouble :: LexerState -> Maybe (LexerVal, LexerState)
parseSingleOrDouble lexerState =
  parseSingle lexerState ?: do
    (x, y) <- getC lexerState
    matchTok x y (currentPos lexerState)
  where
    matchTok :: Char -> LexerState -> Position -> Maybe (LexerVal, LexerState)
    matchTok '!' pos = Just . weighTok TokBang TokBangEqual ('=' ==) pos
    matchTok '=' pos = Just . weighTok TokEqual TokEqualEqual ('=' ==) pos
    matchTok '>' pos = Just . weighTok TokGreater TokGreaterEqual ('=' ==) pos
    matchTok '<' pos = Just . weighTok TokLess TokLessEqual ('=' ==) pos
    matchTok _ _ = \_ -> Nothing
    weighTok :: Token -> Token -> (Char -> Bool) -> LexerState -> Position -> (LexerVal, LexerState)
    weighTok tok1 tok2 pred st oldPos = case getC st of
      Nothing -> (makeVal tok1 oldPos 1, posAddCol 1 st)
      Just (c, r) ->
        if pred c
          then (makeVal tok2 oldPos 2, posAddCol 2 r)
          else (makeVal tok1 oldPos 1, posAddCol 1 st)

parseSingle :: LexerState -> Maybe (LexerVal, LexerState)
parseSingle source = do
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
    matchTok '/' = Just TokSlash
    matchTok _ = Nothing

skip :: (LexerState -> Maybe (LexerVal, LexerState)) -> LexerState -> Maybe (LexerVal, LexerState)
skip lexer = lexer . skipComment . skipWhiteSpace

skipWhiteSpace :: LexerState -> LexerState
skipWhiteSpace lexerState = case getC lexerState of
  Nothing -> lexerState
  Just ('\n', rest) -> skipWhiteSpace (posNewLine rest)
  Just (c, rest) ->
    if isSpace c
      then skipWhiteSpace (posAddCol 1 rest)
      else lexerState

skipComment :: LexerState -> LexerState
skipComment lexerState = case commentLine lexerState of
  Nothing -> lexerState
  Just newState -> newState
  where
    commentLine :: LexerState -> Maybe LexerState
    commentLine state = do
      (c1, rest1) <- getC state 
      (c2, rest2) <- getC rest1
      if c1 == '/' && c2 == '/'
        then return $ posNewLine state { source = discardUntilNewline (source rest2) }
        else Nothing
    discardUntilNewline :: String -> String
    discardUntilNewline [] = []
    discardUntilNewline (x:xs)
      | x == '\n' = xs
      | otherwise = discardUntilNewline xs

getC :: LexerState -> Maybe (Char, LexerState)
getC state =
  let src = source state
   in case src of
        [] -> Nothing
        x : xs -> Just $ (x, state {source = xs})
