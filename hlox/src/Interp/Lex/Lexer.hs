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
  lexerHelper (skip parser) (newLexerState source)
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

parser :: LexerState -> Maybe (LexerVal, LexerState)
parser state =
  parseDouble state
  ?: parseSingle state
  ?: parseString state
  ?: parseNumber state
  ?: parseIdent state

parseIdent :: LexerState -> Maybe (LexerVal, LexerState)
parseIdent lexerState = do
  (firstChar, _restSource) <- getC (source lexerState)
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
parseNumber lexerState = do
  (firstChar, _restSource) <- getC (source lexerState)
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
parseString lexerState = do
  (firstChar, restSource) <- getC (source lexerState)
  if firstChar == '"'
    then munchString (currentPos lexerState) (lexerState { source = restSource })
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

parseSingle :: LexerState -> Maybe (LexerVal, LexerState)
parseSingle sourceState = do
  (c, rest) <- getC (source sourceState)
  tok <- matchTok c
  return (makeVal tok (currentPos sourceState) 1, posAddCol 1 (sourceState { source = rest}))
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

parseDouble :: LexerState -> Maybe (LexerVal, LexerState)
parseDouble lexerState = do
    (fstChar, restStr) <- getC (source lexerState)
    (secChar, restStr') <- matchTok fstChar restStr
    tok <- go secChar
    let originalPos = currentPos lexerState
    return (makeVal tok originalPos 2, posAddCol 2 (lexerState {source = restStr'}))
  where
    matchTok :: Char -> String -> Maybe (String, String)
    matchTok c restStr
      | c `elem` "!=><" = case restStr of
        ('=':restStr') -> Just (c:['='], restStr')
        _ -> Nothing
      | otherwise = Nothing
    go :: String -> Maybe Token
    go "!=" = Just TokBangEqual
    go "==" = Just TokEqualEqual
    go ">=" = Just TokGreaterEqual
    go "<=" = Just TokLessEqual
    go _ = Nothing

skip :: (LexerState -> Maybe (LexerVal, LexerState)) -> LexerState -> Maybe (LexerVal, LexerState)
skip lexer = lexer . skipComment . skipWhiteSpace

skipWhiteSpace :: LexerState -> LexerState
skipWhiteSpace lexerState = case getC (source lexerState) of
  Nothing -> lexerState
  Just ('\n', rest) -> skipWhiteSpace (lexerState { source = rest})
  Just (c, rest) ->
    if isSpace c
      then skipWhiteSpace (posAddCol 1 (lexerState { source = rest }))
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
        then return $ posNewLine state { source = discardUntilNewline rest2 }
        else Nothing
    discardUntilNewline :: String -> String
    discardUntilNewline [] = []
    discardUntilNewline (x:xs)
      | x == '\n' = xs
      | otherwise = discardUntilNewline xs

getC :: String -> Maybe (Char, String)
getC state =
  let src = state
   in case src of
        [] -> Nothing
        x : xs -> Just (x, xs)
