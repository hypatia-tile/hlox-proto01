module Interp.Lex.Parsers.Operator
  ( parseSingle,
    parseDouble,
  )
where

import Control.Monad.State.Strict
import Interp.Data.Lexer
import Interp.Data.Token
import Interp.Lex.Primitives

-- | Parse single-character operators and punctuation
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

-- | Parse two-character operators (e.g., ==, !=, <=, >=)
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
