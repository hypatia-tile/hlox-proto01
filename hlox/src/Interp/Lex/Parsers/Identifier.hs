module Interp.Lex.Parsers.Identifier
  ( parseIdent,
  )
where

import Control.Monad.State.Strict
import qualified Data.Char as C
import Interp.Data.Lexer
import Interp.Data.Token
import Interp.Lex.Primitives

-- | Parse an identifier or keyword
parseIdent :: ParserM LexerVal
parseIdent = do
  originPos <- currentPos <$> get
  (ident, lastPosition) <- munchIdent
  let token = case reservedTokens ident of
        Just tok -> tok
        Nothing -> TokIdentifier ident
  return $ TokenWithRange token originPos lastPosition
  where
    munchIdent :: ParserM (String, Position)
    munchIdent = do
      (c, _) <- matchC isAlpha
      (s, pos) <- munch isAlphaNum
      return (c : s, pos)
    isAlpha :: Char -> Bool
    isAlpha c = C.isAlpha c || c == '_'
    isAlphaNum c = C.isAlphaNum c || c == '_'
