module Interp.Data.Token where

data Token
  = -- Single-character tokens.
    TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokComma
  | TokDot
  | TokMinus
  | TokPlus
  | TokSemicolon
  | TokStar
  -- One charactor or comment
  | TokSlash
  | -- One or two character tokens.
    TokBang
  | TokBangEqual
  | TokEqual
  | TokEqualEqual
  | TokGreater
  | TokGreaterEqual
  | TokLess
  | TokLessEqual
  | -- Literals.
    TokIdentifier String
  | TokString String
  | TokNumber Double
  | -- Keywords.
    TokAnd
  | TokClass
  | TokElse
  | TokFalse
  | TokFun
  | TokFor
  | TokIf
  | TokNil
  | TokOr
  | TokPrint
  | TokReturn
  | TokSuper
  | TokThis
  | TokTrue
  | TokVar
  | TokWhile
  | -- End of file.
    TokEOF
  | -- Pseude token for error handling.
    TokError String
  deriving (Show, Eq)

reservedTokens :: String -> Maybe Token
reservedTokens "and" = Just TokAnd
reservedTokens "class" = Just TokClass
reservedTokens "else" = Just TokElse
reservedTokens "false" = Just TokFalse
reservedTokens "for" = Just TokFor
reservedTokens "fun" = Just TokFun
reservedTokens "if" = Just TokIf
reservedTokens "nil" = Just TokNil
reservedTokens "or" = Just TokOr
reservedTokens "print" = Just TokPrint
reservedTokens "return" = Just TokReturn
reservedTokens "super" = Just TokSuper
reservedTokens "this" = Just TokThis
reservedTokens "true" = Just TokTrue
reservedTokens "var" = Just TokVar
reservedTokens "while" = Just TokWhile
reservedTokens _ = Nothing


