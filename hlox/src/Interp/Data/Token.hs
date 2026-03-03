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
    TokIdentifier
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
  deriving (Show, Eq)

