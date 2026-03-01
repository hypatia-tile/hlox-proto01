module Interp.Core where

data TokenType
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
  | TokSlash
  | TokStar
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
  | TokString
  | TokNumber
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

data LiteralType
  = LiteralString String
  | LiteralNumber Double
  deriving (Show, Eq)

data Token = Token
  { tokenType :: TokenType,
    tokenLexeme :: String,
    tokenLiteral :: Maybe LiteralType,
    tokenLine :: Int
  }
  deriving (Show, Eq)

scan :: String -> [Token]
scan = undefined
