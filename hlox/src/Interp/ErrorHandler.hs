module Interp.ErrorHandler where

data InterpError
  = ParseError String
  | RuntimeError String
  | NameError String
  deriving (Eq, Show)
