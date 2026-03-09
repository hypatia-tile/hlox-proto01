module Main (main) where

import qualified LexerSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexer" LexerSpec.spec
