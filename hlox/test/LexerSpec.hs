module LexerSpec (spec) where

import Interp.Data.Lexer
import Interp.Data.Token
import Interp.Lex.Lexer
import Test.Hspec

-- Helper function to extract tokens from lexer result
-- This will be updated when we implement LexResult
getTokens :: String -> [LexerVal]
getTokens = lexer

-- Helper to extract just the token types for easier testing
getTokenTypes :: String -> [Token]
getTokenTypes = map token . getTokens

-- Helper to check if lexing succeeded without errors
shouldLexCleanly :: String -> Expectation
shouldLexCleanly input = do
  let tokens = getTokens input
  length tokens `shouldSatisfy` (> 0)

spec :: Spec
spec = do
  describe "Lexer" $ do
    describe "Single-character tokens" $ do
      it "lexes left parenthesis" $ do
        let toks = getTokenTypes "("
        length toks `shouldBe` 1
        head toks `shouldBe` TokLeftParen

      it "lexes right parenthesis" $ do
        let toks = getTokenTypes ")"
        head toks `shouldBe` TokRightParen

      it "lexes left brace" $ do
        let toks = getTokenTypes "{"
        head toks `shouldBe` TokLeftBrace

      it "lexes right brace" $ do
        let toks = getTokenTypes "}"
        head toks `shouldBe` TokRightBrace

      it "lexes comma" $ do
        let toks = getTokenTypes ","
        head toks `shouldBe` TokComma

      it "lexes dot" $ do
        let toks = getTokenTypes "."
        head toks `shouldBe` TokDot

      it "lexes minus" $ do
        let toks = getTokenTypes "-"
        head toks `shouldBe` TokMinus

      it "lexes plus" $ do
        let toks = getTokenTypes "+"
        head toks `shouldBe` TokPlus

      it "lexes semicolon" $ do
        let toks = getTokenTypes ";"
        head toks `shouldBe` TokSemicolon

      it "lexes star" $ do
        let toks = getTokenTypes "*"
        head toks `shouldBe` TokStar

      it "lexes slash" $ do
        let toks = getTokenTypes "/"
        head toks `shouldBe` TokSlash

    describe "Two-character tokens" $ do
      it "lexes bang equal" $ do
        let toks = getTokenTypes "!="
        length toks `shouldBe` 1
        head toks `shouldBe` TokBangEqual

      it "lexes equal equal" $ do
        let toks = getTokenTypes "=="
        head toks `shouldBe` TokEqualEqual

      it "lexes greater equal" $ do
        let toks = getTokenTypes ">="
        head toks `shouldBe` TokGreaterEqual

      it "lexes less equal" $ do
        let toks = getTokenTypes "<="
        head toks `shouldBe` TokLessEqual

      it "lexes single bang when not followed by equal" $ do
        let toks = getTokenTypes "!"
        head toks `shouldBe` TokBang

      it "lexes single equal when not followed by equal" $ do
        let toks = getTokenTypes "="
        head toks `shouldBe` TokEqual

      it "lexes single greater when not followed by equal" $ do
        let toks = getTokenTypes ">"
        head toks `shouldBe` TokGreater

      it "lexes single less when not followed by equal" $ do
        let toks = getTokenTypes "<"
        head toks `shouldBe` TokLess

    describe "Number literals" $ do
      it "lexes integer" $ do
        let toks = getTokenTypes "123"
        length toks `shouldBe` 1
        head toks `shouldBe` TokNumber 123.0

      it "lexes floating point number" $ do
        let toks = getTokenTypes "123.456"
        head toks `shouldBe` TokNumber 123.456

      it "lexes zero" $ do
        let toks = getTokenTypes "0"
        head toks `shouldBe` TokNumber 0.0

      it "lexes decimal starting with zero" $ do
        let toks = getTokenTypes "0.5"
        head toks `shouldBe` TokNumber 0.5

      it "lexes multiple numbers separated by spaces" $ do
        let toks = getTokenTypes "1 2 3"
        length toks `shouldBe` 3
        toks !! 0 `shouldBe` TokNumber 1.0
        toks !! 1 `shouldBe` TokNumber 2.0
        toks !! 2 `shouldBe` TokNumber 3.0

    describe "String literals" $ do
      it "lexes empty string" $ do
        let toks = getTokenTypes "\"\""
        length toks `shouldBe` 1
        head toks `shouldBe` TokString ""

      it "lexes simple string" $ do
        let toks = getTokenTypes "\"hello\""
        head toks `shouldBe` TokString "hello"

      it "lexes string with spaces" $ do
        let toks = getTokenTypes "\"hello world\""
        head toks `shouldBe` TokString "hello world"

      it "lexes string with special characters" $ do
        let toks = getTokenTypes "\"Hello, World!\""
        head toks `shouldBe` TokString "Hello, World!"

    describe "Identifiers" $ do
      it "lexes simple identifier" $ do
        let toks = getTokenTypes "foo"
        length toks `shouldBe` 1
        head toks `shouldBe` TokIdentifier "foo"

      it "lexes identifier with underscores" $ do
        let toks = getTokenTypes "foo_bar"
        head toks `shouldBe` TokIdentifier "foo_bar"

      it "lexes identifier starting with underscore" $ do
        let toks = getTokenTypes "_foo"
        head toks `shouldBe` TokIdentifier "_foo"

      it "lexes identifier with numbers" $ do
        let toks = getTokenTypes "foo123"
        head toks `shouldBe` TokIdentifier "foo123"

      it "lexes multiple identifiers" $ do
        let toks = getTokenTypes "foo bar baz"
        length toks `shouldBe` 3
        toks !! 0 `shouldBe` TokIdentifier "foo"
        toks !! 1 `shouldBe` TokIdentifier "bar"
        toks !! 2 `shouldBe` TokIdentifier "baz"

    describe "Keywords" $ do
      it "lexes 'and' keyword" $ do
        head (getTokenTypes "and") `shouldBe` TokAnd

      it "lexes 'class' keyword" $ do
        head (getTokenTypes "class") `shouldBe` TokClass

      it "lexes 'else' keyword" $ do
        head (getTokenTypes "else") `shouldBe` TokElse

      it "lexes 'false' keyword" $ do
        head (getTokenTypes "false") `shouldBe` TokFalse

      it "lexes 'for' keyword" $ do
        head (getTokenTypes "for") `shouldBe` TokFor

      it "lexes 'fun' keyword" $ do
        head (getTokenTypes "fun") `shouldBe` TokFun

      it "lexes 'if' keyword" $ do
        head (getTokenTypes "if") `shouldBe` TokIf

      it "lexes 'nil' keyword" $ do
        head (getTokenTypes "nil") `shouldBe` TokNil

      it "lexes 'or' keyword" $ do
        head (getTokenTypes "or") `shouldBe` TokOr

      it "lexes 'print' keyword" $ do
        head (getTokenTypes "print") `shouldBe` TokPrint

      it "lexes 'return' keyword" $ do
        head (getTokenTypes "return") `shouldBe` TokReturn

      it "lexes 'super' keyword" $ do
        head (getTokenTypes "super") `shouldBe` TokSuper

      it "lexes 'this' keyword" $ do
        head (getTokenTypes "this") `shouldBe` TokThis

      it "lexes 'true' keyword" $ do
        head (getTokenTypes "true") `shouldBe` TokTrue

      it "lexes 'var' keyword" $ do
        head (getTokenTypes "var") `shouldBe` TokVar

      it "lexes 'while' keyword" $ do
        head (getTokenTypes "while") `shouldBe` TokWhile

    describe "Whitespace handling" $ do
      it "skips spaces" $ do
        let toks = getTokenTypes "1   2"
        length toks `shouldBe` 2

      it "skips tabs" $ do
        let toks = getTokenTypes "1\t\t2"
        length toks `shouldBe` 2

      it "skips newlines" $ do
        let toks = getTokenTypes "1\n2"
        length toks `shouldBe` 2

      it "handles mixed whitespace" $ do
        let toks = getTokenTypes "1 \t\n 2"
        length toks `shouldBe` 2

    describe "Comments" $ do
      it "skips line comment" $ do
        let toks = getTokenTypes "// this is a comment"
        length toks `shouldBe` 0

      it "skips line comment with code after newline" $ do
        let toks = getTokenTypes "// comment\n123"
        length toks `shouldBe` 1
        head toks `shouldBe` TokNumber 123.0

      it "lexes code before comment" $ do
        let toks = getTokenTypes "123 // comment"
        length toks `shouldBe` 1
        head toks `shouldBe` TokNumber 123.0

      it "handles multiple line comments" $ do
        let toks = getTokenTypes "// comment 1\n// comment 2\n123"
        length toks `shouldBe` 1
        head toks `shouldBe` TokNumber 123.0

    describe "Complex expressions" $ do
      it "lexes simple arithmetic" $ do
        let toks = getTokenTypes "1 + 2"
        length toks `shouldBe` 3
        toks !! 0 `shouldBe` TokNumber 1.0
        toks !! 1 `shouldBe` TokPlus
        toks !! 2 `shouldBe` TokNumber 2.0

      it "lexes variable assignment" $ do
        let toks = getTokenTypes "var x = 42;"
        length toks `shouldBe` 5
        toks !! 0 `shouldBe` TokVar
        toks !! 1 `shouldBe` TokIdentifier "x"
        toks !! 2 `shouldBe` TokEqual
        toks !! 3 `shouldBe` TokNumber 42.0
        toks !! 4 `shouldBe` TokSemicolon

      it "lexes print statement" $ do
        let toks = getTokenTypes "print \"Hello, World!\";"
        length toks `shouldBe` 3
        toks !! 0 `shouldBe` TokPrint
        toks !! 1 `shouldBe` TokString "Hello, World!"
        toks !! 2 `shouldBe` TokSemicolon

      it "lexes function call" $ do
        let toks = getTokenTypes "foo(1, 2)"
        length toks `shouldBe` 6
        toks !! 0 `shouldBe` TokIdentifier "foo"
        toks !! 1 `shouldBe` TokLeftParen
        toks !! 2 `shouldBe` TokNumber 1.0
        toks !! 3 `shouldBe` TokComma
        toks !! 4 `shouldBe` TokNumber 2.0
        toks !! 5 `shouldBe` TokRightParen

      it "lexes if statement" $ do
        let toks = getTokenTypes "if (x > 0) { print x; }"
        length toks `shouldBe` 11
        toks !! 0 `shouldBe` TokIf
        toks !! 1 `shouldBe` TokLeftParen
        toks !! 2 `shouldBe` TokIdentifier "x"
        toks !! 3 `shouldBe` TokGreater
        toks !! 4 `shouldBe` TokNumber 0.0
        toks !! 5 `shouldBe` TokRightParen
        toks !! 6 `shouldBe` TokLeftBrace
        toks !! 7 `shouldBe` TokPrint
        toks !! 8 `shouldBe` TokIdentifier "x"
        toks !! 9 `shouldBe` TokSemicolon
        toks !! 10 `shouldBe` TokRightBrace

    describe "Position tracking" $ do
      it "tracks position for single token" $ do
        let tokens = getTokens "123"
        let tok = head tokens
        line (tokenStart tok) `shouldBe` 0
        column (tokenStart tok) `shouldBe` 0
        line (tokenEnd tok) `shouldBe` 0
        column (tokenEnd tok) `shouldBe` 3

      it "tracks position across whitespace" $ do
        let tokens = getTokens "1 2"
        let tok1 = tokens !! 0
        let tok2 = tokens !! 1
        column (tokenEnd tok1) `shouldBe` 1
        column (tokenStart tok2) `shouldBe` 2

      it "tracks position across newlines" $ do
        let tokens = getTokens "1\n2"
        let tok1 = tokens !! 0
        let tok2 = tokens !! 1
        line (tokenEnd tok1) `shouldBe` 0
        line (tokenStart tok2) `shouldBe` 1
        column (tokenStart tok2) `shouldBe` 0

    -- NOTE: Error recovery tests will be enabled when implementing Hybrid approach
    -- These tests are prepared but commented out until TokError and LexResult are implemented
    {-
    describe "Error recovery" $ do
      it "reports unexpected character and continues lexing" $ do
        let toks = getTokenTypes "var x = @;"
        -- Should contain: TokVar, TokIdentifier, TokEqual, TokError, TokSemicolon
        length toks `shouldBe` 5
        toks !! 0 `shouldBe` TokVar
        toks !! 1 `shouldBe` TokIdentifier "x"
        toks !! 2 `shouldBe` TokEqual
        -- toks !! 3 should be TokError
        toks !! 4 `shouldBe` TokSemicolon

      it "reports unterminated string" $ do
        let toks = getTokenTypes "\"unterminated"
        length toks `shouldBe` 1
        -- head toks should be TokError

      it "recovers from multiple errors" $ do
        let toks = getTokenTypes "var @ = #;"
        -- Should continue lexing after each error
        length toks `shouldSatisfy` (> 0)

      it "handles unterminated string followed by valid code" $ do
        let toks = getTokenTypes "\"bad\nvar x = 1;"
        -- Should have error token for unterminated string, then continue
        length toks `shouldSatisfy` (> 3)
    -}
