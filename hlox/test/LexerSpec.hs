module LexerSpec (spec) where

import Interp.Data.Lexer
import Interp.Data.Token
import Interp.Lex.Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "Lexer" $ do
    describe "Single-character tokens" $ do
      it "lexes left parenthesis" $ do
        let tokens = lexer "("
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokLeftParen

      it "lexes right parenthesis" $ do
        let tokens = lexer ")"
        token (head tokens) `shouldBe` TokRightParen

      it "lexes left brace" $ do
        let tokens = lexer "{"
        token (head tokens) `shouldBe` TokLeftBrace

      it "lexes right brace" $ do
        let tokens = lexer "}"
        token (head tokens) `shouldBe` TokRightBrace

      it "lexes comma" $ do
        let tokens = lexer ","
        token (head tokens) `shouldBe` TokComma

      it "lexes dot" $ do
        let tokens = lexer "."
        token (head tokens) `shouldBe` TokDot

      it "lexes minus" $ do
        let tokens = lexer "-"
        token (head tokens) `shouldBe` TokMinus

      it "lexes plus" $ do
        let tokens = lexer "+"
        token (head tokens) `shouldBe` TokPlus

      it "lexes semicolon" $ do
        let tokens = lexer ";"
        token (head tokens) `shouldBe` TokSemicolon

      it "lexes star" $ do
        let tokens = lexer "*"
        token (head tokens) `shouldBe` TokStar

      it "lexes slash" $ do
        let tokens = lexer "/"
        token (head tokens) `shouldBe` TokSlash

    describe "Two-character tokens" $ do
      it "lexes bang equal" $ do
        let tokens = lexer "!="
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokBangEqual

      it "lexes equal equal" $ do
        let tokens = lexer "=="
        token (head tokens) `shouldBe` TokEqualEqual

      it "lexes greater equal" $ do
        let tokens = lexer ">="
        token (head tokens) `shouldBe` TokGreaterEqual

      it "lexes less equal" $ do
        let tokens = lexer "<="
        token (head tokens) `shouldBe` TokLessEqual

      it "lexes single bang when not followed by equal" $ do
        let tokens = lexer "!"
        token (head tokens) `shouldBe` TokBang

      it "lexes single equal when not followed by equal" $ do
        let tokens = lexer "="
        token (head tokens) `shouldBe` TokEqual

      it "lexes single greater when not followed by equal" $ do
        let tokens = lexer ">"
        token (head tokens) `shouldBe` TokGreater

      it "lexes single less when not followed by equal" $ do
        let tokens = lexer "<"
        token (head tokens) `shouldBe` TokLess

    describe "Number literals" $ do
      it "lexes integer" $ do
        let tokens = lexer "123"
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokNumber 123.0

      it "lexes floating point number" $ do
        let tokens = lexer "123.456"
        token (head tokens) `shouldBe` TokNumber 123.456

      it "lexes zero" $ do
        let tokens = lexer "0"
        token (head tokens) `shouldBe` TokNumber 0.0

      it "lexes decimal starting with zero" $ do
        let tokens = lexer "0.5"
        token (head tokens) `shouldBe` TokNumber 0.5

      it "lexes multiple numbers separated by spaces" $ do
        let tokens = lexer "1 2 3"
        length tokens `shouldBe` 3
        token (tokens !! 0) `shouldBe` TokNumber 1.0
        token (tokens !! 1) `shouldBe` TokNumber 2.0
        token (tokens !! 2) `shouldBe` TokNumber 3.0

    describe "String literals" $ do
      it "lexes empty string" $ do
        let tokens = lexer "\"\""
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokString ""

      it "lexes simple string" $ do
        let tokens = lexer "\"hello\""
        token (head tokens) `shouldBe` TokString "hello"

      it "lexes string with spaces" $ do
        let tokens = lexer "\"hello world\""
        token (head tokens) `shouldBe` TokString "hello world"

      it "lexes string with special characters" $ do
        let tokens = lexer "\"Hello, World!\""
        token (head tokens) `shouldBe` TokString "Hello, World!"

    describe "Identifiers" $ do
      it "lexes simple identifier" $ do
        let tokens = lexer "foo"
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokIdentifier "foo"

      it "lexes identifier with underscores" $ do
        let tokens = lexer "foo_bar"
        token (head tokens) `shouldBe` TokIdentifier "foo_bar"

      it "lexes identifier starting with underscore" $ do
        let tokens = lexer "_foo"
        token (head tokens) `shouldBe` TokIdentifier "_foo"

      it "lexes identifier with numbers" $ do
        let tokens = lexer "foo123"
        token (head tokens) `shouldBe` TokIdentifier "foo123"

      it "lexes multiple identifiers" $ do
        let tokens = lexer "foo bar baz"
        length tokens `shouldBe` 3
        token (tokens !! 0) `shouldBe` TokIdentifier "foo"
        token (tokens !! 1) `shouldBe` TokIdentifier "bar"
        token (tokens !! 2) `shouldBe` TokIdentifier "baz"

    describe "Keywords" $ do
      it "lexes 'and' keyword" $ do
        let tokens = lexer "and"
        token (head tokens) `shouldBe` TokAnd

      it "lexes 'class' keyword" $ do
        let tokens = lexer "class"
        token (head tokens) `shouldBe` TokClass

      it "lexes 'else' keyword" $ do
        let tokens = lexer "else"
        token (head tokens) `shouldBe` TokElse

      it "lexes 'false' keyword" $ do
        let tokens = lexer "false"
        token (head tokens) `shouldBe` TokFalse

      it "lexes 'for' keyword" $ do
        let tokens = lexer "for"
        token (head tokens) `shouldBe` TokFor

      it "lexes 'fun' keyword" $ do
        let tokens = lexer "fun"
        token (head tokens) `shouldBe` TokFun

      it "lexes 'if' keyword" $ do
        let tokens = lexer "if"
        token (head tokens) `shouldBe` TokIf

      it "lexes 'nil' keyword" $ do
        let tokens = lexer "nil"
        token (head tokens) `shouldBe` TokNil

      it "lexes 'or' keyword" $ do
        let tokens = lexer "or"
        token (head tokens) `shouldBe` TokOr

      it "lexes 'print' keyword" $ do
        let tokens = lexer "print"
        token (head tokens) `shouldBe` TokPrint

      it "lexes 'return' keyword" $ do
        let tokens = lexer "return"
        token (head tokens) `shouldBe` TokReturn

      it "lexes 'super' keyword" $ do
        let tokens = lexer "super"
        token (head tokens) `shouldBe` TokSuper

      it "lexes 'this' keyword" $ do
        let tokens = lexer "this"
        token (head tokens) `shouldBe` TokThis

      it "lexes 'true' keyword" $ do
        let tokens = lexer "true"
        token (head tokens) `shouldBe` TokTrue

      it "lexes 'var' keyword" $ do
        let tokens = lexer "var"
        token (head tokens) `shouldBe` TokVar

      it "lexes 'while' keyword" $ do
        let tokens = lexer "while"
        token (head tokens) `shouldBe` TokWhile

    describe "Whitespace handling" $ do
      it "skips spaces" $ do
        let tokens = lexer "1   2"
        length tokens `shouldBe` 2

      it "skips tabs" $ do
        let tokens = lexer "1\t\t2"
        length tokens `shouldBe` 2

      it "skips newlines" $ do
        let tokens = lexer "1\n2"
        length tokens `shouldBe` 2

      it "handles mixed whitespace" $ do
        let tokens = lexer "1 \t\n 2"
        length tokens `shouldBe` 2

    describe "Comments" $ do
      it "skips line comment" $ do
        let tokens = lexer "// this is a comment"
        length tokens `shouldBe` 0

      it "skips line comment with code after newline" $ do
        let tokens = lexer "// comment\n123"
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokNumber 123.0

      it "lexes code before comment" $ do
        let tokens = lexer "123 // comment"
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokNumber 123.0

      it "handles multiple line comments" $ do
        let tokens = lexer "// comment 1\n// comment 2\n123"
        length tokens `shouldBe` 1
        token (head tokens) `shouldBe` TokNumber 123.0

    describe "Complex expressions" $ do
      it "lexes simple arithmetic" $ do
        let tokens = lexer "1 + 2"
        length tokens `shouldBe` 3
        token (tokens !! 0) `shouldBe` TokNumber 1.0
        token (tokens !! 1) `shouldBe` TokPlus
        token (tokens !! 2) `shouldBe` TokNumber 2.0

      it "lexes variable assignment" $ do
        let tokens = lexer "var x = 42;"
        length tokens `shouldBe` 5
        token (tokens !! 0) `shouldBe` TokVar
        token (tokens !! 1) `shouldBe` TokIdentifier "x"
        token (tokens !! 2) `shouldBe` TokEqual
        token (tokens !! 3) `shouldBe` TokNumber 42.0
        token (tokens !! 4) `shouldBe` TokSemicolon

      it "lexes print statement" $ do
        let tokens = lexer "print \"Hello, World!\";"
        length tokens `shouldBe` 3
        token (tokens !! 0) `shouldBe` TokPrint
        token (tokens !! 1) `shouldBe` TokString "Hello, World!"
        token (tokens !! 2) `shouldBe` TokSemicolon

      it "lexes function call" $ do
        let tokens = lexer "foo(1, 2)"
        length tokens `shouldBe` 6
        token (tokens !! 0) `shouldBe` TokIdentifier "foo"
        token (tokens !! 1) `shouldBe` TokLeftParen
        token (tokens !! 2) `shouldBe` TokNumber 1.0
        token (tokens !! 3) `shouldBe` TokComma
        token (tokens !! 4) `shouldBe` TokNumber 2.0
        token (tokens !! 5) `shouldBe` TokRightParen

      it "lexes if statement" $ do
        let tokens = lexer "if (x > 0) { print x; }"
        length tokens `shouldBe` 11
        token (tokens !! 0) `shouldBe` TokIf
        token (tokens !! 1) `shouldBe` TokLeftParen
        token (tokens !! 2) `shouldBe` TokIdentifier "x"
        token (tokens !! 3) `shouldBe` TokGreater
        token (tokens !! 4) `shouldBe` TokNumber 0.0
        token (tokens !! 5) `shouldBe` TokRightParen
        token (tokens !! 6) `shouldBe` TokLeftBrace
        token (tokens !! 7) `shouldBe` TokPrint
        token (tokens !! 8) `shouldBe` TokIdentifier "x"
        token (tokens !! 9) `shouldBe` TokSemicolon
        token (tokens !! 10) `shouldBe` TokRightBrace

    describe "Position tracking" $ do
      it "tracks position for single token" $ do
        let tokens = lexer "123"
        let tok = head tokens
        line (tokenStart tok) `shouldBe` 0
        column (tokenStart tok) `shouldBe` 0
        line (tokenEnd tok) `shouldBe` 0
        column (tokenEnd tok) `shouldBe` 3

      it "tracks position across whitespace" $ do
        let tokens = lexer "1 2"
        let tok1 = tokens !! 0
        let tok2 = tokens !! 1
        column (tokenEnd tok1) `shouldBe` 1
        column (tokenStart tok2) `shouldBe` 2

      it "tracks position across newlines" $ do
        let tokens = lexer "1\n2"
        let tok1 = tokens !! 0
        let tok2 = tokens !! 1
        line (tokenEnd tok1) `shouldBe` 0
        line (tokenStart tok2) `shouldBe` 1
        column (tokenStart tok2) `shouldBe` 0
