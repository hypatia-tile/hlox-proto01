# Test Suite Refactoring for Error Recovery

## Overview

The test suite has been refactored to prepare for the Hybrid error recovery approach. All 65 existing tests pass without modification to the lexer implementation.

## Changes Made

### 1. Added Helper Functions

```haskell
-- Helper to extract tokens from lexer result
-- Will be updated to: lexer source >>= tokens
getTokens :: String -> [LexerVal]
getTokens = lexer

-- Helper to extract just token types for easier testing
getTokenTypes :: String -> [Token]
getTokenTypes = map token . getTokens

-- Helper to check if lexing succeeded (for future use)
shouldLexCleanly :: String -> Expectation
shouldLexCleanly input = do
  let tokens = getTokens input
  length tokens `shouldSatisfy` (> 0)
```

### 2. Refactored All Tests

**Before:**
```haskell
it "lexes integer" $ do
  let tokens = lexer "123"
  length tokens `shouldBe` 1
  token (head tokens) `shouldBe` TokNumber 123.0
```

**After:**
```haskell
it "lexes integer" $ do
  let toks = getTokenTypes "123"
  length toks `shouldBe` 1
  head toks `shouldBe` TokNumber 123.0
```

### 3. Added Placeholder Error Recovery Tests

Prepared tests for error recovery (currently commented out):
- Unexpected character handling
- Unterminated string detection
- Multiple error recovery
- Error recovery with subsequent valid code

## Migration Path to Hybrid Approach

When implementing the Hybrid approach, these changes need to be made:

### Step 1: Update Helper Functions

```haskell
-- Change from:
getTokens :: String -> [LexerVal]
getTokens = lexer

-- To:
getTokens :: String -> [LexerVal]
getTokens = tokens . lexer

-- Add:
getErrors :: String -> [LexError]
getErrors = errors . lexer

-- Add:
hasNoErrors :: String -> Bool
hasNoErrors = null . getErrors
```

### Step 2: Update Test Assertions

Some tests should verify no errors occurred:

```haskell
it "lexes integer" $ do
  let input = "123"
  hasNoErrors input `shouldBe` True
  let toks = getTokenTypes input
  length toks `shouldBe` 1
  head toks `shouldBe` TokNumber 123.0
```

### Step 3: Enable Error Recovery Tests

Uncomment and update the error recovery tests:

```haskell
it "reports unexpected character and continues lexing" $ do
  let input = "var x = @;"
  let toks = getTokenTypes input
  let errs = getErrors input

  -- Should have tokens despite error
  length toks `shouldBe` 5
  toks !! 0 `shouldBe` TokVar
  toks !! 1 `shouldBe` TokIdentifier "x"
  toks !! 2 `shouldBe` TokEqual
  toks !! 3 `shouldBe` TokError "Unexpected character: @"
  toks !! 4 `shouldBe` TokSemicolon

  -- Should have error record
  length errs `shouldBe` 1
  errorMessage (head errs) `shouldContain` "Unexpected"
```

## Benefits

✅ **Minimal Disruption**: All existing tests continue to work
✅ **Single Point of Change**: Only helper functions need updating
✅ **Clear Intent**: Test code is cleaner with `getTokenTypes`
✅ **Future Ready**: Prepared for error testing
✅ **Type Safety**: Compiler will catch any missed updates

## Testing Strategy

1. **Current state**: All 65 tests pass with current lexer
2. **After adding TokError**: Tests still pass, error token never generated
3. **After adding Writer monad**: Update helpers, tests still pass
4. **After error recovery**: Enable error tests, verify behavior

## Test Counts

- **Single-character tokens**: 11 tests
- **Two-character tokens**: 8 tests
- **Number literals**: 5 tests
- **String literals**: 4 tests
- **Identifiers**: 5 tests
- **Keywords**: 16 tests
- **Whitespace**: 4 tests
- **Comments**: 4 tests
- **Complex expressions**: 5 tests
- **Position tracking**: 3 tests
- **Error recovery**: 4 tests (prepared, commented out)

**Total**: 65 current + 4 future = 69 tests
