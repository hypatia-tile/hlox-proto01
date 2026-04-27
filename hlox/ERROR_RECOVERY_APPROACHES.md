# Error Recovery Approaches - Detailed Examples

## Current State (No Error Recovery)

### Current Problem
```haskell
-- Input: "unterminated string
lexer "\"unterminated"
-- Result: [] - just stops, no error reported

-- Input: var x = @;  (@ is invalid)
lexer "var x = @;"
-- Result: [TokVar, TokIdentifier "x", TokEqual] - stops at @, no error
```

### Current Lexer Monad
```haskell
type Lexer a = StateT LexerState Maybe a

lexerHelper :: Lexer LexerVal -> LexerState -> [LexerVal]
lexerHelper lexer sourceState = case runStateT lexer sourceState of
  Nothing -> []  -- ❌ Error = stop immediately
  Just (token, newState) -> token : lexerHelper lexer newState
```

---

## Approach 1: Error Tokens Only

### Concept
Add `TokError` to the Token type. Errors become part of the token stream.

### Code Changes

```haskell
-- 1. Update Token type
data Token
  = TokLeftParen
  | TokRightParen
  -- ... existing tokens ...
  | TokError String  -- ✨ NEW: Error token
  deriving (Show, Eq)

-- 2. Lexer monad stays the same
type Lexer a = StateT LexerState Maybe a

-- 3. Update lexer to catch errors
lexer :: String -> [LexerVal]
lexer source = lexerHelper (newLexerState source)
  where
    lexerHelper :: LexerState -> [LexerVal]
    lexerHelper state
      | null (source state) = []
      | otherwise =
          case runStateT (skipTrivia *> parser) state of
            Just (token, newState) ->
              token : lexerHelper newState
            Nothing ->
              -- ✨ NEW: Generate error token and skip bad char
              let errToken = TokenWithRange
                    (TokError $ "Unexpected character: " ++ [head (source state)])
                    (currentPos state)
                    (currentPos state)
                  newState = posAddCol 1 (state { source = tail (source state) })
              in errToken : lexerHelper newState

-- 4. Update parseString for unterminated strings
parseString :: Lexer LexerVal
parseString = do
  (firstChar, pos) <- advance
  if firstChar == '"'
    then do
      result <- munchString pos
      case result of
        Just (tokStr, lastPos) ->
          return $ TokenWithRange (TokString tokStr) pos lastPos
        Nothing ->
          -- ✨ NEW: Unterminated string error
          return $ TokenWithRange
            (TokError "Unterminated string literal")
            pos
            (currentPos <$> get)
    else fail "Not match a string"
  where
    munchString :: Position -> Lexer (Maybe (String, Position))
    munchString startPos = do
      st <- get
      if null (source st)
        then return Nothing  -- EOF = error
        else do
          (c, po) <- advance
          if c == '"'
            then return $ Just ("", po)
            else if c == '\n'
              then return Nothing  -- Newline = error
              else do
                result <- munchString startPos
                case result of
                  Just (s, po') -> return $ Just (c : s, po')
                  Nothing -> return Nothing
```

### Example Output
```haskell
lexer "var x = @;"
-- Result:
[ TokenWithRange TokVar (0,0) (0,3)
, TokenWithRange (TokIdentifier "x") (0,4) (0,5)
, TokenWithRange TokEqual (0,6) (0,7)
, TokenWithRange (TokError "Unexpected character: @") (0,8) (0,8)  -- ✨
, TokenWithRange TokSemicolon (0,9) (0,9)
]

lexer "\"unterminated"
-- Result:
[ TokenWithRange (TokError "Unterminated string literal") (0,0) (0,13) ]
```

### Pros & Cons
✅ Simple - minimal changes to existing code
✅ Errors have positions in token stream
✅ Parser can see where errors occurred
❌ Harder to collect all errors at once (mixed with tokens)
❌ No summary of errors without filtering tokens

---

## Approach 2: Writer Monad

### Concept
Change monad to accumulate errors separately from tokens.

### Code Changes

```haskell
-- 1. Add error type
data LexError = LexError
  { errorMessage :: String
  , errorPosition :: Position
  } deriving (Show, Eq)

-- 2. Add result type
data LexResult = LexResult
  { tokens :: [LexerVal]
  , errors :: [LexError]
  } deriving (Show)

-- 3. Change Lexer monad
type Lexer a = StateT LexerState (Writer [LexError]) a

-- 4. Update lexer entry point
lexer :: String -> LexResult
lexer source =
  let (tokens, errors) = runWriter (lexerHelper (newLexerState source))
  in LexResult tokens errors
  where
    lexerHelper :: LexerState -> Writer [LexError] [LexerVal]
    lexerHelper state
      | null (source state) = return []
      | otherwise = do
          let result = runStateT (skipTrivia *> parser) state
          case result of
            Nothing -> do
              -- ✨ Record error
              tell [LexError
                     ("Unexpected character: " ++ [head (source state)])
                     (currentPos state)]
              -- Skip bad character and continue
              let newState = posAddCol 1 (state { source = tail (source state) })
              lexerHelper newState
            Just (token, newState) -> do
              rest <- lexerHelper newState
              return (token : rest)

-- 5. Update parseString
parseString :: Lexer LexerVal
parseString = do
  (firstChar, pos) <- advance
  if firstChar == '"'
    then do
      result <- munchString pos
      case result of
        Just (tokStr, lastPos) ->
          return $ TokenWithRange (TokString tokStr) pos lastPos
        Nothing -> do
          -- ✨ Record error and return placeholder token
          lift $ tell [LexError "Unterminated string literal" pos]
          return $ TokenWithRange (TokString "") pos pos
    else fail "Not match a string"
```

### Example Output
```haskell
lexer "var x = @;"
-- Result:
LexResult
  { tokens =
      [ TokenWithRange TokVar (0,0) (0,3)
      , TokenWithRange (TokIdentifier "x") (0,4) (0,5)
      , TokenWithRange TokEqual (0,6) (0,7)
      , TokenWithRange TokSemicolon (0,9) (0,9)
      ]
  , errors =
      [ LexError "Unexpected character: @" (0,8) ]  -- ✨ Separate!
  }

lexer "\"unterminated\nvar x = @;"
-- Result:
LexResult
  { tokens =
      [ TokenWithRange (TokString "") (0,0) (0,0)  -- placeholder
      , TokenWithRange TokVar (1,0) (1,3)
      , TokenWithRange (TokIdentifier "x") (1,4) (1,5)
      , TokenWithRange TokEqual (1,6) (1,7)
      , TokenWithRange TokSemicolon (1,9) (1,9)
      ]
  , errors =
      [ LexError "Unterminated string literal" (0,0)
      , LexError "Unexpected character: @" (1,8)
      ]
  }
```

### Pros & Cons
✅ Clean separation: tokens vs errors
✅ Easy to report all errors at once
✅ Tokens stream stays clean
❌ Bigger refactor (change monad everywhere)
❌ Need placeholder tokens or skip entirely
❌ Parser can't see error positions unless we add them back

---

## Approach 3: Hybrid (Error Tokens + Writer)

### Concept
Best of both worlds: TokError tokens AND separate error collection.

### Code Changes

```haskell
-- 1. Add error token
data Token
  = -- ... existing tokens ...
  | TokError String
  deriving (Show, Eq)

-- 2. Add error type
data LexError = LexError
  { errorMessage :: String
  , errorPosition :: Position
  , errorToken :: Maybe LexerVal  -- ✨ Optional: link to error token
  } deriving (Show, Eq)

-- 3. Add result type
data LexResult = LexResult
  { tokens :: [LexerVal]         -- Includes TokError tokens
  , errors :: [LexError]          -- Detailed error info
  } deriving (Show)

-- 4. Change Lexer monad
type Lexer a = StateT LexerState (Writer [LexError]) a

-- 5. Helper to record errors
recordError :: String -> Position -> Lexer LexerVal
recordError msg pos = do
  let errToken = TokenWithRange (TokError msg) pos pos
  lift $ tell [LexError msg pos (Just errToken)]
  return errToken

-- 6. Update lexer
lexer :: String -> LexResult
lexer source =
  let (tokens, errors) = runWriter (lexerHelper (newLexerState source))
  in LexResult tokens errors
  where
    lexerHelper :: LexerState -> Writer [LexError] [LexerVal]
    lexerHelper state
      | null (source state) = return []
      | otherwise = do
          let result = runStateT (skipTrivia *> parser) state
          case result of
            Nothing -> do
              let pos = currentPos state
                  c = head (source state)
                  msg = "Unexpected character: " ++ [c]
              -- ✨ Record error AND create error token
              let errToken = TokenWithRange (TokError msg) pos pos
              tell [LexError msg pos (Just errToken)]
              let newState = posAddCol 1 (state { source = tail (source state) })
              rest <- lexerHelper newState
              return (errToken : rest)
            Just (token, newState) -> do
              rest <- lexerHelper newState
              return (token : rest)

-- 7. Update parseString
parseString :: Lexer LexerVal
parseString = do
  (firstChar, pos) <- advance
  if firstChar == '"'
    then do
      result <- munchString pos
      case result of
        Just (tokStr, lastPos) ->
          return $ TokenWithRange (TokString tokStr) pos lastPos
        Nothing ->
          recordError "Unterminated string literal" pos
    else fail "Not match a string"
```

### Example Output
```haskell
lexer "var x = @;"
-- Result:
LexResult
  { tokens =
      [ TokenWithRange TokVar (0,0) (0,3)
      , TokenWithRange (TokIdentifier "x") (0,4) (0,5)
      , TokenWithRange TokEqual (0,6) (0,7)
      , TokenWithRange (TokError "Unexpected character: @") (0,8) (0,8)  -- ✨
      , TokenWithRange TokSemicolon (0,9) (0,9)
      ]
  , errors =
      [ LexError "Unexpected character: @" (0,8) (Just <error-token>) ]
  }

-- Pretty printing errors:
printErrors :: LexResult -> IO ()
printErrors (LexResult _ errs) =
  mapM_ (\err -> putStrLn $ show (errorPosition err) ++ ": " ++ errorMessage err) errs

-- Output:
-- (0,8): Unexpected character: @
```

### Pros & Cons
✅ Best flexibility - errors in tokens AND separate
✅ Parser sees error locations
✅ Easy to report errors separately
✅ Can link error records to error tokens
❌ Most complex refactor
❌ Some duplication (error in both places)

---

## Comparison Table

| Feature | Error Tokens | Writer Monad | Hybrid |
|---------|--------------|--------------|--------|
| Monad change | ❌ None | ✅ StateT + Writer | ✅ StateT + Writer |
| Token changes | ✅ Add TokError | ❌ None | ✅ Add TokError |
| Error collection | Manual filter | ✅ Automatic | ✅ Automatic |
| Parser sees errors | ✅ Yes | ❌ No | ✅ Yes |
| Refactor effort | 🟢 Low | 🟡 Medium | 🔴 High |
| Production ready | 🟡 Good | 🟢 Better | 🟢 Best |

---

## Recommendation

**For your project**, I recommend the **Hybrid approach** because:

1. **You already modularized** - easier to update each module independently
2. **Test suite exists** - can verify error recovery works correctly
3. **Best long-term** - supports both IDE tooling (needs error positions) and CLI reporting
4. **Crafting Interpreters style** - book uses error tokens for parser recovery

### Implementation Path

**Phase 1:** Add `TokError` to Token type (small change)
**Phase 2:** Change monad to `StateT + Writer` (medium refactor)
**Phase 3:** Update each parser module with error recovery
**Phase 4:** Add tests for error cases

Would you like me to proceed with this approach?
