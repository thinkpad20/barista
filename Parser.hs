{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
module Parser where

import Prelude hiding (replicate)
import Text.Parsec hiding (spaces, parse)
--import System.IO.Unsafe
import System.Environment

import Common hiding (tail)
import AST

data Expr = Expr SourcePos (AbsExpr Expr) deriving (Show, Eq)
data ParserState = ParserState {indents::[Int], debugIndent::Int}
type Parser = ParsecT String ParserState (WriterT Text Identity)

instance Pretty Expr where
  render (Expr _ e) = render e
  pretty (Expr _ e) = pretty e

instance IsExpr Expr where
  unExpr (Expr _ abstr) = abstr

------------------------------------------------------------
-------------------  High-level parsers  -------------------
------------------------------------------------------------

-- Entry point parser.
pTopLevel :: Parser Expr
pTopLevel = go <* eof where
  go = blockOf pStatement >>= \case
    [expr] -> return expr
    exprs@(e:_) -> return $ Expr (getPos e) $ Block exprs

-- A collection of statements separated by newlines or semicolons.
pBlock :: Parser Expr
pBlock = item $ logged "block" $ Block <$> indented pStatement

-- A statement. For now redundant, possibly needed later.
pStatement :: Parser Expr
pStatement = logged "statement" pExpr <|> item (return EmptyExpr)

-- An expression. If statements, unary/binary operations, etc.
pExpr :: Parser Expr
pExpr = choice [logged "if" pIf,
                logged "while" pWhile,
                logged "for" pFor,
                logged "function" pFunction,
                logged "assignment" pAssign,
                logged "logical" pBinaryOp]

-- For when either an expression or a block a block is valid.
pExprOrBlock :: Parser Expr
pExprOrBlock = pBlock <|> do
  e <- pExpr
  return $ Expr (getPos e) $ Block [e]

-- An inline-only block
pInlineBlock :: Parser Expr
pInlineBlock = item $ logged "inline block" $ Block <$> pStatement `sepBy1` schar ';'

-- Smallest unit (lower than function application or binary).
pTerm :: Parser Expr
pTerm = choice [pVariable,
                pNumber,
                pString,
                pParens,
                pArray]

-- An expression wrapped in parentheses.
pParens :: Parser Expr
pParens = enclose "()" pExpr

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

-- | Parses any valid, non-keyword identifier. Doesn't store position.
pIdent :: Parser Name
pIdent = checkKeyword $ do
  first <- letter <|> char '_' <|> char '@' <|> char '$'
  rest <- many (letter <|> digit <|> char '_' <|> char '$')
  return $ pack (first : rest)

-- | Same as @pIdent@ but skips spaces after.
pIdent' :: Parser Name
pIdent' = pIdent <* spaces

-- | Wraps an ident parse in a Variable and records the position.
pVariable :: Parser Expr
pVariable = item $ Variable <$> pIdent

-- | Parses a number. Must start with a digit.
pNumber :: Parser Expr
pNumber = item $ do
  first <- many1 digit
  option (Number $ read first) $ do
    dot <- try (char '.' <* notFollowedBy (char '.'))
    rest <- many1 digit
    return $ Number $ read $ first <> (dot : rest)

------------------------------------------------------------
-------------------  Strings & regexes  --------------------
------------------------------------------------------------

-- | Interpolated strings: surrounded with ""
pInString :: Parser Expr
pInString = item $ InString <$> (char '"' *> go) where
  go = do
    str <- fmap pack (anyChar `manyTill` (lookAhead $ oneOf "\\\"#"))
    let escape c = (Plain (str `snoc` c) <>) <$> go
    oneOf "\\\"#" >>= \case
      '\\' -> anyChar >>= \case
        'n'  -> escape '\n'
        '\\' -> escape '\\'
        't'  -> escape '\t'
        'r'  -> escape '\r'
        'b'  -> escape '\b'
        c | c `elem` [' ', '\n', '\t'] -> consume
          | otherwise -> escape c
        where consume = anySpaces >> (Plain str <>) <$> go
      '#' -> anyChar >>= \case
        '{' -> do
          expr <- unExpr <$> pExpr <* char '}'
          Interpolated (Plain str) <$> (pure expr) <*> go
        c -> escape c
      '"' -> return (Plain str)
      c -> error $ "wtf is " <> [c]

-- | Regular strings: surrounded with ''
pRegString :: Parser Expr
pRegString = item $ String <$> (char '\'' >> go) where
  go = do
    str <- fmap pack (anyChar `manyTill` (lookAhead $ oneOf "\\'"))
    let escape c = ((str `snoc` c) <>) <$> go
    oneOf "\\'" >>= \case
      '\\' -> anyChar >>= \case
        'n'  -> escape '\n'
        '\\' -> escape '\\'
        't'  -> escape '\t'
        'r'  -> escape '\r'
        'b'  -> escape '\b'
        c | c `elem` [' ', '\n', '\t'] -> consume
          | otherwise -> escape c
        where consume = anySpaces >> (str <>) <$> go
      '\'' -> return str
      c -> error $ "wtf is " <> [c]

-- | Parses either a regular string, or an interpolated one
pString :: Parser Expr
pString = pRegString <|> pInString

-- | Parses a regex.
pRegex :: Parser Expr
pRegex = item $ do
  char '/'
  lookAhead anyChar >>= \case
    ' ' -> unexpected $ "Not a regex"
    c -> Regex . pack <$> noneOf "/" `manyTill` try (char '/')

------------------------------------------------------------
--------------  Classes, functions, literals  --------------
------------------------------------------------------------

-- | Parses a class definition.
pClass :: Parser Expr
pClass = item $ Class <$ pKeyword "class" <*> name <*> extends <*> decs where
  name = optionMaybe (pIdent <* spaces)
  extends = optionMaybe $ pKeyword "extends" *> pExpr
  decs = optionMaybe (indented pClassDec) >>= \case
    Nothing -> return []
    Just decs -> return decs

-- | Parses a declaration in a class, e.g. a constructor or side-effect.
pClassDec :: Parser (ClassDec Expr)
pClassDec = choice [ try $ ClassDecDef <$> pIdent <* schar ':' <*> pExpr
                   , ClassDecExpr <$> pStatement ]

-- | Parses a function declaration.
pFunction :: Parser Expr
pFunction = item $ do
  args <- optionMaybe pArgs <* spaces >>= \case
    Nothing -> return []
    Just as -> return as
  sstring "->"
  Function args <$> (pExprOrBlock <|> item (return EmptyExpr))
  where pArgs = schar '(' *> pIdent `sepBy` schar ',' <* char ')'

-- | Parses a variable declaration/assignment.
pAssign :: Parser Expr
pAssign = item $ try $ Assign <$> pPattern <* pExactSym "=" <*> pExpr

-- | Patterns are a restricted subset of expressions.
pPattern :: Parser Expr
pPattern = choice $ [getVar, pArrayOfVariables, pObjectDeref]
  where pArrayOfVariables = item $ do
          vars <- enclose "[]" $ getVar `sepBy` schar ','
          return $ Array vars
        getVar = pCallChain >>= \case
          Expr _ (Call _ _) -> unexpected $ "Pattern ended with function call"
          e -> return e
        pObjectDeref = unexpected "not implemented"

-- | Parses an array literal. Currently only supports comma separation.
pArray :: Parser Expr
pArray = item $ do
  exprs <- enclose "[]" $ pExpr `sepEndBy` (schar ',' <|> schar ';')
  return $ Array exprs

-- | Parses an object literal.
pObject :: Parser Expr
pObject = go where
  go = item $ Object <$> (withIndent <|> withBraces)
  keyValOrJustKey = try keyVal <|> do
    ident <- pIdent'
    (,) ident <$> item (pure $ Variable ident)
  keyVal = (,) <$> pIdent' <* schar ':' <*> pExpr
  inline = keyVal `sepBy1` schar ','
  withIndent = indented keyValOrJustKey
  withBraces = enclose "{}" keyValOrJustKey `sepBy` dividers
  dividers = schar ',' <|> (many1 (schar ';' <|> schar '\n') >> return ',')

---------------------------------------------------
-----------------  Control flow  ------------------
---------------------------------------------------

-- | If statements.
pIf :: Parser Expr
pIf = item $ If <$ pKeyword "if"
                <*> logged "if condition" pExpr
                <*> logged "then branch" pThen
                <*> logged "else branch" pElse
  where
    pElse = optionMaybe $ pKeyword "else" *> pExprOrBlock

-- | While loops.
pWhile :: Parser Expr
pWhile = item $ While <$ pKeyword "while"
                      <*> logged "while condition" pExpr
                      <*> logged "while body" pThen

-- | For loops, two kinds: either `in` or `of`.
pFor :: Parser Expr
pFor = item $ do
  pKeyword "for"
  names <- pIdent' `sepBy1` schar ','
  (pKeyword "in" <|> pKeyword "of") >>= \case
    "in" -> ForIn names <$> pExpr <*> pThen
    "of" -> ForOf names <$> pExpr <*> pThen

-- | Used by a few structures to do inlines (if a then b; c; d)
pThen :: Parser Expr
pThen = pBlock <|> pKeyword "then" *> pInlineBlock

------------------------------------------------------------
------------  Calling functions and attributes  ------------
------------------------------------------------------------

-- | Function application has low precedence in CoffeeScript,
-- unless the arguments are not separated by spaces (see below).
-- So we parse a "call chain" (which is space-sensitive) first.
pCall :: Parser Expr
pCall = lexeme $ do
  func <- pCallChain
  debug $ "parsed 'func' " <> render func
  args <- logged "function args" $ optionMaybe $ try $ do
    emptyTuple <|> pExpr `sepBy1` schar ','
  case args of
    Nothing -> return func
    Just args -> return $ Expr (getPos func) $ Call func args

  where emptyTuple = try $ schar '(' *> char ')' *> pure []

-- | This parser will grab a chain of function applications and dots.
-- For example, `foo.bar().baz(a, b).qux`. In CoffeeScript, there is
-- a syntactic distinction between `a (b) 1` and `a(b) 1`. The former
-- means `a(b(1))` and the latter `a(b)(1)`. Screwy but whatevs.
pCallChain :: Parser Expr
pCallChain = lexeme $ logged "term" pTerm >>= go where
  go :: Expr -> Parser Expr
  go expr = do
    lookAhead anyChar >>= \case
      -- If there is a parens immediately following the term,
      -- it's a function call.
      '(' -> do
        -- Grab the arguments, then recurse.
        args <- schar '(' *> pExpr `sepBy` schar ',' <* char ')'
        go $ Expr (getPos expr) $ Call expr args
      -- If there is a square bracket, it's an object dereference.
      '[' -> do
        -- Grab the arguments, then recurse.
        ref <- schar '[' *> pExpr <* char ']'
        go $ Expr (getPos expr) $ ObjectDeref expr ref
      -- Otherwise, we can skip spaces.
      c -> spaces *> lookAhead anyChar >>= \case
        -- If the next thing is a dot, then grab an identifier and recurse.
        '.' -> do
          member <- char '.' *> pIdent
          go $ Expr (getPos expr) $ Dotted expr member
        -- Otherwise, we're done.
        c -> return expr
    -- It's possible that the lookAhead will fail, if we have no input left.
    -- Put this in just in case.
    <|> return expr

---------------------------------------------------
---------------  Binary operators  ----------------
---------------------------------------------------

-- | Entry point for binary operators.
pBinaryOp :: Parser Expr
pBinaryOp = logged "binary operator" pLogical

-- | Lowest level precedence of binary operation are logical operators.
pLogical :: Parser Expr
pLogical = pLeftBinary ["&&", "||", "and", "or"] pComparative

-- | Comparisons are next higher precedence
pComparative :: Parser Expr
pComparative = pLeftBinary ["<", ">", "<=", ">=", "==", "!="] pAdditive

-- | Addition, subtraction
pAdditive :: Parser Expr
pAdditive = pLeftBinary ["+", "-"] pMultiplicative

-- | Multiplication and division
pMultiplicative :: Parser Expr
pMultiplicative = pLeftBinary ["*", "//", "/"] pExponent

-- | Exponentiation
pExponent :: Parser Expr
pExponent = pLeftBinary ["**"] pCall

-- | Left-associative binary parser. Takes a list of operators, and the
-- next-higher-precedence parser to run first.
pLeftBinary :: [String] -> Parser Expr -> Parser Expr
pLeftBinary ops higher = higher >>= go where
  ops' :: Parser Text
  ops' = try $ choice (map pExactSym ops) <* spaces
  go left = spaces *> optionMaybe ops' >>= \case
    Nothing -> return left
    Just op -> do
      right <- pLeftBinary ops higher
      go $ Expr (getPos left) $ Binary op left right

---------------------------------------------------------
-----------------------  Indentation  -------------------
---------------------------------------------------------

-- | Succeeds if indentation has increased.
indent :: Parser ()
indent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- indents <$> getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent
    False -> unexpected "Not an indent"
  where pushIndent i = modifyState $ \s -> s{indents = i: indents s}

-- | Succeeds if indentation has decreased. Doesn't consume input.
outdent :: Parser ()
outdent = try $ do
  isOutdent <- (eof >> return True) <|> lookAhead (
    do newline
       newIndent <- length <$> many (char ' ')
       oldIndent:_ <- indents <$> getState
       return (newIndent < oldIndent))
  case isOutdent of
    True -> popIndent
    False -> unexpected "Not an outdent"
  where popIndent = modifyState $ \s -> s {indents = tail $ indents s}

-- | Succeeds if there is a new line with the same indentation.
nodent :: Parser ()
nodent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- indents <$> getState
  case newIndent == oldIndent of
    True -> return ()
    False -> unexpected "Not an nodent"

-- | Succeeds if there's an empty line (or only whitespace)
emptyLine :: Parser ()
emptyLine = try $ do
  newline
  spaces
  lookAhead (char '\n')
  return ()

-- | In CoffeeScript, a semicolon is (mostly) the same as same indentation.
same :: Parser ()
same = nodent <|> (schar ';' *> return ()) <|> emptyLine

-- | Parses its argument one or more times, separated by @same@.
blockOf :: Parser a -> Parser [a]
blockOf p = p `sepEndBy1` same

-- | Parses an indented block of @p@s.
indented :: Parser a -> Parser [a]
indented p = between indent outdent $ blockOf p

---------------------------------------------------------
-----------------------  Comments  ----------------------
---------------------------------------------------------

pLineComment :: Parser Text
pLineComment = do
  char '#'
  pack <$> manyTill anyChar (lookAhead $ char '\n')

pBlockComment :: Parser Expr
pBlockComment = item $ do
  try $ string "###"
  body <- manyTill anyChar (try $ string "###")
  pure $ Comment $ pack body

---------------------------------------------------------
-----------------------  Helpers  -----------------------
---------------------------------------------------------

-- | Parses spaces only. Tabs are not allowed, and newlines are meaningful.
spaces :: Parser ()
spaces = (many $ char ' ') *> return ()

-- | Parses any whitespace, for when we don't care.
anySpaces :: Parser ()
anySpaces = (many $ oneOf " \t\n\r") *> return ()

-- | Parses an item and consumes any following whitespace.
lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

-- | String + following whitespace.
sstring :: String -> Parser Text
sstring s = fmap pack (lexeme $ string s)

-- | Same as @sstring@ but for chars.
schar :: Char -> Parser Char
schar = lexeme . char

-- | Wraps common @between@ usage.
enclose :: String -> Parser a -> Parser a
enclose (c1:c2:[]) = between (schar c1) (schar c2)
enclose _ = error "Argument to `enclose` should be a string of length 2"

-- | Wrapper for an abstract expression parser. Gets the current position,
-- runs the parser, and stores the result in an `Expr`.
item :: Parser (AbsExpr Expr) -> Parser Expr
item parser = Expr <$> getPosition <*> parser

-- | Parses a given keyword. If it fails, it consumes no input.
pKeyword :: String -> Parser String
pKeyword s = lexeme $ try $ do
  kw <- string s
  notFollowedBy $ choice [letter, digit, char '_', char '$']
  return kw

-- | Parses the exact symbol given, or consumes nothing.
pExactSym :: String -> Parser Text
pExactSym s = try $ sstring s <* notFollowedBy (oneOf symChars)

-- | Fails if the parsed identifier is a keyword.
checkKeyword :: Parser Text -> Parser Text
checkKeyword p = try p >>= \case
  ident | ident `member` keywords -> unexpected $ "keyword " <> show ident
        | otherwise -> return ident

-- | The set of keywords.
keywords :: Set Text
keywords = fromList
  [ "catch", "class", "else", "extends", "false", "finally", "for"
  , "if", "in", "is", "isnt", "new", "return", "switch", "then"
  , "this", "true", "try", "when", "while"]

-- | Gets the position out of an expr.
getPos :: Expr -> SourcePos
getPos (Expr pos _) = pos

----------------------------------------------------------
----------------------   Debugging   ---------------------
----------------------------------------------------------

-- | Wraps a parser with some logging.
logged :: Pretty a => Text -> Parser a -> Parser a
logged desc p = do
  debug $ "Attempting to parse '" <> desc <> "'"
  logInput
  ind <- debugIndent <$> getState
  optionMaybe (try $ withIndent p) >>= \case
    Nothing -> do
      debug $ "Failed to parse '" <> desc <> "'"
      unexpected $ unpack $ "Failed to parse '" <> desc <> "'"
    Just a -> do
      debug $ desc <> " succeeded with `" <> render a <> "`"
      return a

-- | Logs the current remaining input.
logInput :: Parser ()
logInput = do input <- getInput
              debug $ "Remaining input: " <> pack (show input)

-- | Logs a message. Indents it according to the level.
debug :: Text -> Parser ()
debug msg = do
  ind <- debugIndent <$> getState
  let msg' = replicate ind "  " <> msg <> "\n"
  lift $ tell msg'

-- | Wraps a parser, increments/decrements the debug.
withIndent :: Parser a -> Parser a
withIndent p = do
  modifyState $ \s -> s {debugIndent = debugIndent s + 1}
  finally p $ modifyState $ \s -> s {debugIndent = debugIndent s - 1}

-- | Pretty-prints.
print' :: Pretty a => a -> IO ()
print' = putStrLn . unpack . pretty

-- | Guarantees that @action@ will be taken even if @parser@ fails.
finally :: Parser a -> Parser b -> Parser a
finally parser action = (try parser <* action) <|> (action *> unexpected "failure")

----------------------------------------------------------
-----------------  Running the parser  -------------------
----------------------------------------------------------

-- | Initial state of the parser
initState :: ParserState
initState = ParserState {indents = [0], debugIndent = 0}

-- | Parses a string, retuns logs.
parse :: String -> (Either ParseError Expr, Text)
parse = parseWith pTopLevel

-- | Parses a string, drops logs.
parse' :: String -> Either ParseError Expr
parse' = fst . parse

-- | Parses a string with a specific parser.
parseWith :: Parser a -> String -> (Either ParseError a, Text)
parseWith parser = runIdentity . runWriterT . runParserT parser initState ""

-- | Parses a file.
parseFile :: FilePath -> IO (Either ParseError Expr, Text)
parseFile = parseFileWith pTopLevel

-- | Parses a file with a specific parser.
parseFileWith :: Parser a -> FilePath -> IO (Either ParseError a, Text)
parseFileWith parser path = readFile path >>= return . parseWith parser

-- | Parses a file and prints the result.
testFile :: FilePath -> IO ()
testFile = parseFile >=> \case
  (Right expr, _) -> print' expr
  (Left err, _) -> error $ show err

-- | Parses a string and prints the result.
testString :: String -> IO ()
testString = testStringWith pTopLevel

-- | Tests with a specific parser.
testStringWith :: Parser Expr -> FilePath -> IO ()
testStringWith parser s = do
  let (res, _) = parseWith parser s
  case res of
    Right expr -> print' expr
    Left err -> error $ show err

-- | Parses a string and prints the result.
testStringVerbose :: FilePath -> IO ()
testStringVerbose = testStringWithVerbose pTopLevel

-- | Tests with a specific parser.
testStringWithVerbose :: Parser Expr -> FilePath -> IO ()
testStringWithVerbose parser s = do
  let (res, logs) = parseWith parser s
  putStrLn $ unpack logs
  case res of
    Right expr -> print' expr
    Left err -> error $ show err

main :: IO ()
main = getArgs >>= \case
  "-p":path:_ -> testFile path
  input:_ -> testString input

