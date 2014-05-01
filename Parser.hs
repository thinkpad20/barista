{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
module Parser where

import Prelude hiding (replicate)
import Text.Parsec hiding (spaces, parse)
import System.IO.Unsafe

import Common hiding (tail)
import AST

data Expr = Expr SourcePos (AbsExpr Expr) deriving (Show, Eq)
data ParserState = ParserState {indents::[Int], debugIndent::Int}
type Parser = ParsecT String ParserState (WriterT Text Identity)

instance Pretty Expr where
  render (Expr _ e) = render e

------------------------------------------------------------
-------------------  High-level parsers  -------------------
------------------------------------------------------------

-- Entry point parser.
pTopLevel :: Parser Expr
pTopLevel = go where
  go = blockOf pStatement >>= \case
    [expr] -> return expr
    (e:exprs) -> return $ Expr (getPos e) $ Block exprs

-- A collection of statements separated by newlines or semicolons.
pBlock :: Parser Expr
pBlock = item $ logged "block" $ Block <$> indented pStatement

-- A statement. For now redundant, possibly needed later.
pStatement :: Parser Expr
pStatement = logged "statement" pExpr

-- An expression. If statements, unary/binary operations, etc.
pExpr :: Parser Expr
pExpr = choice [logged "if" pIf,
                logged "logical" pLogical]

-- For when either an expression or a block a block is valid.
pExprOrBlock :: Parser Expr
pExprOrBlock = pBlock <|> pExpr

-- Smallest unit (lower than function application or binary).
pTerm :: Parser Expr
pTerm = choice [pVariable, pNumber, pString, pParens]

-- An expression wrapped in parentheses.
pParens :: Parser Expr
pParens = between (schar '(') (char ')') pExpr

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

-- | Parses any valid, non-keyword identifier. Doesn't store position.
pIdent :: Parser Name
pIdent = checkKeyword $ do
  first <- letter <|> char '_' <|> char '@' <|> char '$'
  rest <- many (letter <|> digit <|> char '_' <|> char '$')
  return $ pack (first : rest)

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

-- | Parses a class.
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
  Function args <$> pExprOrBlock
  where pArgs = schar '(' *> pIdent `sepBy` schar ',' <* char ')'

---------------------------------------------------
-----------------  Control flow  ------------------
---------------------------------------------------

pIf :: Parser Expr
pIf = item $ If <$  pKeyword "if"
                <*> logged "if condition" pExpr
                <*> logged "true branch" pThen
                <*> logged "else branch" pElse
  where
    pThen = pBlock <|> pKeyword "then" *> pExpr
    pElse = optionMaybe $ pKeyword "else" *> pExprOrBlock

------------------------------------------------------------
------------  Calling functions and attributes  ------------
------------------------------------------------------------

-- Function application has low precedence in CoffeeScript,
-- unless the arguments are not separated by spaces (see below).
-- So we parse a "call chain" (which will )
pCall :: Parser Expr
pCall = lexeme $ do
  func <- pCallChain
  args <- logged "function args" $ optionMaybe $ do
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
      -- If there's not, we can skip spaces.
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
pLeftBinary ops higher = logged ("higher precedence than " <> render ops) higher >>= go where
  ops' :: Parser Text
  ops' = choice (map pExactSym ops) <* spaces
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

-- | In CoffeeScript, a semicolon is the same as same indentation.
same :: Parser ()
same = nodent <|> (schar ';' *> return ())

-- | Parses its argument one or more times, separated by @same@.
blockOf :: Parser a -> Parser [a]
blockOf p = p `sepBy1` same

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

-- | Wrapper for an abstract expression parser. Gets the current position,
-- runs the parser, and stores the result in an `Expr`.
item :: Parser (AbsExpr Expr) -> Parser Expr
item parser = Expr <$> getPosition <*> parser

-- | Parses a given keyword. If it fails, it consumes no input.
pKeyword :: String -> Parser ()
pKeyword s = try $ do
  sstring s
  notFollowedBy $ (letter <|> digit <|> char '_' <|> char '$')

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

-- | Pulls the abstract expression out of an Expr. Occasionally necessary.
unExpr :: Expr -> AbsExpr Expr
unExpr (Expr _ abstr) = abstr

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
  debug $ "--indent is at " <> render ind
  finally (withIndent p) $ do
    ind <- debugIndent <$> getState
    debug $  "--done! indent is " <> render ind
    logInput

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
print' = putStrLn . unpack . render

-- | Switch, if on then we won't print logs when we're done.
hideLogs :: Bool
hideLogs = False

-- | Guarantees that @action@ will be taken even if @parser@ fails.
finally :: Parser a -> Parser b -> Parser a
finally parser action = (parser <* action) <|> (action *> unexpected "failure")

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
parseFile = parseFileWith pExpr

-- | Parses a file with a specific parser.
parseFileWith :: Parser a -> FilePath -> IO (Either ParseError a, Text)
parseFileWith parser path = readFile path >>= return . parseWith parser

-- | Parses a string and prints the result.
testString :: FilePath -> IO ()
testString = testStringWith pTopLevel

-- | Tests with a specific parser.
testStringWith :: Parser Expr -> FilePath -> IO ()
testStringWith parser s = do
  let (res, logs) = parseWith parser s
  when (not hideLogs) $ putStrLn $ unpack logs
  case res of
    Right (Expr _ (Block exprs)) -> mapM_ print' exprs
    Right expr -> print' expr
    Left err -> error $ show err
