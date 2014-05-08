{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
module Parser where

import Prelude hiding (replicate)
import Text.Parsec hiding (spaces, parse)
import Text.Parsec.Pos (newPos)
import System.Environment

import Common hiding (tail, head)
import AST

data Expr = Expr SourcePos (AbsExpr Expr) deriving (Show)
data ParserState = ParserState {indents::[Int], debugIndent::Int}
type Parser = ParsecT String ParserState (WriterT Text Identity)

instance Render Expr where
  render (Expr _ e) = render e
  pretty (Expr _ e) = pretty e

instance IsExpr Expr where
  unExpr (Expr _ abstr) = abstr

instance Eq Expr where
  Expr _ e == Expr _ e' = e == e'

------------------------------------------------------------
-------------------  High-level parsers  -------------------
------------------------------------------------------------

-- | Entry point parser.
pTopLevel :: Parser Expr
pTopLevel = do
  input <- getInput
  debug "OK here we go"
  logInput
  go <* eof
  where
    go = logged "main" (blockOf pStatement) >>= \case
      [expr] -> return expr
      exprs@(e:_) -> return $ Expr (getPos e) $ Block exprs

-- | A collection of statements separated by newlines or semicolons.
pBlock :: Parser Expr
pBlock = item . logged "block" $ Block <$> indented pStatement

-- | A statement. Possibly nothing.
pStatement :: Parser Expr
pStatement = logged "statement" $ pExpr -- <|> item' EmptyExpr

pExpr :: Parser Expr
pExpr = do
  expr <- pSmallExpr
  option expr $ lookAhead (logged "some keyword" pAnyKeyword) >>= \case
    "if" -> pEmbeddedIf expr
    "unless" -> pEmbeddedUnless expr
    "while" -> pEmbeddedWhile expr
    "for" -> pEmbeddedFor expr
    _ -> unexpected "Unknown keyword expression"

-- | An expression. If statements, unary/binary operations, etc.
pSmallExpr :: Parser Expr
pSmallExpr = choice [ logged "if"          pIf
                    , logged "while"       pWhile
                    , logged "for"         pFor
                    , logged "switch"      pSwitch
                    , logged "try/catch"   pTryCatch
                    , logged "function"    pFunction
                    , logged "assignment"  pAssign
                    , logged "bare object" pBareObject
                    , logged "newed"       pNew
                    , logged "return"      pReturn
                    , logged "break"       pBreak
                    , logged "continue"    pContinue
                    , pBinaryOp]

-- | For when either an expression or a block a block is valid.
pExprOrBlock :: Parser Expr
pExprOrBlock = pBlock <|> pInlineBlock

-- | An inline-only block
pInlineBlock :: Parser Expr
pInlineBlock = item . logged "inline block" $ Block <$> pStatement `sepBy1` schar ';'

-- | Smallest unit (lower than function application or binary).
pTerm :: Parser Expr
pTerm = choice [ pNumber
               , pVariable
               , pString
               , pRegex
               , pParens
               , pArray
               , pObject ]

-- | An expression wrapped in parentheses.
pParens :: Parser Expr
pParens = enclose "()" pExpr

section p = p <* many emptyLine <* same

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

-- | Parses any valid, non-keyword identifier. Doesn't store position.
pIdent :: Parser Name
pIdent = checkKeyword pAnyIdent

-- | Same as @pIdent@, but doesn't check if it's a keyword. Used for .member
-- vars.
pAnyIdent :: Parser Name
pAnyIdent = do
  first <- letter <|> char '_' <|> char '@' <|> char '$'
  rest <- many (letter <|> digit <|> char '_' <|> char '$')
  return $ pack (first : rest)

-- | Same as @pIdent@ but skips spaces after.
pIdent' :: Parser Name
pIdent' = pIdent <* spaces

-- | Same as @pAnyIdent@ but skips spaces.
pAnyIdent' :: Parser Name
pAnyIdent' = pAnyIdent <* spaces

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
    str <- strTill (oneOf "\\\"#")
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
          expr <- unExpr <$> pSmallExpr <* char '}'
          Interpolated (Plain str) <$> (pure expr) <*> go
        c -> escape c
      '"' -> return (Plain str)
      c -> error $ "wtf is " <> [c]

-- | Regular strings: surrounded with ''
pRegString :: Parser Expr
pRegString = item $ String <$> (char '\'' >> go) where
  go = do
    str <- strTill (oneOf "\\'")
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

-- | Long strings, python-style
pLongString :: Parser Expr
pLongString = item $ String <$> blockStr "'''"

-- | Parses either a regular string, or an interpolated one
pString :: Parser Expr
pString = pLongString <|> pRegString <|> pInString

-- | Parses a regex.
pRegex :: Parser Expr
pRegex = item $ do
  char '/'
  lookAhead anyChar >>= \case
    ' ' -> unexpected "Not a regex"
    c -> Regex <$> strTillConsume (char '/')

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
  args <- optionMaybe pArgs >>= \case
    Nothing -> return []
    Just as -> return as
  arrow <- sstring "->" <|> sstring "=>"
  body <- pExprOrBlock <|> item' EmptyExpr
  return $ case arrow of
    "->" -> Function args body
    "=>" -> FatArrowFunction args body
  where pArgs = schar '(' *> pIdent `sepBy` schar ',' <* schar ')'

-- | Parses a variable declaration/assignment.
pAssign :: Parser Expr
pAssign = itry $ Assign <$> pPattern <* pExactSym "=" <*> pExpr

-- | Patterns are a restricted subset of expressions.
pPattern :: Parser Expr
pPattern = choice $ [pCallChain, pArray, objPattern]
  where objPattern = item $ Object <$> pObjectWithBraces

-- | Parses an array literal. Currently only supports comma separation.
pArray :: Parser Expr
pArray = item $ do
  exprs <- enclose "[]" $
    anySpaces *> pSmallExpr `sepEndBy` pObjectDivider <* anySpaces
  return $ Array exprs

-- | Parses an object literal.
pObject :: Parser Expr
pObject = go where
  go = item $ Object <$> (withIndent <|> pObjectWithBraces)
  inline = keyVal `sepBy1` schar ','
  withIndent = logged "indented keyval" $ indented keyVal

-- | Parses an object using curly braces.
pObjectWithBraces :: Parser [(Name, Expr)]
pObjectWithBraces = enclose "{}" $ keyValOrJustKey `sepBy` pObjectDivider

-- | Parses a "bare object", that is, one that isn't preceeded by indents or
-- curly braces.
pBareObject :: Parser Expr
pBareObject = item $ Object <$> (kv >>= go) where
  kv = logged "key/value pair" $ try keyVal
  go k = option [k] $ try $ do
    -- Will not consume the object divider UNLESS it parses another keyval.
    fmap (k :) (pObjectDivider *> keyVal >>= go)

-- | Parses the separaters in an object: line breaks, commas, or semicolons.
pObjectDivider :: Parser ()
pObjectDivider = choice [ignore $ schar ',',
                         ignore $ many1 $ schar ';' <|> schar '\n']

-- | In CoffeeScript, if in a dictionary we write just a key @foo@, it's the
-- same as writing @foo: foo@.
keyValOrJustKey :: Parser (Name, Expr)
keyValOrJustKey = try keyVal <|> do
    ident <- pIdent'
    (,) ident <$> item' (Variable ident)

keyVal :: Parser (Name, Expr)
keyVal = (,) <$> pAnyIdent' <* schar ':' <*> logged "obj value" pExpr

---------------------------------------------------
-----------------  Control flow  ------------------
---------------------------------------------------

-- | If or Unless statements.
pIf :: Parser Expr
pIf = item $ k <*> cond <*> _then <*> _else where
  k = choice [pKeyword "if" >> return If, pKeyword "unless" >> return Unless]
  cond = logged "if condition" pExpr
  _then = logged "then branch" pThen <* many same
  _else = logged "else branch" pElse

-- | Used by a few structures to do inlines (if a then b; c; d)
pThen :: Parser Expr
pThen = pBlock <|> pKeyword "then" *> pInlineBlock -- pBlock -- <|>

-- | Parses an 'else'. Used by @pIf@ and @pSwitch@.
pElse :: Parser (Maybe Expr)
pElse = optionMaybe $ pKeyword "else" *> pExprOrBlock

-- | Embedded if statements. `foo = a if b`
pEmbeddedIf :: Expr -> Parser Expr
pEmbeddedIf expr = item $ EmbeddedIf expr <$ pKeyword "if" <*> pExpr

-- | Embedded unless statements. `foo = a unless b`
pEmbeddedUnless :: Expr -> Parser Expr
pEmbeddedUnless expr = item $ EmbeddedUnless expr <$ pKeyword "unless" <*> pExpr


-- | For loops, two kinds: either `in` or `of`.
pFor :: Parser Expr
pFor = item $ do
  debug "trying to get keyword for"
  pKeyword "for"
  debug "trying to get identifiers"
  names <- pIdent' `sepBy1` schar ','
  (pKeyword "in" <|> pKeyword "of") >>= \case
    "in" -> ForIn names <$> pSmallExpr <*> pThen
    "of" -> ForOf names <$> pSmallExpr <*> pThen

-- | For comprehensions (a for b in c).
pEmbeddedFor :: Expr -> Parser Expr
pEmbeddedFor expr = item $ logged "embedded for" $ do
  pKeyword "for"
  names <- pIdent' `sepBy1` schar ','
  (pKeyword "in" <|> pKeyword "of") >>= \case
    "in" -> EmbeddedForIn expr names <$> pExpr
    "of" -> EmbeddedForOf expr names <$> pExpr

-- | While loops.
pWhile :: Parser Expr
pWhile = item $ While <$ pKeyword "while"
                      <*> logged "while condition" pExpr
                      <*> logged "while body" pThen

pEmbeddedWhile :: Expr -> Parser Expr
pEmbeddedWhile expr = item $ EmbeddedWhile expr <$ pKeyword "while" <*> pExpr

-- | Parses a switch statement.
pSwitch :: Parser Expr
pSwitch = item $ do
  pKeyword "switch"
  cond <- pExpr
  (cases, _else) <- getCases
  return $ Switch cond cases _else
  where sCase :: Parser ([Expr], Expr)
        sCase = (,) <$ pKeyword "when" <*> pSmallExpr `sepBy1` schar ',' <*> pThen
        getCases = do
          indent
          cases <- blockOf sCase
          _else <- pElse
          outdent
          return (cases, _else)

-- | Parses a try/catch/finally.
pTryCatch :: Parser Expr
pTryCatch = item $ TryCatch <$ pKeyword "try" <*> toTry <*> catch <*> finally
  where
    toTry = pExprOrBlock
    catch = optionMaybe $ do
      pKeyword "catch"
      name <- pIdent'
      expr <- pThen <|> item' EmptyExpr
      return (name, expr)
    finally = optionMaybe $
      pKeyword "finally" *> (pExprOrBlock <|> item' EmptyExpr)

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
    emptyTuple <|> pSmallExpr `sepBy1` schar ','
  case args of
    Nothing -> return func
    Just args -> return $ Expr (getPos func) $ Call func args
  where emptyTuple = try $ schar '(' *> char ')' *> pure []

-- | This parser will grab a chain of function applications and dots.
-- For example, `foo.bar().baz(a, b).qux`. In CoffeeScript, there is
-- a syntactic distinction between `a (b) 1` and `a(b) 1`. The former
-- means `a(b(1))` and the latter `a(b)(1)`. Similarly, `a[b]` means
-- `a[b]`, while `a [b]` means `a([b])`.
pCallChain :: Parser Expr
pCallChain = lexeme $ pTerm >>= go where
  go :: Expr -> Parser Expr
  go expr = do
    lookAhead anyChar >>= \case
      -- If there is a parens immediately following the term,
      -- it's a function call.
      '(' -> do
        -- Grab the arguments, then recurse.
        args <- schar '(' *> pSmallExpr `sepBy` schar ',' <* char ')'
        go $ Expr (getPos expr) $ Call expr args
      -- If there is a square bracket, it's an object dereference.
      '[' -> do
        -- Grab the arguments, then recurse.
        ref <- schar '[' *> pExpr
        ref2 <- optionMaybe $ try $ sstring ".." *> pExpr
        char ']'
        case ref2 of
          Nothing -> go $ Expr (getPos expr) $ ObjectDeref expr ref
          Just e -> go $ Expr (getPos expr) $ ArraySlice expr ref e
      -- Otherwise, we can skip spaces.
      c -> spaces *> do
        option expr $ try $ do
          char '.' >> notFollowedBy (char '.')
          member <- pAnyIdent'
          go $ Expr (getPos expr) $ Dotted expr member
    -- It's possible that the lookAhead will fail, if we have no input left.
    -- Put this in just in case.
    <|> return expr

---------------------------------------------------
------------  Keyworded expressions  --------------
---------------------------------------------------

-- | A new'ed expression
pNew :: Parser Expr
pNew = item $ New <$ pKeyword "new" <*> pExpr

-- | A returned expression
pReturn :: Parser Expr
pReturn = item $ Return <$ pKeyword "return" <*> optionMaybe pExpr

-- | A break statement
pBreak :: Parser Expr
pBreak = item $ Break <$ pKeyword "break"

-- | A continue statement
pContinue :: Parser Expr
pContinue = item $ Continue <$ pKeyword "continue"

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
pComparative = pLeftBinary ["<", ">", "<=", ">=", "==", "!=", "is", "isnt"] pAdditive

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
  ops' = try $ choice (map pExactSym ops) <* anySpaces
  go left = spaces *> optionMaybe ops' >>= \case
    Nothing -> return left
    Just op -> do
      right <- pLeftBinary ops higher
      go $ Expr (getPos left) $ Binary op left right

---------------------------------------------------------
-----------------------  Comments  ----------------------
---------------------------------------------------------

pComment :: Parser Expr
pComment = pLineComment <|> pBlockComment

pLineComment :: Parser Expr
pLineComment = item $ logged "line comment" $ do
  char '#'
  Comment <$> strTill (char '\n')

pBlockComment :: Parser Expr
pBlockComment = item $ Comment <$> blockStr "###"

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

-- | Tries, and if successful then skips spaces.
ltry :: Parser a -> Parser a
ltry = lexeme . try

-- | Wrapper for an abstract expression parser. Gets the current position,
-- runs the parser, and stores the result in an `Expr`.
item :: Parser (AbsExpr Expr) -> Parser Expr
item parser = Expr <$> getPosition <*> parser

-- | Lifts the intended thing into the context and records the position.
item' :: (AbsExpr Expr) -> Parser Expr
item' = item . return

-- | Tries, and if successful records the position.
itry :: Parser (AbsExpr Expr) -> Parser Expr
itry = item . try

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

-- | Parses a given keyword. If it fails, it consumes no input.
pKeyword :: Text -> Parser Text
pKeyword (unpack -> s) = ltry $ pack <$> string s <* notFollowedBy identChar

pAnyKeyword :: Parser Text
pAnyKeyword = choice $ map pKeyword $ toList keywords

-- | Parses the exact symbol given, or consumes nothing.
pExactSym :: String -> Parser Text
pExactSym s = ltry $ pack <$> string s <* notFollowedBy (oneOf symChars)

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

-- | Parses any character valid in an identifier.
identChar :: Parser Char
identChar = choice [letter, digit, char '_', char '$']

-- | Gets the position out of an expr.
getPos :: Expr -> SourcePos
getPos (Expr pos _) = pos

-- | Creates an expression with dummy source position, for testing.
toExpr :: AbsExpr Expr -> Expr
toExpr = Expr (newPos "" 0 0)

---------------------------------------------------------
-----------------------  Indentation  -------------------
---------------------------------------------------------

-- | Succeeds if indentation has increased.
indent :: Parser ()
indent = logged "an indent" $ try $ do
  newline
  newIndent <- length <$> many (char ' ')
  debug $ "the new indent is " <> render newIndent
  oldIndent:_ <- indents <$> getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent
    False -> unexpected "Not an indent"
  where pushIndent i = modifyState $ \s -> s{indents = i: indents s}

-- | Succeeds if indentation has decreased. Doesn't consume input.
outdent :: Parser ()
outdent = logged "an outdent" $ try $ do
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
    False -> unexpected "Not a nodent"

-- | Succeeds if there's an empty line, one with only whitespace, or a comment
emptyLine :: Parser ()
emptyLine = try $ newline *> spaces *> finish
  where
    finish = (ignore $ lookAhead $ char '\n') <|> ignore pLineComment

-- | Indents, outdents, nodents which also grab up preceding emptylines.
indent', outdent', nodent' :: Parser ()
indent'  = try $ many emptyLine >> indent
outdent' = try $ many emptyLine >> outdent
nodent'  = try $ many emptyLine >> nodent

-- | In CoffeeScript, a semicolon is (mostly) the same as same indentation.
same :: Parser ()
same = nodent' <|> ignore (schar ';')

-- | Parses its argument one or more times, separated by @same@.
blockOf :: Render a => Parser a -> Parser [a]
blockOf p = p `sepEndBy1` same

-- | Parses an indented block of @p@s.
indented :: Render a => Parser a -> Parser [a]
--indented p = between (logged "indent!" indent') (logged "outdent!" outdent') $ logged "some block" $ blockOf p

indented p = do
  debug "starting an indent"
  indent'
  debug "got the indent"
  block <- blockOf p
  debug "finished getting lines"
  outdent'
  debug "w00t!"
  return block


ignore :: Parser a -> Parser ()
ignore p = p >> return ()

perhaps :: Parser a -> Parser ()
perhaps = ignore . optionMaybe

--skip = spaces *> emptyLine' `sepEndBy` newline

-- | Grabs any character until the stopping parser. Doesn't consume @stop@.
strTill :: Parser a -> Parser Text
strTill stop = fmap pack $ anyChar `manyTill` lookAhead stop

-- | Same as @strTill@, but consumes the stopping parser.
strTillConsume :: Parser a -> Parser Text
strTillConsume stop = fmap pack $ anyChar `manyTill` try stop

-- | For block comments and long strings.
blockStr :: String -> Parser Text
blockStr start = try (string start) >> strTillConsume (string start)

----------------------------------------------------------
----------------------   Debugging   ---------------------
----------------------------------------------------------

-- | Wraps a parser with some logging.
logged :: Render a => Text -> Parser a -> Parser a
logged desc p = do
  debug $ "Attempting to parse '" <> desc <> "'"
  logInput
  ind <- debugIndent <$> getState
  (optionMaybe . try . withIndent) p >>= \case
    Nothing -> do
      debug $ "Failed to parse '" <> desc <> "'"
      unexpected $ unpack $ "Failed to parse '" <> desc <> "'"
    Just a -> do
      debug $ "+++++++++++++++++++++++++++++ " <> desc
              <> " succeeded with `" <> render a <> "`"
      logInput
      return a

-- | Logs the current remaining input.
logInput :: Parser ()
logInput = do input <- getInput
              debug $ "Remaining input: " <> pack (show input)
              level <- head . indents <$> getState
              debug $ "Indentation level: " <> render level

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

-- | Render-prints.
print' :: Render a => a -> IO ()
print' = putStrLn . unpack . pretty

-- | Guarantees that @action@ will be taken even if @parser@ fails.
finally :: Parser a -> Parser b -> Parser a
finally parser action = (try parser <* action) <|> (action *> unexpected "failure")

----------------------------------------------------------
-----------------  Running the parser  -------------------
----------------------------------------------------------

wrapParser :: Parser a -> Parser a
wrapParser p = do
  debug "hi..."
  same
  res <- p
  logInput
  logged "is this the end?" eof
  return res
  where
    skipEmpties = logged "trying an empty" emptyLine `sepEndBy` logged "trying a newline" newline

-- | Initial state of the parser
initState :: ParserState
initState = ParserState {indents = [0], debugIndent = 0}

-- | Parses a string, retuns logs.
parseString :: String -> (Either ParseError Expr, Text)
parseString = parseWith pTopLevel

-- | Parses a string, drops logs.
parse :: String -> Either ParseError Expr
parse = fst . parseString

parse' :: String -> Either ParseError (AbsExpr Expr)
parse' s = fmap unExpr $ parse s

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
testFile = readFile >=> testString

-- | Parses a file and prints the result.
testFileVerbose :: FilePath -> IO ()
testFileVerbose = readFile >=> testStringWithVerbose pTopLevel

-- | Parses a file and prints the result.
testFileWithVerbose p = readFile >=> testStringWithVerbose p

-- | Parses a string and prints the result.
testString :: String -> IO ()
testString = testStringWith pTopLevel

-- | Tests with a specific parser.
testStringWith :: Render a => Parser a -> String -> IO ()
testStringWith parser s = do
  let parser' = wrapParser parser
      s' = "\n" ++ s ++ "\n"
  case parseWith parser' s' of
    (Right expr, _) -> print' expr
    (Left err, _) -> error $ show err

-- | Parses a string and prints the result.
testStringVerbose :: String -> IO ()
testStringVerbose = testStringWithVerbose pTopLevel

-- | Tests with a specific parser.
testStringWithVerbose :: Render a => Parser a -> String -> IO ()
testStringWithVerbose parser s = do
  let parser' = wrapParser parser
      s' = "\n" ++ s ++ "\n"
  let (res, logs) = parseWith parser' s'
  putStrLn $ unpack logs
  case res of
    Right expr -> print' expr
    Left err -> error $ show err

main :: IO ()
main = getArgs >>= \case
  "-p":path:_ -> testFile path
  opt@('-':_):_ -> error $ "Unknown option '" <> opt <> "'"
  input:_ -> testString input
  _ -> putStrLn "Please provide arguments!"
