{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
module Parser where

import Text.Parsec hiding (spaces, parse)
import System.IO.Unsafe

import Common
import AST

data Expr = Expr SourcePos (AbsExpr Expr) deriving (Show, Eq)
type IndentState = [Int]
type Parser = ParsecT String IndentState IO

instance Pretty Expr where
  render (Expr _ e) = render e

------------------------------------------------------------
------------------- <High-level parsers> -------------------
------------------------------------------------------------

-- Entry point parser.
pTopLevel :: Parser Expr
pTopLevel = go <* eof where
  go = blockOf pStatement >>= \case
    [expr] -> return expr
    (e:exprs) -> return $ Expr (getPos e) $ Block exprs

pBlock :: Parser Expr
pBlock = item $ Block <$> indented pStatement

pStatement :: Parser Expr
pStatement = pExpr

pExpr :: Parser Expr
pExpr = choice [puts "for if: " >> inp >> pIf,
                puts "for log: " >> inp >> pLogical]

pExprOrBlock :: Parser Expr
pExprOrBlock = pBlock <|> pExpr

pTerm :: Parser Expr
pTerm = choice [pVariable, pNumber, pString, pParens]

pParens :: Parser Expr
pParens = between (schar '(') (char ')') pExpr

------------------------------------------------------------
----------------------- <Primitives> -----------------------
------------------------------------------------------------

-- | Parses any valid identifier.
pIdent :: Parser Name
pIdent = checkKeyword $ do
  first <- letter <|> char '_' <|> char '@' <|> char '$'
  rest <- many (letter <|> digit <|> char '_' <|> char '$')
  return $ pack (first : rest)

pVariable :: Parser Expr
pVariable = item $ Variable <$> pIdent

pNumber :: Parser Expr
pNumber = item $ do
  first <- many1 digit
  option (Number $ read first) $ do
    dot <- try (char '.' <* notFollowedBy (char '.'))
    rest <- many1 digit
    return $ Number $ read $ first <> (dot : rest)

------------------------------------------------------------
------------------- <Strings & regexes> --------------------
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

pRegex :: Parser Expr
pRegex = item $ do
  char '/'
  lookAhead anyChar >>= \case
    ' ' -> unexpected $ "Not a regex"
    c -> Regex . pack <$> noneOf "/" `manyTill` try (char '/')

------------------------------------------------------------
-------------- <Classes, functions, literals> --------------
------------------------------------------------------------

pClass :: Parser Expr
pClass = item $ Class <$ pKeyword "class" <*> name <*> extends <*> decs where
  name = optionMaybe (pIdent <* spaces)
  extends = optionMaybe $ pKeyword "extends" *> pExpr
  decs = optionMaybe (indented pClassDec) >>= \case
    Nothing -> return []
    Just decs -> return decs

pClassDec :: Parser (ClassDec Expr)
pClassDec = choice [ try $ ClassDecDef <$> pIdent <* schar ':' <*> pExpr
                   , ClassDecExpr <$> pStatement ]

pFunction :: Parser Expr
pFunction = item $ do
  args <- optionMaybe pArgs <* spaces >>= \case
    Nothing -> return []
    Just as -> return as
  sstring "->"
  Function args <$> pExprOrBlock
  where pArgs = schar '(' *> pIdent `sepBy` schar ',' <* char ')'

emptyTuple :: Parser [Expr]
emptyTuple = try $ schar '(' *> char ')' *> pure []

---------------------------------------------------
----------------- <Control flow> ------------------
---------------------------------------------------

pIf :: Parser Expr
--pIf = item $ If <$ pKeyword "if" <*> pExpr <*> pThen <*> pElse where
pIf = item $ do
  puts "trying if"
  inp
  pKeyword "if"
  puts "got my if"
  inp
  cond <- logged pExpr
  If cond <$> pThen <*> pElse where
  pThen = pBlock <|> pKeyword "then" *> pExpr
  pElse = optionMaybe $ pKeyword "else" *> pExprOrBlock

------------------------------------------------------------
------------ <Calling functions and attributes> ------------
------------------------------------------------------------

pCall :: Parser Expr
pCall = lexeme $ do
  func <- pCallChain
  option func $ do
    args <- emptyTuple <|> pExpr `sepBy1` schar ','
    return $ Expr (getPos func) $ Call func args

-- | This parser will grab a chain of function applications and dots.
-- For example, `foo.bar().baz(a, b).qux`. In CoffeeScript, there is
-- a syntactic distinction between `a (b) 1` and `a(b) 1`. The former
-- means `a(b(1))` and the latter `a(b)(1)`. Screwy but whatevs.
pCallChain :: Parser Expr
pCallChain = lexeme $ pTerm >>= go where
  go :: Expr -> Parser Expr
  go expr = do
    lookAhead anyChar >>= \case
      -- If there is a parens immediately following the term,
      -- it's a function call.
      '(' -> do
        puts "found a parens"
        -- Grab the arguments, then recurse.
        args <- schar '(' *> pExpr `sepBy` schar ',' <* char ')'
        go $ Expr (getPos expr) $ Call expr args
      -- If there's not, we can skip spaces.
      c -> do
        spaces
        puts $ "found a '" <> singleton c <> "'"
        -- Next, look for a dot, or just return what we have
        option expr $ do
          schar '.'
          puts "goin for the dot"
          go =<< Expr (getPos expr) . Dotted expr <$> pIdent
    -- It's possible that the lookAhead will fail, if we have no input left.
    -- Put this in just in case.
    <|> return expr

---------------------------------------------------
--------------- <Binary operators> ----------------
---------------------------------------------------

pLogical :: Parser Expr
pLogical = pLeftAssoc ["&&", "||", "and", "or"] pComparative

pComparative :: Parser Expr
pComparative = pLeftAssoc ["<", ">", "<=", ">=", "==", "!="] pAdditive

pAdditive :: Parser Expr
pAdditive = pLeftAssoc ["+", "-"] pMultiplicative

pMultiplicative :: Parser Expr
pMultiplicative = pLeftAssoc ["*", "/"] pCall

pLeftAssoc :: [String] -> Parser Expr -> Parser Expr
pLeftAssoc ops higher = logged higher >>= go where
  str = choice . map sstring
  go left = spaces *> inp *> optionMaybe (str ops <* spaces) >>= \case
    Nothing -> return left
    Just op -> do
      right <- pLeftAssoc ops higher
      go $ Expr (getPos left) $ Binary op left right

---------------------------------------------------------
----------------------- <Indentation> -------------------
---------------------------------------------------------

indent :: Parser ()
indent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent
    False -> unexpected "Not an indent"
  where pushIndent i = modifyState $ \s -> i:s

outdent :: Parser ()
outdent = try $ do
  isOutdent <- (eof >> return True) <|> lookAhead (
    do newline
       newIndent <- length <$> many (char ' ')
       oldIndent:_ <- getState
       return (newIndent < oldIndent))
  case isOutdent of
    True -> popIndent
    False -> unexpected "Not an outdent"
  where popIndent = modifyState $ \(_:s) -> s

nodent :: Parser ()
nodent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- getState
  case newIndent == oldIndent of
    True -> return ()
    False -> unexpected "Not an nodent"

same :: Parser ()
same = nodent <|> (schar ';' *> return ())

blockOf :: Parser a -> Parser [a]
blockOf p = p `sepBy1` same

indented :: Parser a -> Parser [a]
indented p = between indent outdent $ blockOf p

---------------------------------------------------------
----------------------- <Comments> ----------------------
---------------------------------------------------------

pLineComment :: Parser ()
pLineComment =
  char '#' >> manyTill anyChar (lookAhead $ char '\n') >> return ()

pBlockComment :: Parser Expr
pBlockComment = item $ do
  try $ string "###"
  body <- manyTill anyChar (try $ string "###")
  pure $ Comment $ pack body

---------------------------------------------------------
----------------------- <Helpers> -----------------------
---------------------------------------------------------

spaces :: Parser ()
spaces = (many $ oneOf " \t") *> return ()

anySpaces :: Parser ()
anySpaces = (many $ oneOf " \t\n\r") *> return ()

sstring :: String -> Parser Text
sstring s = fmap pack (string s) <* spaces

schar :: Char -> Parser Char
schar c = char c <* spaces

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces

item :: Parser (AbsExpr Expr) -> Parser Expr
item parser = Expr <$> getPosition <*> parser

item' :: Parser Expr -> Parser Expr
item' parser = item $ fmap unExpr parser

unExpr :: Expr -> AbsExpr Expr
unExpr (Expr _ abstr) = abstr

getPos :: Expr -> SourcePos
getPos (Expr pos _) = pos

pKeyword :: String -> Parser ()
pKeyword s = try (sstring s) >> return ()

checkKeyword :: Parser Text -> Parser Text
checkKeyword p = try p >>= \case
  ident | ident `member` keywords -> unexpected $ "keyword " <> show ident
        | otherwise -> return ident

keywords :: Set Text
keywords = fromList
  [ "catch", "class", "else", "extends", "false", "finally", "for"
  , "if", "in", "is", "isnt", "new", "return", "switch", "then"
  , "this", "true", "try", "when", "while"]

----------------------------------------------------------
---------------------- < Debugging > ---------------------
----------------------------------------------------------

logged :: Pretty a => Parser a -> Parser a
logged p = do
  a <- p
  puts $ "Parsed: " <> render a
  inp
  return a

inp :: Parser ()
inp = do input <- getInput
         puts $ "Remaining input: " <> render input

puts :: Text -> Parser ()
puts = liftIO . putStrLn . unpack

print' :: Pretty a => a -> IO ()
print' = putStrLn . unpack . render

----------------------------------------------------------
----------------- <Running the parser> -------------------
----------------------------------------------------------

initState :: IndentState
initState = [0]

parse :: String -> Either ParseError Expr
parse = parseWith pTopLevel

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser = unsafePerformIO . runParserT parser initState ""

parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile = parseFileWith pExpr

parseFileWith :: Parser a -> FilePath -> IO (Either ParseError a)
parseFileWith parser path = readFile path >>= return . parseWith parser

testString :: FilePath -> IO ()
testString = testStringWith pTopLevel

testStringWith :: Parser Expr -> FilePath -> IO ()
testStringWith parser s = case parseWith parser s of
  Right (Expr _ (Block exprs)) -> mapM_ print' exprs
  Right expr -> print' expr
  Left err -> error $ show err
