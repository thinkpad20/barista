{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

keywords :: Set Text
keywords = fromList
  [ "catch", "class", "else", "extends", "false", "finally", "for"
  , "if", "in", "is", "isnt", "new", "return", "switch", "then"
  , "this", "true", "try", "when", "while"]

spaces :: Parser ()
spaces = (many $ oneOf " \t") *> return ()

anySpaces :: Parser ()
anySpaces = (many $ oneOf " \t\n\r") *> return ()

sstring :: String -> Parser Text
sstring s = fmap pack (string s) <* spaces

schar :: Char -> Parser Char
schar c = char c <* spaces

initState :: IndentState
initState = [0]

--tokenize :: String -> Either ParseError [Expr]
--tokenize = runTokens tTokens

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

checkKeyword :: Parser Text -> Parser Text
checkKeyword p = try p >>= \case
  ident | ident `member` keywords -> unexpected $ "keyword " <> show ident
        | otherwise -> return ident

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

pString :: Parser Expr
pString = pRegString <|> pInString

pFunction :: Parser Expr
pFunction = item $ do
  args <- optionMaybe pArgs <* spaces >>= \case
    Nothing -> return []
    Just as -> return as
  sstring "->"
  Function args <$> pExprOrBlock
  where pArgs = schar '(' *> pIdent `sepBy` schar ',' <* char ')'

pCallChain :: Parser Expr
pCallChain = lexeme $ pTerm >>= go where
  go :: Expr -> Parser Expr
  go expr = optionMaybe (lookAhead $ char '(') >>= \case
    Nothing -> return expr
    Just _ -> do
      args <- schar '(' *> pExpr `sepBy` schar ',' <* char ')'
      go $ Expr (getPos expr) $ Call expr args

pCall :: Parser Expr
pCall = lexeme $ do
  func <- pCallChain
  option func $ do
    args <- emptyTuple <|> pExpr `sepBy1` schar ','
    return $ Expr (getPos func) $ Call func args

emptyTuple :: Parser [Expr]
emptyTuple = schar '(' *> char ')' *> pure []

validSymbols :: Set String
validSymbols = fromList
  [ "+", "*", "-", "/", ">", "<", ">=", "<=", "==", "===", "&", "|", "&&"
  , "||", "^", "**", "//", "+=", "-=", "*=", "/=", "->", "=>", "=", "?", "=->"]

symChars :: String
symChars = "+*-/|&><=@?"

pLogical :: Parser Expr
pLogical = pLeftAssoc ["&&", "||", "and", "or"] pComparative

pComparative :: Parser Expr
pComparative = pLeftAssoc ["<", ">", "<=", ">=", "==", "!="] pAdditive

pAdditive :: Parser Expr
pAdditive = pLeftAssoc ["+", "-"] pMultiplicative

pMultiplicative :: Parser Expr
pMultiplicative = pLeftAssoc ["*", "/"] pCallChain

pLeftAssoc :: [String] -> Parser Expr -> Parser Expr
pLeftAssoc ops higher = logged higher >>= go where
  str = choice . map sstring
  go left = spaces *> inp *> optionMaybe (str ops <* spaces) >>= \case
    Nothing -> return left
    Just op -> do
      right <- pLeftAssoc ops higher
      go $ Expr (getPos left) $ Binary op left right

pExprOrBlock :: Parser Expr
pExprOrBlock = pBlock <|> pExpr

pBlock :: Parser Expr
pBlock = item $ Block <$> (indent *> (pExpr `sepBy1` same) <* outdent)

pParens :: Parser Expr
pParens = between (schar '(') (char ')') $ pExpr

pTerm :: Parser Expr
pTerm = choice [pVariable, pNumber, pString, pParens]

pExpr :: Parser Expr
pExpr = pLogical

pTopLevel :: Parser Expr
pTopLevel = pExpr `sepBy1` same >>= \case
  [expr] -> return expr
  (e:exprs) -> return $ Expr (getPos e) $ Block exprs

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

pLineComment :: Parser ()
pLineComment =
  char '#' >> manyTill anyChar (lookAhead $ char '\n') >> return ()

pBlockComment :: Parser Expr
pBlockComment = item $ do
  try $ string "###"
  body <- manyTill anyChar (try $ string "###")
  pure $ Comment $ pack body

pRegex :: Parser Expr
pRegex = item $ do
  char '/'
  lookAhead anyChar >>= \case
    ' ' -> unexpected $ "Not a regex"
    c -> Regex . pack <$> noneOf "/" `manyTill` try (char '/')

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

parse :: String -> Either ParseError Expr
parse = parseWith pTopLevel

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser = unsafePerformIO . runParserT parser initState ""

parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile = parseFileWith pExpr

parseFileWith :: Parser a -> FilePath -> IO (Either ParseError a)
parseFileWith parser path = readFile path >>= return . parseWith parser

testString :: FilePath -> IO ()
testString = testStringWith pExpr

testStringWith :: Parser Expr -> FilePath -> IO ()
testStringWith parser s = case parseWith parser s of
  Right (Expr _ (Block exprs)) -> mapM_ print' exprs
  Right expr -> print' expr
  Left err -> error $ show err

print' :: Pretty a => a -> IO ()
print' = putStrLn . unpack . render
