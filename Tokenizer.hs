{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Tokenizer where

import Common
import Tokens

import Text.Parsec hiding (many, (<|>), spaces)
import Data.Set hiding (map, singleton)
import Data.Char (toLower)
import System.Environment
import System.IO.Unsafe

data TokenizerState = TokenizerState
  { tsIndentLevels :: [Int]
  , tsHasSpace     :: Bool } deriving (Show, Eq)

type TokenizerError = ParseError

type Tokenizer = ParsecT String TokenizerState IO

---------------------------------------------------------
-----------------------  Helpers  -----------------------
---------------------------------------------------------

spaces :: Tokenizer String
spaces = many (char ' ') >>== \case
  [] -> return ()
  _ -> modifyState $ \s -> s {tsHasSpace=True}

anySpaces :: Tokenizer String
anySpaces = many (oneOf "\r\n\t ") >>== \case
  [] -> return ()
  _ -> modifyState $ \s -> s {tsHasSpace=True}

sstring :: String -> Tokenizer String
sstring s = lexeme $ string s

lexeme :: Tokenizer a -> Tokenizer a
lexeme tizer = tizer <* spaces

-- | Wrapper that adds metadata to a token.
item :: Tokenizer Token -> Tokenizer PToken
item tokenizer = do
  pos <- getPosition
  sps <- tsHasSpace <$> getState
  -- Reset the spaces.
  modifyState $ \s -> s {tsHasSpace=False}
  token <- tokenizer
  return PToken {
    tHasSpace = sps
  , tToken = token
  , tPosition = pos
  }

litem :: Tokenizer Token -> Tokenizer PToken
litem = lexeme . item

-- | Grabs any character until the stopping parser. Doesn't consume @stop@.
strTill :: Tokenizer a -> Tokenizer Text
strTill stop = fmap pack $ anyChar `manyTill` lookAhead stop

------------------------------------------------------------
-----------------------  Primitives  -----------------------
------------------------------------------------------------

identChars :: String
identChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_@$"

identifier :: Tokenizer Name
identifier = do
  first <- letter <|> char '_' <|> char '@' <|> char '$'
  rest <- many $ oneOf identChars
  return $ pack (first : rest)

tAnyId :: Tokenizer PToken
tAnyId = litem $ TId <$> identifier

tId :: Tokenizer PToken
tId = lexeme $ try $ item $ do
  id <- identifier
  if id `elem` map snd keywords then unexpected $ "keyword: " <> unpack id
  else return $ TId id

tNum :: Tokenizer PToken
tNum = litem $ do
  first <- many1 digit
  option (TInt $ read first) $ do
    dot <- try (char '.' <* notFollowedBy (char '.'))
    rest <- many1 digit
    return $ TFloat $ read $ first <> (dot : rest)

tKeyword :: Tokenizer PToken
tKeyword = try $ litem $ choice $ map go keywords
  where
    go (kw, unpack -> name) = try $ do
      string name
      notFollowedBy $ oneOf identChars
      return $ TKeyword kw

------------------------------------------------------------
-------------------  Strings & regexes  --------------------
------------------------------------------------------------

tRegString :: Tokenizer PToken
tRegString = litem $ char '\'' >> TStr <$> go where
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

-- | An interpolated string.
tInString :: Tokenizer PToken
tInString = item $ char '"' >> TIStr <$> go where
  go = do
    str <- Plain . pack <$> many (noneOf "\"#\\")
    anyChar >>= \case
      '\\' -> do
        c <- anyChar
        fmap ((str <> Plain ("\\" <> singleton c)) <>) $ go
      '#' -> anyChar >>= \case
        '{' -> do
           str' <- balancedString '}'
           case tokenize str' of
            Left err -> unexpected $ show err
            Right tokens -> IStr str tokens <$> go
      '"' -> return str

balancedString :: Char -> Tokenizer String
balancedString stop = reverse <$> loop ([stop], "")
  where
    loop :: ([Char], String) -> Tokenizer String
    loop (stop:rest, str) = anyChar >>= \case
      c | c == stop -> case rest of [] -> return str
                                    _  -> loop (rest, c:str)
      '\\' -> anyChar >>= \c -> loop (stop:rest, c:'\\':str)
      '#' | stop == '"' -> anyChar >>= \case
        '{' -> loop ('}':stop:rest, '{':'#':str)
        c   -> loop (stop:rest, c:'#':str)
      c | stop == '}' && c `elem` ['"', '\''] -> loop (c:stop:rest, c:str)
      c -> loop (stop:rest, c:str)

tRegex :: Tokenizer PToken
tRegex = litem $ do
  char '/'
  lookAhead anyChar >>= \case
    ' ' -> return $ TSymbol "/"
    c -> TRegex . pack <$> noneOf "/" `manyTill` try (char '/')

---------------------------------------------------------
-----------------------  Indentation  -------------------
---------------------------------------------------------

tIndent :: Tokenizer PToken
tIndent = try $ item $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- tsIndentLevels <$> getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent *> return Indent
    False -> unexpected "Not an indent"
  where pushIndent i = modifyState $ push i
        push i s = s {tsIndentLevels = i:tsIndentLevels s}

tOutdent :: Tokenizer PToken
tOutdent = try $ item $ do
  isOutdent <- (eof >> return True) <|> lookAhead (
    do newline
       newIndent <- length <$> many (char ' ')
       oldIndent:_ <- tsIndentLevels <$> getState
       return (newIndent < oldIndent))
  case isOutdent of
    True -> popIndent *> return Outdent
    False -> unexpected "Not an outdent"
  where
    popIndent = modifyState pop
    pop s = s {tsIndentLevels = tail $ tsIndentLevels s}

tNodent :: Tokenizer PToken
tNodent = try $ item $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- tsIndentLevels <$> getState
  case newIndent == oldIndent of
    True -> return Nodent
    False -> unexpected "Not a nodent"

tDent :: Tokenizer PToken
tDent = tIndent <|> tNodent <|> tOutdent

------------------------------------------------------------
----------------------- Punctuation  -----------------------
------------------------------------------------------------

-- | Order here matters: longest prefix match comes first.
validSymbols' =
  [ "++", "+=", "+", "--", "->", "-=", "-", "**", "*=", "*", "//", "/=", "/"
  , ">>", ">=", "<<", "<=", "<", ">", "=>", "===", "==", "=", "&&", "&", "||"
  , "|", "?=", "?", "!", "and=", "or=", "is", "isnt", "and", "or"]

validSymbols :: Set String
validSymbols = fromList validSymbols'

symChars :: String
symChars = "+*-/|&><=?!"

tSymbol' :: Tokenizer PToken
tSymbol' = item $ do
  sym <- many1 (oneOf symChars) <|> kwSym
  spaces
  case sym `member` validSymbols of
    True -> return $ TSymbol $ pack sym
    False ->  unexpected $ "Invalid symbol: " <> sym
  where kwSym = choice . map string $ ["and", "or", "is", "isnt"]

tSymbol :: Tokenizer PToken
tSymbol = lexeme . item . choice . map go $ validSymbols' where
  go sym = try (string sym) >> return (TSymbol $ pack sym)

tPunc :: String -> Tokenizer PToken
tPunc s = item $ sstring s >>= return . TPunc . pack

tCharPuncs :: String -> Tokenizer PToken
tCharPuncs chars = choice $ map (tPunc . pure) chars

------------------------------------------------------------
------------------------  Comments  ------------------------
------------------------------------------------------------

tLineComment :: Tokenizer PToken
tLineComment = item $ do
  char '#'
  body <- manyTill anyChar (lookAhead $ char '\n')
  pure $ TLineComment $ pack body

tBlockComment :: Tokenizer PToken
tBlockComment = item $ do
  try $ string "###"
  body <- manyTill anyChar (try $ string "###")
  pure $ TBlockComment $ pack body

------------------------------------------------------------
-------------------  High-level parsers  -------------------
------------------------------------------------------------

tTokens :: Tokenizer [PToken]
tTokens = tOneToken `manyTill` eof

tOneToken :: Tokenizer PToken
tOneToken = choice
  [ tBlockComment
  , tLineComment
  , tRegString
  , tInString
  , tKeyword
  , tId
  , tNum
  , tRegex
  , tSymbol
  , tDent
  , try $ tPunc "..."
  , try $ tPunc ".."
  , tCharPuncs "(){}[]:,;."
  ]

----------------------------------------------------------
-----------------  Running the tokenizer -----------------
----------------------------------------------------------

initState :: TokenizerState
initState = TokenizerState
  { tsIndentLevels = [0]
  , tsHasSpace = False }

tokenize :: String -> Either TokenizerError [PToken]
tokenize = runTokens tTokens

runTokens :: Tokenizer a -> String -> Either TokenizerError a
runTokens tizer input = do
  let a = runParserT tizer initState "" input
  unsafePerformIO a

puts :: String -> Tokenizer ()
puts = liftIO . putStrLn

putt :: Text -> Tokenizer ()
putt = liftIO . putStrLn . unpack

testFile :: FilePath -> IO ()
testFile = readFile >=> testString

testString :: String -> IO ()
testString s = case tokenize s of
  Right tokens -> mapM_ print tokens
  Left err -> error $ show err



--main :: IO ()
--main = getArgs >>= \case
--  "path":path:_ -> testFile path
--  input:_ -> testString input
--  [] -> error $ "Please enter a file path."
