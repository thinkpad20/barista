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

data PToken = PToken
  { tPosition :: SourcePos
  , tHasSpace :: Bool
  , tToken         :: Token } deriving (Show, Eq)

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
tAnyId = lexeme $ item $ TId <$> identifier

tId :: Tokenizer PToken
tId = lexeme $ try $ item $ do
  id <- identifier
  if id `elem` map snd keywords then unexpected $ "keyword: " <> unpack id
  else return $ TId id

tNum :: Tokenizer PToken
tNum = lexeme $ item $ do
  first <- many1 digit
  option (TInt $ read first) $ do
    dot <- try (char '.' <* notFollowedBy (char '.'))
    rest <- many1 digit
    return $ TFloat $ read $ first <> (dot : rest)

tKeyword :: Tokenizer PToken
tKeyword = try $ lexeme $ item $ choice $ map go keywords
  where
    go (kw, unpack -> name) = try $ do
      string name
      notFollowedBy $ oneOf identChars
      return $ TKeyword kw

------------------------------------------------------------
-------------------  Strings & regexes  --------------------
------------------------------------------------------------

tString :: Tokenizer PToken
tString = lexeme $ item $ do
  start <- oneOf "'\""
  TStr . pack <$> (many $ noneOf [start]) <* char start

tRegex :: Tokenizer PToken
tRegex = lexeme $ item $ do
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
  , tString
  , tKeyword
  , tId
  , tNum
  , tRegex
  , tSymbol
  , tDent
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
