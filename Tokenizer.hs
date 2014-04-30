{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokenizer where

import Text.Parsec hiding (spaces)
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import "mtl" Control.Monad.Trans (liftIO)
import "mtl" Control.Monad.Identity
import Data.Set hiding (map, singleton)
import Data.Char (toLower)
import Data.Monoid
import Data.Text hiding (length, map, toLower)
import System.Environment
import System.IO.Unsafe

type Name = Text
data Token = IdToken Name
           | IntToken Integer
           | NumToken Double
           | StrToken Text
           | Symbol Name
           | Punc Text
           | Indent
           | Outdent
           | Nodent
           | LineComment Text
           | BlockComment Text
           | Keyword Keyword
           | Raw Text
           | Regex Text
           deriving (Show, Eq)

data Parsed a = Parsed SourcePos a deriving (Show)
type PToken = Parsed Token
type IndentState = [Int]
type Tokenizer = ParsecT String IndentState IO

data Keyword = TCatch | TClass | TElse | TExtends | TFalse | TFinally | TFor
             | TIf | TIn | TIs | TIsnt | TNew | TReturn | TSwitch | TThen
             | TThis | TTrue | TTry | TWhen | TWhile
             deriving (Show, Eq, Enum)

spaces :: Tokenizer ()
spaces = (many $ oneOf " \t") *> return ()

sstring :: String -> Tokenizer String
sstring s = string s <* spaces

ret :: SourcePos -> Token -> Tokenizer PToken
ret pos = return . Parsed pos

initState :: IndentState
initState = [0]

tokenize :: String -> Either ParseError [PToken]
tokenize = runTokens tTokens

lexeme :: Tokenizer a -> Tokenizer a
lexeme tizer = tizer <* spaces

item :: Tokenizer a -> Tokenizer (Parsed a)
item tizer = do
  p <- getPosition
  a <- tizer
  return $ Parsed p a

tId :: Tokenizer PToken
tId = lexeme $ item $ do
  first <- letter <|> char '_' <|> char '@' <|> char '$'
  rest <- many (letter <|> digit <|> char '_' <|> char '$')
  return $ IdToken $ pack (first : rest)

tNum :: Tokenizer PToken
tNum = lexeme $ item $ do
  first <- many1 digit
  option (IntToken $ read first) $ do
    dot <- try (char '.' <* notFollowedBy (char '.'))
    rest <- many1 digit
    return $ NumToken $ read $ first <> (dot : rest)

tString :: Tokenizer PToken
tString = lexeme $ item $ do
  start <- oneOf "'\""
  StrToken . pack <$> (many $ noneOf [start]) <* char start

tIndent :: Tokenizer PToken
tIndent = try $ item $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent *> return Indent
    False -> unexpected "Not an indent"
  where pushIndent i = modifyState $ \s -> i:s

tOutdent :: Tokenizer PToken
tOutdent = try $ item $ do
  isOutdent <- (eof >> return True) <|> lookAhead (
    do newline
       newIndent <- length <$> many (char ' ')
       oldIndent:_ <- getState
       return (newIndent < oldIndent))
  case isOutdent of
    True -> popIndent *> return Outdent
    False -> unexpected "Not an outdent"
  where popIndent = modifyState $ \(_:s) -> s

tNodent :: Tokenizer PToken
tNodent = try $ item $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- getState
  case newIndent == oldIndent of
    True -> return Nodent
    False -> unexpected "Not an nodent"

tDent :: Tokenizer PToken
tDent = tIndent <|> tNodent <|> tOutdent

tTokens :: Tokenizer [PToken]
tTokens = tToken `manyTill` eof

tPunc :: String -> Tokenizer PToken
tPunc s = item $ sstring s >>= return . Punc . pack

tCharPuncs :: String -> Tokenizer PToken
tCharPuncs chars = choice $ map (tPunc . pure) chars

tStrPuncs :: [String] -> Tokenizer PToken
tStrPuncs strs = choice $ map tPunc strs

tLineComment :: Tokenizer PToken
tLineComment = item $ do
  char '#'
  body <- manyTill anyChar (lookAhead $ char '\n')
  pure $ LineComment $ pack body

tBlockComment :: Tokenizer PToken
tBlockComment = item $ do
  try $ string "###"
  body <- manyTill anyChar (try $ string "###")
  pure $ BlockComment $ pack body

tKeyWord :: Tokenizer PToken
tKeyWord = try $ lexeme $ item $ do
  let trans (_:c:name) = toLower c : name
      kwn kw = (kw, trans $ show kw)
      names = map kwn [TCatch .. TWhile]
  go names
  where
    go ((kw, name):rest) = flip (<|>) (go rest) $ do
      string name
      return $ Keyword kw
    go [] = unexpected "Not a keyword"

tRegex :: Tokenizer PToken
tRegex = lexeme $ item $ do
  char '/'
  lookAhead anyChar >>= \case
    ' ' -> return $ Symbol "/"
    c -> Regex . pack <$> noneOf "/" `manyTill` try (char '/')

validSymbols :: Set String
validSymbols = fromList
  [ "+", "*", "-", "/", ">", "<", ">=", "<=", "==", "===", "&", "|", "&&"
  , "||", "^", "**", "//", "+=", "-=", "*=", "/=", "->", "=>", "=", "?", "=->"]

symChars :: String
symChars = "+*-/|&><=@?"

tSymbol :: Tokenizer PToken
tSymbol = do
  pos <- getPosition
  sym <- many1 $ oneOf symChars
  spaces
  case sym `member` validSymbols of
    True -> ret pos $ Symbol $ pack sym
    False ->  unexpected $ "Invalid symbol: " <> sym

tToken :: Tokenizer PToken
tToken = choice
  [ tBlockComment
  , tLineComment
  , tString
  , tKeyWord
  , tId
  , tNum
  , tRegex
  , tSymbol
  , tDent
  , tPunc ".."
  , tCharPuncs "(){}[]:,;."
  ]

showInput "" = ""
showInput (' ':cs) = '_' : showInput cs
showInput ('\n':cs) = '|' : showInput cs
showInput (c:cs) = c : showInput cs

runTokens :: Tokenizer a -> String -> Either ParseError a
runTokens tizer = unsafePerformIO . runParserT tizer initState ""

puts :: String -> Tokenizer ()
puts = liftIO . putStrLn

tokenizeFile :: FilePath -> IO (Either ParseError [PToken])
tokenizeFile path = readFile path >>= return . tokenize

testString :: FilePath -> IO ()
testString s = case tokenize s of
  Right tokens -> mapM_ print tokens
  Left err -> error $ show err

testFile :: FilePath -> IO ()
testFile p = tokenizeFile p >>= \case
  Right tokens -> mapM_ print tokens
  Left err -> error $ show err

main :: IO ()
main = getArgs >>= \case
  "path":path:_ -> testFile path
  input:_ -> testString input
  [] -> error $ "Please enter a file path."
