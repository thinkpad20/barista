{-# LANGUAGE PackageImports #-}
module Parser where

import Text.Parsec hiding (spaces)
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import "mtl" Control.Monad.Trans (liftIO)
import "mtl" Control.Monad.Identity
import Data.Monoid
import Data.Text hiding (length)
import System.IO.Unsafe


type Name = Text
data Token = IdToken Name
           | NumToken Double
           | StrToken Text
           | Symbol Name
           | Punc Text
           | Indent
           | Outdent
           | Nodent
           deriving (Show)

data AST = AST
type IndentState = [Int]
type Tokenizer = ParsecT String IndentState IO
type Parser = ParsecT [Token] () Identity

spaces :: Tokenizer ()
spaces = (many $ oneOf " \t") *> return ()

initState :: IndentState
initState = [0]

tokenize :: String -> Either ParseError [Token]
tokenize = runTokens tTokens

lexeme :: Tokenizer a -> Tokenizer a
lexeme tizer = tizer <* spaces

tId :: Tokenizer Token
tId = lexeme $ do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return $ IdToken $ pack (first : rest)

tNum :: Tokenizer Token
tNum = lexeme $ do
  first <- many1 digit
  option (NumToken $ read first) $ do
    dot <- char '.'
    rest <- many1 digit
    return (NumToken $ read $ first <> (dot : rest))

tStr :: Tokenizer Token
tStr = lexeme $ do
  start <- oneOf "'\""
  content <- many $ noneOf [start]
  stop <- char start
  return $ StrToken $ pack content

tIndent :: Tokenizer Token
tIndent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- getState
  case newIndent > oldIndent of
    True -> pushIndent newIndent *> return Indent
    False -> unexpected "Not an indent"
  where
    pushIndent :: Int -> Tokenizer ()
    pushIndent i = modifyState $ \s -> i:s

tOutdent :: Tokenizer Token
tOutdent = try $ do
  isOutdent <- (eof >> return True) <|> lookAhead (
    do newline
       newIndent <- length <$> many (char ' ')
       oldIndent:_ <- getState
       return (newIndent < oldIndent))
  case isOutdent of
    True -> popIndent *> return Outdent
    False -> unexpected "Not an outdent"
  where
    popIndent :: Tokenizer ()
    popIndent = modifyState $ \(_:s) -> s

tNodent :: Tokenizer Token
tNodent = try $ do
  newline
  newIndent <- length <$> many (char ' ')
  oldIndent:_ <- getState
  case newIndent == oldIndent of
    True -> return Nodent
    False -> unexpected "Not an nodent"

tDent :: Tokenizer Token
tDent = tIndent <|> tNodent <|> tOutdent

tToken :: Tokenizer Token
tToken = choice [tId, tDent]

tTokens :: Tokenizer [Token]
tTokens = do
  tok <- tToken
  liftIO $ putStrLn $ "TOKEN: " ++ show tok
  i <- getInput
  liftIO $ putStrLn $ "Remaining: " ++ showInput i
  done <- optionMaybe eof
  case done of
    Nothing -> do
      rest <- tTokens
      return (tok : rest)
    Just _ -> return [tok]

showInput "" = ""
showInput (' ':cs) = '_' : showInput cs
showInput ('\n':cs) = '|' : showInput cs
showInput (c:cs) = c : showInput cs

parseTokens :: [Token] -> Either ParseError AST
parseTokens = undefined

parse :: String -> Either ParseError AST
parse = tokenize >=> parseTokens

runTokens :: Tokenizer a -> String -> Either ParseError a
runTokens tizer = unsafePerformIO . runParserT tizer initState ""
