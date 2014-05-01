{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.Parsec hiding (spaces, parse)
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import "mtl" Control.Monad.Trans (liftIO)
import "mtl" Control.Monad.Identity
import Data.Set hiding (map, singleton)
import Data.Char (toLower)
import Data.Monoid
import Data.String (IsString(..))
import Data.Text hiding (length, map, toLower)
import System.Environment
import System.IO.Unsafe

type Name = Text
data AbsExpr expr = Variable Name
                  | Number Double
                  | String Text
                  | Regex Text
                  | InString InString
                  | Assign Pattern expr
                  | Block [expr]
                  | Array [expr]
                  | Function [Name] expr
                  | Call expr [expr]
                  | Binary Name expr expr
                  | Prefix Name expr
                  | Postfix expr Name
                  | Return (Maybe expr)
                  | Throw expr
                  | Switch expr [SwitchCase expr]
                  | If expr expr (Maybe expr)
                  | ForIn Pattern expr expr
                  | ForOf Pattern expr expr
                  | While expr expr
                  | TryCatch expr Name expr (Maybe expr)
                  | Comment Text
                  | Break
                  | Continue
                  deriving (Show, Eq)

data InString = Plain Text
              | Interpolated InString Expr InString
              deriving (Show, Eq)

data SwitchCase expr = SwitchCase expr deriving (Show, Eq)
data Pattern = Pattern deriving (Show, Eq)
data Expr = Expr SourcePos (AbsExpr Expr) deriving (Eq)
data Parsed a = Parsed SourcePos a deriving (Show)
type IndentState = [Int]
type Parser = ParsecT String IndentState IO

instance Show Expr where
  show (Expr _ e) = show e

instance IsString InString where
  fromString str = Plain $ pack str

instance Monoid InString where
  mempty = Plain mempty
  is1 `mappend` is2 = case (is1, is2) of
    (Plain s, Plain s') -> Plain (s <> s')
    (s, Interpolated is e is') -> Interpolated (s <> is) e is'
    (Interpolated is e is', s) -> Interpolated is e (is' <> s)

keywords :: [Text]
keywords = [ "catch", "class", "else", "extends", "false", "finally", "for"
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

pIdent :: Parser Name
pIdent = do
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
        '{' -> Interpolated (Plain str) <$> (pExpr <* char '}') <*> go
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

pAdditive :: Parser Expr
pAdditive = pLeftAssoc "+-" pMultiplicative

pMultiplicative :: Parser Expr
pMultiplicative = pLeftAssoc "*/" pCallChain

pLeftAssoc :: String -> Parser Expr -> Parser Expr
pLeftAssoc ops higher = higher >>= go where
  go left = spaces *> inp *> optionMaybe (oneOf ops <* spaces) >>= \case
    Nothing -> return left
    Just op -> do
      right <- pLeftAssoc ops higher
      go $ Expr (getPos left) $ Binary (singleton op) left right

pExprOrBlock :: Parser Expr
pExprOrBlock = pBlock <|> pExpr

pBlock :: Parser Expr
pBlock = item $ Block <$> (indent *> (pExpr `sepBy1` same) <* outdent)

pParens :: Parser Expr
pParens = between (schar '(') (char ')') $ pExpr

pTerm :: Parser Expr
pTerm = choice [pVariable, pNumber, pString, pParens]

pExpr :: Parser Expr
pExpr = pAdditive

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

logged :: Show a => Parser a -> Parser a
logged p = do
  a <- p
  puts $ "Parsed: " <> show a
  inp
  return a

inp :: Parser ()
inp = do input <- getInput
         puts $ "Remaining input: " <> show input

--tSymbol :: Parser Expr
--tSymbol = do
--  pos <- getPosition
--  sym <- many1 $ oneOf symChars
--  spaces
--  case sym `member` validSymbols of
--    True -> ret pos $ Symbol $ pack sym
--    False ->  unexpected $ "Invalid symbol: " <> sym

--tToken :: Parser Expr
--tToken = choice
--  [ tBlockComment
--  , tLineComment
--  , tString
--  , tKeyWord
--  , tId
--  , tNum
--  , tRegex
--  , tSymbol
--  , tDent
--  , tPunc ".."
--  , tCharPuncs "(){}[]:,;."
--  ]

--showInput "" = ""
--showInput (' ':cs) = '_' : showInput cs
--showInput ('\n':cs) = '|' : showInput cs
--showInput (c:cs) = c : showInput cs

puts :: String -> Parser ()
puts = liftIO . putStrLn

--tokenizeFile :: FilePath -> IO (Either ParseError [Expr])
--tokenizeFile path = readFile path >>= return . tokenize

parse :: String -> Either ParseError Expr
parse = runParse pExpr

runParse :: Parser a -> String -> Either ParseError a
runParse parser = unsafePerformIO . runParserT parser initState ""

testStringWith :: Parser Expr -> FilePath -> IO ()
testStringWith parser s = case runParse parser s of
  Right (Expr _ (Block exprs)) -> mapM_ print exprs
  Right expr -> print expr
  Left err -> error $ show err

testString :: FilePath -> IO ()
testString = testStringWith pExpr

--testFile :: FilePath -> IO ()
--testFile p = tokenizeFile p >>= \case
--  Right tokens -> mapM_ print tokens
--  Left err -> error $ show err

--main :: IO ()
--main = getArgs >>= \case
--  "path":path:_ -> testFile path
--  input:_ -> testString input
--  [] -> error $ "Please enter a file path."
