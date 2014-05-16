module Tokens (
    Token(..), Keyword(..), IStr(..), PToken(..), keywords
  ) where

import Text.Parsec (SourcePos)
import Common

data Token = TId Name
           | TInt Integer
           | TFloat Double
           | TStr Text
           | TIStr IStr
           | TSymbol Name
           | TPunc Char
           | Indent
           | Outdent
           | Nodent
           | TLineComment Text
           | TBlockComment Text
           | TKeyword Keyword
           | TRaw Text
           | TRegex Text
           deriving (Show, Eq)

data Keyword = TCatch | TClass | TElse | TExtends | TFalse | TFinally | TFor
             | TIf | TIn | TIs | TIsnt | TNew | TReturn | TSwitch | TThen
             | TThis | TTrue | TTry | TUnless | TWhen | TWhile
             deriving (Show, Eq, Enum)

data IStr = Plain Text
          | IStr IStr [PToken] IStr
          deriving (Show, Eq)

instance Monoid IStr where
  mempty = Plain mempty
  is1 `mappend` is2 = case (is1, is2) of
    (Plain s, Plain s') -> Plain (s <> s')
    (s, IStr is t is') -> IStr (s <> is) t is'
    (IStr is t is', s) -> IStr is t (is' <> s)

data PToken = PToken
  { tPosition :: SourcePos
  , tHasSpace :: Bool
  , tToken         :: Token } deriving (Show, Eq)

addChar :: IStr -> Char -> IStr
addChar (Plain t) c = Plain $ t `snoc` c
addChar (IStr is t is') c = IStr is t (is' `addChar` c)

keywords :: [(Keyword, Name)]
keywords = do
  let trans (_:c:name) = pack $ toLower c : name
      kwn kw = (kw, trans $ show kw)
  map kwn [TCatch .. TWhile]
