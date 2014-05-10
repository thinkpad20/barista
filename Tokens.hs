module Tokens (
    Token(..), Keyword(..), keywords
  ) where

import Common

data Token = TId Name
           | TInt Integer
           | TFloat Double
           | TStr Text
           | TSymbol Name
           | TPunc Text
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

keywords :: [(Keyword, Name)]
keywords = do
  let trans (_:c:name) = pack $ toLower c : name
      kwn kw = (kw, trans $ show kw)
  map kwn [TCatch .. TWhile]
