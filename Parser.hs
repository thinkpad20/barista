module Parser where

import Tokenizer

data AST = AST

type Parser = ParsecT [Token] () Identity

parseTokens :: [Token] -> Either ParseError AST
parseTokens = undefined

parse :: String -> Either ParseError AST
parse = tokenize >=> parseTokens
