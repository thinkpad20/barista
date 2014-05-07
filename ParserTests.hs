{-# LANGUAGE OverloadedStrings #-}
module ParserTests (allTests, main) where

import qualified Prelude as P

import Common
import Tests
import AST
import Parser (parse, toExpr, Expr(..))

-- I write this way too many times
e = toExpr

type ETest = Test String Expr

foo, bar, baz, qux, one, two, three :: Expr
[foo, bar, baz, qux] = e . Variable <$> ["foo", "bar", "baz", "qux"]
[one, two, three] = e . Number <$> [1, 2, 3]

test desc input res = Test desc input (e res)

termTests :: ETest
termTests = TestGroup "Terms" [
]

forInTests :: ETest
forInTests = TestGroup "For statements" tests where
  tests =
    [
      test "basic" "for foo in bar\n  baz" (ForIn ["foo"] bar baz)
    , test "basic one-line" "for foo in bar then baz" (ForIn ["foo"] bar baz)
    , test "nested via one-line" "for foo in bar do for baz in qux do 1"
      (ForIn ["foo"] bar $ e (ForIn ["baz"] qux one))
    ]

doTests :: IO TesterState
doTests = runTests parse [ forInTests ]

allTests :: [TesterState -> IO TesterState]
allTests = [run parse [forInTests]]

main :: IO ()
main = doTests >> return ()
