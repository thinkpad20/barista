{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Common (
    module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.Identity
  , module Control.Monad.State.Strict
  , module Control.Monad.Writer
  , module Data.Set
  , module Data.Char
  , module Data.Monoid
  , module Data.String
  , module Data.Text
  , module Prelude
  , Render(..), Name
  , isInt, isIntTo, (>>==), (|>), (<!>), (~>)
  ) where

import Prelude (IO, Eq(..), Ord(..), Bool(..), flip, (*), (/), (^), any, zip,
                Double, String, Maybe(..), Int, Monad(..), Char, round,
                reverse, ($), (.), floor, map, Functor(..), mapM, fst, snd,
                tail, (+), (-), elem, Either(..), length, fromIntegral,
                otherwise)
import qualified Prelude as P
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import "mtl" Control.Monad.Trans (liftIO)
import "mtl" Control.Monad.State.Strict
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Writer
import Data.Set hiding (map, singleton, empty, foldr,
                        foldl, null, filter, findIndex,
                        foldl', partition, split)
import Data.Char (toLower)
import Data.Monoid
import Data.String (IsString(..))
import Data.Text hiding (length, map, toLower, empty, any, zip, reverse, tail)
import Text.Parsec

isIntTo :: Double -> Int -> Bool
isIntTo x n = do
  let rounded = fromIntegral (round x :: Int)
  (round (10 ^ n * (x - rounded)) :: Int) == 0

isInt :: Double -> Bool
isInt x = isIntTo x 10

class P.Show a => Render a where
  raw :: a -> Text
  raw = pack . P.show
  render :: a -> Text
  render = raw
  pretty :: a -> Text
  pretty = render

type Name = Text
instance Render Double where
  render n | isInt n = pack $ P.show $ floor n
           | otherwise = pack $ P.show n
instance Render Char
instance Render Text
instance Render a => Render [a]
instance (Render a, Render b) => Render (a, b)
instance Render ()

(>>==) :: Monad m => m a -> m b -> m a
action1 >>== action2 = action1 >>= \result -> action2 >> return result

(|>) = flip ($)
infixl 0 |>

(<!>) = flip (<$>)
infixl 4 <!>

(~>) = flip (.)
infixl 9 ~>

instance Render ParseError
