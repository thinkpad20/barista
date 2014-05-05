{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
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
  , Render(..)
  , isInt, isIntTo, (>>==)
  ) where

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
import Data.Text hiding (length, map, toLower, empty)

isIntTo :: Double -> Int -> Bool
isIntTo x n = do
  let rounded = fromIntegral (round x :: Int)
  (round (10 ^ n * (x - rounded)) :: Int) == 0

isInt :: Double -> Bool
isInt x = isIntTo x 10

class Show a => Render a where
  raw :: a -> Text
  raw = pack . show
  render :: a -> Text
  render = raw
  pretty :: a -> Text
  pretty = render

instance Render Double where
  render n | isInt n = pack $ show $ floor n
           | otherwise = pack $ show n
instance Render Char
instance Render Text
instance Render a => Render [a]
instance (Render a, Render b) => Render (a, b)
instance Render ()

(>>==) :: Monad m => m a -> m b -> m a
action1 >>== action2 = action1 >>= \result -> action2 >> return result

