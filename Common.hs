{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
module Common (
    module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.Identity
  , module Data.Set
  , module Data.Char
  , module Data.Monoid
  , module Data.String
  , module Data.Text
  , Pretty(..)
  , isInt, isIntTo
  ) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import "mtl" Control.Monad.Trans (liftIO)
import "mtl" Control.Monad.Identity
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

class Show a => Pretty a where
  render :: a -> Text
  render = pack . show
  pretty :: a -> Text
  pretty = render

instance Pretty Double where
  render n | isInt n = pack $ show $ floor n
           | otherwise = pack $ show n

instance Pretty String
instance Pretty Text
