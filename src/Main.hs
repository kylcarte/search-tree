{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Tree.Search
import Control.Monad
import System.Environment (getArgs)

main :: IO ()
main = testSearch breadthSearch iterDeepening pyth

testSearch :: Show a => (t -> [a]) -> (t -> [a]) -> t -> IO ()
testSearch bfs iter test = getArgs >>= check
  where
  check [key,ns] | [(n,"")] <- reads ns =
      print
    $ take n
    $ select key test
  select = \case
    "bfs"  -> bfs
    "iter" -> iter

pyth :: MonadPlus m => m (Int,Int,Int)
pyth = do
  x <- from 1
  y <- from 1
  z <- from 1
  returnWhen (x*x + y*y == z*z) (x,y,z)

returnWhen :: MonadPlus m => Bool -> a -> m a
returnWhen b a = if b then return a else mzero

from :: MonadPlus m => Int -> m Int
from i = return i `mplus` from (succ i)

