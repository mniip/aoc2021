{-# LANGUAGE LambdaCase #-}

module Main where

import AOC.Common (aocMain, arithSum)
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Vector.Algorithms.Merge as V
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: ByteString -> Vector Int
readInput bs = runST $ do
  mv <- MV.unsafeNew 1
  go 0 bs 0 mv
  where
    go i bs n mv = case BS.uncons bs of
      Just (d, bs') | isDigit d
        -> go i bs' (n * 10 + digitToInt d) mv
      Just (',', bs')
        -> add i n mv >>= go (i + 1) bs' 0
      Just (ws, bs') | isSpace ws
        -> go i bs' n mv
      Nothing
        -> add i n mv >>= V.unsafeFreeze . MV.take (i + 1)
    add i n mv
      | i < MV.length mv = MV.write mv i n >> pure mv
      | otherwise = MV.unsafeGrow mv (MV.length mv) >>= add i n

select :: (Ord a, MV.Unbox a) => Int -> Vector a -> a
select k v = runST $ do
  mv <- V.thaw v
  V.sort mv
  MV.read mv k

solve1 :: Vector Int -> Int
solve1 v = sum [abs (x - p) | x <- V.toList v]
  where p = select (V.length v `div` 2) v

solve2 :: Vector Int -> Int
solve2 v = sum [arithSum 0 $ abs (x - p) | x <- V.toList v]
  where
    total = V.sum v
    len = V.length v
    p = (total + V.foldl' above 0 v) `div` len
    above n x = if x * len > total then n + 1 else n
