{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Vector.Algorithms.Merge as V
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- BS.getContents
  let input = readInput contents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 input
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 input

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

printOutput :: Int -> IO ()
printOutput = print

quickSelect :: (Ord a, MV.Unbox a) => Int -> Vector a -> a
quickSelect k v = runST $ V.thaw v >>= select k
  where
    select k mv
      | MV.length mv == 1 = MV.read mv 0
      | otherwise = do
        pivot <- MV.read mv (MV.length mv - 1)
        let
          finish i = case compare k i of
            LT -> select k $ MV.slice 0 i mv
            EQ -> pure pivot
            GT -> select (k - i - 1) $ MV.slice i (MV.length mv - i - 1) mv
          goL i j
            | i == j - 1 = (< pivot) <$> MV.read mv i >>= \case
              False -> finish i
              True -> finish (i + 1)
            | i == j = finish i
            | otherwise = (< pivot) <$> MV.read mv i >>= \case
              False -> goR i j
              True -> goL (i + 1) j
          goR i j
            | i == j - 1 = (>= pivot) <$> MV.read mv i >>= \case
              False -> finish (i + 1)
              True -> finish i
            | i == j = finish i
            | otherwise = (>= pivot) <$> MV.read mv j >>= \case
              False -> MV.swap mv i j >> goL (i + 1) (j - 1)
              True -> goR i (j - 1)
        goL 0 (MV.length mv - 2)

solve1 :: Vector Int -> Int
solve1 v = sum [abs (x - p) | x <- V.toList v]
  where p = quickSelect (V.length v `div` 2) v

solve2 :: Vector Int -> Int
solve2 v = sum [let n = abs (x - p) in n * (n + 1) `div` 2 | x <- V.toList v]
  where
    total = V.sum v
    len = V.length v
    p = (total + V.foldl' above 0 v) `div` len
    above n x = if x * len > total then n + 1 else n
