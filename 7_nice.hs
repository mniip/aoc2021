{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import System.Environment
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Algorithms.Merge as V

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
readInput bs = case parseOnly parser bs of
  Right r -> r
  where
    parser = uncurry V.fromListN <$> go1
    go1 = do
      x <- decimal
      (n, xs) <- go2
      let n' = n + 1
      n' `seq` pure (n', x:xs)
    go2 = (char ',' *> go1) <|> pure (0, [])

printOutput :: Int -> IO ()
printOutput = print

solve1 :: Vector Int -> Int
solve1 v = sum [abs (x - p) | x <- V.toList v]
  where
    p = runST $ do
      mv <- V.thaw v
      select (V.length v `div` 2) mv
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

solve2 :: Vector Int -> Int
solve2 v = sum [let n = abs (x - p) in n * (n + 1) `div` 2 | x <- V.toList v]
  where
    total = V.sum v
    len = V.length v
    p = (total + V.foldl' above 0 v) `div` len
    above n x = if x * len > total then n + 1 else n
