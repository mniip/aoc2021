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
    v' = runST $ do
      mv <- V.thaw v
      V.sort mv
      V.unsafeFreeze mv
    p = v' V.! (V.length v `div` 2)

solve2 :: Vector Int -> Int
solve2 v = sum [let n = abs (x - p) in n * (n + 1) `div` 2 | x <- V.toList v]
  where
    total = V.sum v
    len = V.length v
    p = (total + V.foldl' above 0 v) `div` len
    above n x = if x * len > total then n + 1 else n
