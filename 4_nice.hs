module Main where

import AOC.Common (aocMain)
import Data.Array
import Data.List (elemIndex)
import Data.List.Split
import Data.Function ((&))

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

type Bingo = Array (Int, Int) Int

readInput :: String -> ([Int], [Bingo])
readInput xs = splitOn "\n\n" xs & \(xs:xss)
    -> (map read $ splitOn "," xs, (listArray ((0, 0), (4, 4)) . map read . words) <$> xss)

whenWinning :: [Int] -> Bingo -> (Int, Int)
whenWinning xs b
  | Just k1 <- minimum [maximum [elemIndex (b ! (i, j)) xs | j <- [0..4]] | i <- [0..4]]
  , Just k2 <- minimum [maximum [elemIndex (b ! (i, j)) xs | i <- [0..4]] | j <- [0..4]]
  = (min k1 k2, sum $ filter (`notElem` take (1 + min k1 k2) xs) $ elems b)

solve1 :: ([Int], [Bingo]) -> Int
solve1 (xs, boards) = (xs !! k) * total
  where (k, total) = minimum $ whenWinning xs <$> boards


solve2 :: ([Int], [Bingo]) -> Int
solve2 (xs, boards) = (xs !! k) * total
  where (k, total) = maximum $ whenWinning xs <$> boards
