module Main where

import AOC.Common (aocMain, neighbors4, dijkstraTo')
import Data.Array
import Data.Char
import Data.Maybe
import Data.Monoid

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> Array (Int, Int) Int
readInput xs = listArray ((1, 1), (length xss, length $ head xss)) $ concat xss
  where xss = map digitToInt <$> lines xs

solve1 :: Array (Int, Int) Int -> Int
solve1 arr = getSum $ fromJust $ dijkstraTo' (fst $ bounds arr) (snd $ bounds arr)
  $ \p -> [(p', Sum $ arr ! p') | p' <- neighbors4 p, inRange (bounds arr) p']

quintuple :: Array (Int, Int) Int -> Array (Int, Int) Int
quintuple arr = listArray ((x1, y1), (x1 + 5 * w - 1, y1 + 5 * h - 1))
  [(arr ! (x, y) + p + q - 1) `mod` 9 + 1 | q <- [0..4], y <- [y1..y2], p <- [0..4], x <- [x1..x2]]
  where
    ((x1, y1), (x2, y2)) = bounds arr
    w = x2 - x1 + 1
    h = y2 - y1 + 1

solve2 :: Array (Int, Int) Int -> Int
solve2 = solve1 . quintuple
