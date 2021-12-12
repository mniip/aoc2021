module Main where

import AOC.Common (aocMain, buildGraph, neighbors4)
import Control.Arrow
import Control.Lens
import Data.Array
import Data.Char
import Data.Graph
import Data.List (sortBy)

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> Array (Int, Int) Int
readInput xs = listArray ((1, 1), (length xss, length $ head xss)) $ concat xss
  where xss = map digitToInt <$> lines xs

solve1 :: Array (Int, Int) Int -> Int
solve1 a = sum [1 + a ! p | p <- range $ bounds a, all (\p' -> a ! p < a ! p') $ filter (inRange $ bounds a) $ neighbors4 p]

solve2 :: Array (Int, Int) Int -> Int
solve2 a = product . take 3 . sortBy (flip compare) . map length . components . view _1 . buildGraph []
  $ [(p, q) | p <- range $ bounds a, a ! p < 9, q <- [first succ p, second succ p]]
