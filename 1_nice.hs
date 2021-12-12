module Main where

import AOC.Common (aocMain, count)

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> [Int]
readInput = map read . lines

solve1 :: [Int] -> Int
solve1 = count id . (zipWith (<) <*> tail)

solve2 :: [Int] -> Int
solve2 = count id . (zipWith (<) <*> drop 3)
