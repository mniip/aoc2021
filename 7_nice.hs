module Main where

import Control.Monad
import Data.List (sort)
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

readInput1 :: String -> [Int]
readInput1 = map read . splitOn ","

readInput2 :: String -> [Int]
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

solve1 :: [Int] -> Int
solve1 xs = sum [abs (x - p) | x <- xs]
  where p = sort xs !! (length xs `div` 2)

solve2 :: [Int] -> Int
solve2 xs = sum [let n = abs (x - p) in n * (n + 1) `div` 2 | x <- xs]
  where p = (sum xs + length (filter (\x -> x * length xs > sum xs) xs)) `div` length xs
