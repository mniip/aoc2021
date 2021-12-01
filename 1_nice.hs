module Main where

import Control.Monad
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
readInput1 = map read . lines

readInput2 :: String -> [Int]
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

increases :: [Int] -> Int
increases = sum . (zipWith (\x y -> if x < y then 1 else 0) <*> tail)

solve1 :: [Int] -> Int
solve1 = increases

solve2 :: [Int] -> Int
solve2 = increases . (zipWith3 (\x y z -> x + y + z) <*> tail <*> tail . tail)
