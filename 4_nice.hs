module Main where

import Control.Monad
import Data.Array
import Data.List (elemIndex)
import Data.List.Split
import Data.Function ((&))
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

type Bingo = Array (Int, Int) Int

readInput1 :: String -> ([Int], [Bingo])
readInput1 xs = splitOn "\n\n" xs & \(xs:xss)
    -> (map read $ splitOn "," xs, (listArray ((0, 0), (4, 4)) . map read . words) <$> xss)

readInput2 :: String -> ([Int], [Bingo])
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

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
