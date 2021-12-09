module Main where

import Control.Arrow hiding ((<+>))
import Control.Monad
import Data.Array
import Data.Char
import Data.Graph
import Data.List (sortBy)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

readInput1 :: String -> Array (Int, Int) Int
readInput1 xs = listArray ((1, 1), (length xss, length $ head xss)) $ concat xss
  where xss = map digitToInt <$> lines xs

readInput2 :: String -> Array (Int, Int) Int
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

solve1 :: Array (Int, Int) Int -> Int
solve1 a = sum [1 + a ! p | p <- range $ bounds a, all (\p' -> a ! p < a ! p') $ neighbors p]
  where neighbors p = filter (inRange $ bounds a) $ [first, second] <*> [pred, succ] <*> [p]

solve2 :: Array (Int, Int) Int -> Int
solve2 a = product . take 3 . sortBy (flip compare) . map length . components . fst . graphFromEdges'
  $ [((), p, [first succ p, second succ p]) | p <- range $ bounds a, a ! p < 9]
