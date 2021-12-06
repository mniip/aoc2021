{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import qualified Data.IntMap as IM
import Data.List (foldl')
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

simulate :: Int -> [Int] -> Int
simulate n xs = sum $ IM.elems $ foldl' (const . step) (IM.fromListWith (+) $ (,1) <$> xs) [1..n]
  where
    step m =
      IM.unionWith (+)
        (IM.mapKeysMonotonic pred (IM.delete 0 m))
        (IM.fromList $ (,IM.findWithDefault 0 0 m) <$> [6, 8])

solve1 :: [Int] -> Int
solve1 = simulate 80

solve2 :: [Int] -> Int
solve2 = simulate 256
