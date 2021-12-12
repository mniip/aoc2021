{-# LANGUAGE TupleSections #-}

module Main where

import AOC.Common (aocMain)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.List.Split

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> [Int]
readInput = map read . splitOn ","

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
