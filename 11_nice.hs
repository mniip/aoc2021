module Main where

import AOC.Common (aocMain, equals, count, neighbors8)
import Data.Array
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> Array (Int, Int) Int
readInput xs = listArray ((1, 1), (length xss, length $ head xss)) $ digitToInt <$> concat xss
  where xss = lines xs

flash :: Array (Int, Int) Int -> Array (Int, Int) Int
flash arr = go S.empty $ succ <$> arr
  where
    go seen arr = case filter (\p -> arr ! p > 9 && p `S.notMember` seen) $ range $ bounds arr of
      [] -> (\v -> if v > 9 then 0 else v) <$> arr
      ps -> let updates = M.fromListWith (+) [(p', 1) | p <- ps, p' <- neighbors8 p, inRange (bounds arr) p']
        in go (seen `S.union` S.fromList ps) $ arr // [(p', v' + arr ! p') | (p', v') <- M.toList updates]

solve1 :: Array (Int, Int) Int -> Int
solve1 arr = sum $ map (count $ equals 0) $ take 100 $ tail $ iterate flash arr

solve2 :: Array (Int, Int) Int -> Int
solve2 arr = length $ takeWhile (not . all (equals 0)) $ iterate flash arr
