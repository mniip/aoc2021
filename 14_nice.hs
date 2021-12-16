{-# LANGUAGE TupleSections #-}

module Main where

import AOC.Common (aocMain, splitOn2)
import Control.Applicative
import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> (String, Map (Char, Char) Char)
readInput = splitOn2 "\n\n" >>> second (M.fromList . map (((head &&& last) *** head) . splitOn2 " -> ") . lines)

pairs :: [Char] -> (Char, Map (Char, Char) Int)
pairs xw@(x:xs) = (x, M.fromListWith (+) $ (,1) <$> zip xw xs)

expand :: Map (Char, Char) Char -> Map (Char, Char) Int -> Map (Char, Char) Int
expand rules m = M.fromListWith (+) $ do
  (p, count) <- M.toList m
  case M.lookup p rules of
    Nothing -> [(p, count)]
    Just ch -> [((fst p, ch), count), ((ch, snd p), count)]

occurrences :: Char -> Map (Char, Char) Int -> [Int]
occurrences x m = M.elems $ M.insertWith (+) x 1 $ M.mapKeysWith (+) snd m

solve1 :: ([Char], Map (Char, Char) Char) -> Int
solve1 (xs, rules) | (x, m) <- pairs xs
  = liftA2 subtract minimum maximum $ occurrences x (iterate (expand rules) m !! 10)

solve2 :: ([Char], Map (Char, Char) Char) -> Int
solve2 (xs, rules) | (x, m) <- pairs xs
  = liftA2 subtract minimum maximum $ occurrences x (iterate (expand rules) m !! 40)
