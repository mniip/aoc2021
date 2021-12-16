{-# LANGUAGE BangPatterns #-}

module Main where

import AOC.Common (aocMain, splitOn2, buildGraphThin)
import Data.Array
import Data.Char
import Data.Graph
import qualified Data.Set as S
import Data.Tuple

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> (Graph, Vertex -> String, String -> Vertex)
readInput xs = (g, t, f)
  where (g, t, f) = buildGraphThin [] $ [id, swap] <*> (splitOn2 "-" <$> lines xs)

solve1 :: (Graph, Vertex -> String, String -> Vertex) -> Int
solve1 (g, t, f) = countPaths S.empty start
  where
    start = f "start"
    end = f "end"
    isBig v = isUpper $ head $ t v
    countPaths !seen v
      | v `S.member` seen = 0
      | v == end = 1
      | isBig v = sum $ countPaths seen <$> g ! v
      | otherwise = sum $ countPaths (S.insert v seen) <$> g ! v

solve2 :: (Graph, Vertex -> String, String -> Vertex) -> Int
solve2 (g, t, f) = countPaths S.empty Nothing start
  where
    start = f "start"
    end = f "end"
    isBig v = isUpper $ head $ t v
    repeatable v = not (isBig v) && v `notElem` [start, end]
    countPaths !seen repeated v
      | v `S.member` seen
      , Nothing <- repeated
      , repeatable v = sum $ countPaths seen (Just v) <$> g ! v
      | v `S.member` seen = 0
      | v == end = 1
      | isBig v = sum $ countPaths seen repeated <$> g ! v
      | otherwise = sum $ countPaths (S.insert v seen) repeated <$> g ! v
