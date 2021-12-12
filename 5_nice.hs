module Main where

import AOC.Common (aocMain, count, readP)
import qualified Data.Map.Strict as M
import Numeric
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> [((Int, Int), (Int, Int))]
readInput = map (readP segment) . lines
  where
    segment = (,) <$> point <* P.string " -> " <*> point
    point = (,) <$> P.readS_to_P (readSigned readDec) <* P.string "," <*> P.readS_to_P (readSigned readDec)

solve1 :: [((Int, Int), (Int, Int))] -> Int
solve1 = count (>= 2) . M.elems . M.unionsWith (+) . map render
  where
    render ((x1, y1), (x2, y2))
      | x1 == x2 = M.fromList [((x1, y), 1) | y <- [min y1 y2..max y1 y2]]
      | y1 == y2 = M.fromList [((x, y1), 1) | x <- [min x1 x2..max x1 x2]]
      | otherwise = M.empty

solve2 :: [((Int, Int), (Int, Int))] -> Int
solve2 = count (>= 2) . M.elems . M.unionsWith (+) . map render
  where
    render ((x1, y1), (x2, y2)) = M.fromList [((x1 + p * dx, y1 + p * dy), 1) | p <- [0..d]]
      where
        d = max (abs (x1 - x2)) (abs (y1 - y2))
        dx = (x2 - x1) `div` d
        dy = (y2 - y1) `div` d
