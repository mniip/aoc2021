module Main where

import AOC.Common (aocMain, equals, splitOn2)
import Control.Arrow
import Data.List (foldl')
import Data.Functor
import qualified Data.Set as S
import qualified Data.Map.Strict as M

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 (putStrLn . ocr)

type Grid = S.Set (Int, Int)
type Fold = (Bool, Int)

readInput :: String -> (Grid, [Fold])
readInput = splitOn2 "\n\n" >>> \(points, folds) ->
  ( S.fromList $ ((read *** read) . splitOn2 ",") <$> lines points
  , (equals "fold along x" *** read) . splitOn2 "=" <$> lines folds
  )

fold :: Grid -> Fold -> Grid
fold s (axis, z) = S.map ((if axis then first else second) $ reflect z) s
  where reflect z w = z - abs (z - w)

solve1 :: (Grid, [Fold]) -> Int
solve1 (s, f:_) = S.size $ fold s f

solve2 :: (Grid, [Fold]) -> Grid
solve2 (s, fs) = foldl' fold s fs

ocr :: Grid -> String
ocr s = [minX,minX+5..maxX] <&> \lx -> charMap M.! foldl' (\n d -> 2 * n + d) 0
  [fromEnum $ (x, y) `S.member` s | y <- [minY..maxY], x <- [lx..lx+3]]
  where
    sList = S.toList s
    minX = minimum $ fst <$> sList
    minY = minimum $ snd <$> sList
    maxX = maximum $ fst <$> sList
    maxY = maximum $ snd <$> sList
    charMap = M.fromList
      [ (0x699F99, 'A')
      , (0xE9E99E, 'B')
      , (0x698896, 'C')
      , (0xF8E88F, 'E')
      , (0xF8E888, 'F')
      , (0x698B97, 'G')
      , (0x99F999, 'H')
      , (0x311196, 'J')
      , (0x9ACAA9, 'K')
      , (0x88888F, 'L')
      , (0xE99E88, 'P')
      , (0xE99EA9, 'R')
      , (0x999996, 'U')
      , (0xF1248F, 'Z')
      ]
