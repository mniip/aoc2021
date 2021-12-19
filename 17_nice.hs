module Main where

import AOC.Common (aocMain, readP, ceilDiv, isqrt, ceilIsqrt)
import Control.Monad
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> (Int, Int, Int, Int)
readInput = readP $ do
  P.string "target area: x="
  x1 <- P.readS_to_P reads
  P.string ".."
  x2 <- P.readS_to_P reads
  P.string ", y="
  y1 <- P.readS_to_P reads
  P.string ".."
  y2 <- P.readS_to_P reads
  P.skipSpaces
  pure (x1, x2, y1, y2)

invArithCeil :: Int -> Int
invArithCeil x = (ceilIsqrt (8 * x + 1) - 1) `ceilDiv` 2

invArithFloor :: Int -> Int
invArithFloor x = (isqrt (8 * x + 1) - 1) `div` 2

hitRanges :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
hitRanges (x1, x2, y1, y2) = do
  let maxt = 2 * max (abs y1) (abs y2)
  t <- [1 .. maxt]
  let
    tS = t * (t - 1) `div` 2
    minvy = (min y1 y2 + tS) `ceilDiv` t
    maxvy = (max y1 y2 + tS) `div` t
    vxsPos
      | max x1 x2 < 0 = []
      | a <- max 0 $ min x1 x2
      , b <- max x1 x2
      = [(invArithCeil a, min (t - 1) $ invArithFloor b), (max t $ (a + tS) `ceilDiv` t, (b + tS) `div` t)]
    vxsNeg
      | min x1 x2 >= 0 = []
      | a <- -(min (-1) $ max x1 x2)
      , b <- -min x1 x2
      = [(-(min (t - 1) $ invArithFloor b), -invArithCeil a), (-((b + tS) `div` t), -(max t $ (a + tS) `ceilDiv` t))]
  guard $ minvy <= maxvy
  (minvx, maxvx) <- join [vxsNeg, vxsPos]
  guard $ minvx <= maxvx
  pure (minvx, maxvx, minvy, maxvy)

solve1 :: (Int, Int, Int, Int) -> Int
solve1 target = maximum [maxY maxvy | (_, _, _, maxvy) <- hitRanges target]
  where maxY vy = vy * (vy + 1) `div` 2

solve2 :: (Int, Int, Int, Int) -> Int
solve2 target = S.size $ S.fromList
  [(vx, vy) | (minvx, maxvx, minvy, maxvy) <- hitRanges target, vx <- [minvx..maxvx], vy <- [minvy..maxvy]]
