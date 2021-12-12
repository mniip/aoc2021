{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import AOC.Common (aocMain, readP, median)
import Control.Arrow
import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (maximumBy, minimumBy, transpose)
import Data.Ord
import Data.Tuple
import Text.Read.Lex

main :: IO ()
main = aocMain lines solve1 print lines solve2 print

solve1 :: [String] -> Int
solve1 xss = fromBin commons * fromBin (invert <$> commons)
  where
    invert '0' = '1'
    invert '1' = '0'
    commons = median <$> transpose xss

    fromBin :: String -> Int
    fromBin = readP (readIntP 2 (`elem` "01") digitToInt)

solve2 :: [String] -> Int
solve2 xss = go 0 (S.fromList xss) 0 (S.fromList xss)
  where
    go !oxy !oxyS !co2 !co2S
      | "" `S.member` oxyS = oxy * co2
      | otherwise
      , (oxy', oxyS') <- maximumBy (comparing $ first S.size . swap) $ fork oxyS
      , (co2', co2S') <- minimumBy (comparing $ first S.size . swap) $ fork co2S
      = go (oxy * 2 + digitToInt oxy') oxyS' (co2 * 2 + digitToInt co2') co2S'

    fork :: Eq a => Set [a] -> [(a, Set [a])]
    fork s
      | Just (x:_) <- S.lookupMin s = case S.spanAntitone ((== x) . head) s of
        (s1, s2) -> (x, S.mapMonotonic tail s1):fork s2
      | otherwise = []
