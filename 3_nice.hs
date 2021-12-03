{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Control.Arrow
import Data.Char
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl', maximumBy, minimumBy, transpose)
import Data.Ord
import Data.Tuple
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

readInput1 :: String -> [String]
readInput1 = lines

readInput2 :: String -> [String]
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

solve1 :: [String] -> Int
solve1 xss = fromBin commons * fromBin (invert <$> commons)
  where
    invert '0' = '1'
    invert '1' = '0'
    commons = mostCommon <$> transpose xss

    fromBin :: String -> Int
    fromBin = foldl' (\n d -> n * 2 + digitToInt d) 0

    mostCommon :: Ord a => [a] -> a -- ties broken by the value itself. A larger value takes precedence
    mostCommon = fst . maximumBy (comparing swap) . M.toList . M.fromListWith (+) . map (,1)

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
