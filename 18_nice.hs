{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import AOC.Common (aocMain, readP)
import Control.Applicative
import Data.List (foldl1')
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

data Snailfish = Regular !Int | Pair !Snailfish !Snailfish deriving (Eq, Ord, Show)

readInput :: String -> [Snailfish]
readInput = map (readP tree) . lines
  where
    tree = (Regular <$> P.readS_to_P reads)
      <|> (Pair <$> (P.char '[' *> tree) <*> (P.char ',' *> tree <* P.char ']'))

magnitude :: Snailfish -> Int
magnitude = go 1 0
  where
    go !m !s (Regular n) = s + m * n
    go m s (Pair l r) = go (m * 3) (go (m * 2) s r) l

reduce :: Snailfish -> Snailfish
reduce t = case explode (0 :: Int) t of
  Just (t', _, _) -> reduce t'
  Nothing -> case split t of
    Just t' -> reduce t'
    Nothing -> t
  where
    split (Regular n)
      | n >= 10 = Just $ Pair (Regular $ n `div` 2) (Regular $ (n + 1) `div` 2)
      | otherwise = Nothing
    split (Pair l r)
      | Just l' <- split l = Just $ Pair l' r
      | Just r' <- split r = Just $ Pair l r'
      | otherwise = Nothing
    explode !d (Regular _) = Nothing
    explode d (Pair (Regular n) (Regular m)) | d >= 4 = Just (Regular 0, addR n, addL m)
    explode d (Pair l r)
      | Just (l', fl, fr) <- explode (d + 1) l = Just (Pair l' (fr r), fl, id)
      | Just (r', fl, fr) <- explode (d + 1) r = Just (Pair (fl l) r', id, fr)
      | otherwise = Nothing
    addL !n (Regular i) = Regular (n + i)
    addL !n (Pair x y) = Pair (addL n x) y
    addR !n (Regular i) = Regular (n + i)
    addR !n (Pair x y) = Pair x (addR n y)

snailfishAdd :: Snailfish -> Snailfish -> Snailfish
snailfishAdd x y = reduce $ Pair x y

solve1 :: [Snailfish] -> Int
solve1 = magnitude . foldl1' snailfishAdd

solve2 :: [Snailfish] -> Int
solve2 xs = maximum [magnitude $ snailfishAdd x y | (x, y) <- pairs xs []]
  where
    pairs [] _ = []
    pairs (x:xs) ys = map (x,) (xs ++ ys) ++ pairs xs (x:ys)
