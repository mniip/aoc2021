{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Arrow
import Control.Monad
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (partition, foldl')
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

data Row = Row
  { input :: Set (Set Char)
  , output :: [Set Char]
  }

readInput1 :: String -> [Row]
readInput1 = lines >>> map \xs -> case break (== "|") $ words xs of
  (inputs, _:outputs) -> Row (S.fromList $ S.fromList <$> inputs) (S.fromList <$> outputs)

readInput2 :: String -> [Row]
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

solve1 :: [Row] -> Int
solve1 = sum . map (length . filter (\o -> S.size o `elem` [2, 4, 3, 7]) . output)

solve2 :: [Row] -> Int
solve2 rs = sum [fromDec $ (decode (input r) M.!) <$> output r | r <- rs]
  where
    fromDec = foldl' (\n d -> n * 10 + d) 0
    group f s
      | Just (m, s') <- S.minView s
      , k <- f m
      , (ms, s'') <- S.partition ((== k) . f) s'
      = M.insert k (m : S.toList ms) $ group f s''
      | otherwise = M.empty
    decode input
      | [(2, [one]), (3, [seven]), (4, [four]), (5, n235), (6, n069), (7, [eight])] <- M.toList $ group S.size input
      , ([three], n25) <- partition (seven `S.isSubsetOf`) n235
      , ([nine], n06) <- partition (four `S.isSubsetOf`) n069
      , ([zero], [six]) <- partition (one `S.isSubsetOf`) n06
      , ([five], [two]) <- partition (`S.isSubsetOf` six) n25
      = M.fromList [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
