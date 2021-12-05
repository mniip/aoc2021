module Main where

import Control.Monad
import qualified Data.Map.Strict as M
import Numeric
import System.Environment
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

readInput1 :: String -> [((Int, Int), (Int, Int))]
readInput1 = map parse . lines
  where
    parse xs = case P.readP_to_S segment xs of
      (r, ""):_ -> r
    segment = (,) <$> point <* P.string " -> " <*> point
    point = (,) <$> P.readS_to_P readDec <* P.string "," <*> P.readS_to_P readDec

readInput2 :: String -> [((Int, Int), (Int, Int))]
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

solve1 :: [((Int, Int), (Int, Int))] -> Int
solve1 = length . filter (>= 2) . M.elems . M.unionsWith (+) . map render
  where
    render ((x1, y1), (x2, y2))
      | x1 == x2 = M.fromList [((x1, y), 1) | y <- [min y1 y2..max y1 y2]]
      | y1 == y2 = M.fromList [((x, y1), 1) | x <- [min x1 x2..max x1 x2]]
      | otherwise = M.empty

solve2 :: [((Int, Int), (Int, Int))] -> Int
solve2 = length . filter (>= 2) . M.elems . M.unionsWith (+) . map render
  where
    render ((x1, y1), (x2, y2)) = M.fromList [((x1 + p * dx, y1 + p * dy), 1) | p <- [0..d]]
      where
        d = max (abs (x1 - x2)) (abs (y1 - y2))
        dx = (x2 - x1) `div` d
        dy = (y2 - y1) `div` d
