{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.List (foldl')
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

readInput1 :: String -> [(Dir, Int)]
readInput1 = map ((\[a, b] -> (readDir a, read b)) . words) . lines
  where
    readDir "down" = Down
    readDir "up" = Up
    readDir "forward" = Forward

readInput2 :: String -> [(Dir, Int)]
readInput2 = readInput1

printOutput :: Int -> IO ()
printOutput = print

data Dir = Down | Up | Forward

data Pos1 = Pos1
  { horizontal :: !Int
  , depth :: !Int
  } deriving (Eq, Ord, Show)

solve1 :: [(Dir, Int)] -> Int
solve1 = final . foldl' move Pos1 { horizontal = 0, depth = 0 }
  where
    final Pos1{..} = horizontal * depth
    move :: Pos1 -> (Dir, Int) -> Pos1
    move p@Pos1{..} (Down, d) = p { depth = depth + d }
    move p@Pos1{..} (Up, d) = p { depth = depth - d }
    move p@Pos1{..} (Forward, d) = p { horizontal = horizontal + d }

data Pos2 = Pos2
  { horizontal :: !Int
  , depth :: !Int
  , aim :: !Int
  } deriving (Eq, Ord, Show)

solve2 :: [(Dir, Int)] -> Int
solve2 = final . foldl' move Pos2 { horizontal = 0, depth = 0, aim = 0 }
  where
    final Pos2{..} = horizontal * depth
    move :: Pos2 -> (Dir, Int) -> Pos2
    move p@Pos2{..} (Down, d) = p { aim = aim + d }
    move p@Pos2{..} (Up, d) = p { aim = aim - d }
    move p@Pos2{..} (Forward, d) = p { horizontal = horizontal + d, depth = depth + d * aim }
