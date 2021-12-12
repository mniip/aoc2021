module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Either
import qualified Data.Map as M
import Data.List (sort, uncons)
import Data.Maybe
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

parser :: StateT String (MaybeT (Either Int)) Int
parser = do
  close <- mapping closing
  r <- sum <$> many parser
  mapping (M.singleton close r)
    <|> (mapping checkerScore >>= throwError)
    <|> pure (r * 5 + completerScore M.! close)
  where
    mapping m = maybe empty pure =<< (`M.lookup` m) <$> StateT (maybe empty pure . uncons)
    closing = M.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
    checkerScore = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
    completerScore = M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

solve1 :: [String] -> Int
solve1 = sum . fst . partitionEithers . map (runMaybeT . runStateT parser)

solve2 :: [String] -> Int
solve2 = middle . sort . mapMaybe (fmap fst) . snd . partitionEithers . map (runMaybeT . runStateT parser)
  where middle xs = xs !! (length xs `div` 2)
