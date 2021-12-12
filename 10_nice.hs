module Main where

import AOC.Common (aocMain, median)
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Either
import qualified Data.Map as M
import Data.List (uncons)
import Data.Maybe

main :: IO ()
main = aocMain lines solve1 print lines solve2 print

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
solve2 = median . mapMaybe (fmap fst) . snd . partitionEithers . map (runMaybeT . runStateT parser)
