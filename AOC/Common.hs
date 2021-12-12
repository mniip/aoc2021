{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module AOC.Common
  ( aocMain
  , equals
  , count
  , nubOn
  , deleteOn
  , deleteFirstsOn
  , unionOn
  , intersectOn
  , groupOn
  , groupOn'
  , sortOn
  , insertOn
  , maximumOn
  , minimumOn
  , splitOn2
  , readP
  , median
  , arithSum
  , buildGraph
  , buildGraphThin
  , undirected
  , directions4
  , directions8
  , neighbors4
  , neighbors8
  ) where

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Array
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function
import Data.Graph
import Data.List
  ( foldl', stripPrefix, sort
  , nubBy, deleteBy, deleteFirstsBy, unionBy, intersectBy, groupBy
  , sortBy, insertBy, maximumBy, minimumBy
  )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord
import Data.Tuple
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment
import System.IO
import qualified Text.ParserCombinators.ReadP as P

class AOCInput i where
  getInput :: IO i

instance AOCInput String where
  getInput = getContents

instance AOCInput BS.ByteString where
  getInput = BS.getContents

instance AOCInput BSL.ByteString where
  getInput = BSL.getContents

instance AOCInput T.Text where
  getInput = T.getContents

instance AOCInput TL.Text where
  getInput = TL.getContents

aocMain
  :: AOCInput i
  => (i -> p1) -> (p1 -> o1) -> (o1 -> IO r1)
  -> (i -> p2) -> (p2 -> o2) -> (o2 -> IO r2)
  -> IO ()
aocMain parse1 solve1 print1 parse2 solve2 print2 = do
  args <- getArgs
  input <- getInput
  when ("1" `elem` args || null args) $ do
    try @SomeException (evaluate $ parse1 input) >>= \case
      Left err -> hPutStr stderr $ "Parsing input for 1: " <> show err
      Right parsed -> try @SomeException (evaluate $ solve1 parsed) >>= \case
        Left err -> hPutStr stderr $ "Solving 1: " <> show err
        Right output -> try @SomeException (print1 output) >>= \case
          Left err -> hPutStr stderr $ "Printing output for 1: " <> show err
          Right _ -> pure ()
  when ("2" `elem` args || null args) $ do
    try @SomeException (evaluate $ parse2 input) >>= \case
      Left err -> hPutStr stderr $ "Parsing input for 2: " <> show err
      Right parsed -> try @SomeException (evaluate $ solve2 parsed) >>= \case
        Left err -> hPutStr stderr $ "Solving 2: " <> show err
        Right output -> try @SomeException (print2 output) >>= \case
          Left err -> hPutStr stderr $ "Printing output for 2: " <> show err
          Right _ -> pure ()

{-# INLINE equals #-}
equals :: Eq a => a -> a -> Bool
equals = (==)

{-# INLINE count #-}
count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldl' (\n x -> if p x then n + 1 else n) 0

{-# INLINE nubOn #-}
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy (equals `on` f)

{-# INLINE deleteOn #-}
deleteOn :: Eq b => (a -> b) -> a -> [a] -> [a]
deleteOn f = deleteBy (equals `on` f)

{-# INLINE deleteFirstsOn #-}
deleteFirstsOn :: Eq b => (a -> b) -> [a] -> [a] -> [a]
deleteFirstsOn f = deleteFirstsBy (equals `on` f)

{-# INLINE unionOn #-}
unionOn :: Eq b => (a -> b) -> [a] -> [a] -> [a]
unionOn f = unionBy (equals `on` f)

{-# INLINE intersectOn #-}
intersectOn :: Eq b => (a -> b) -> [a] -> [a] -> [a]
intersectOn f = intersectBy (equals `on` f)

{-# INLINE groupOn #-}
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (equals `on` f)

{-# INLINE groupOn' #-}
groupOn' :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn' f = map (f . head &&& id) . groupBy (equals `on` f)

{-# INLINE sortOn #-}
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)

{-# INLINE insertOn #-}
insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn f = insertBy (comparing f)

{-# INLINE maximumOn #-}
maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (comparing f)

{-# INLINE minimumOn #-}
minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (comparing f)

{-# INLINE splitOn2 #-}
splitOn2 :: Eq a => [a] -> [a] -> ([a], [a])
splitOn2 needle hay
  | Just xs <- stripPrefix needle hay = ([], xs)
  | h:hs <- hay = first (h:) $ splitOn2 needle hs
  | otherwise = error "splitOn2: separator not found"

readP :: P.ReadP a -> String -> a
readP p xs = case [x | (x, "") <- P.readP_to_S p xs] of
  [x] -> x
  [] -> error "readP: no parse"
  _ -> error "readP: ambiguous parse"

median :: Ord a => [a] -> a
median [] = error "median: empty list"
median xs = sort xs !! (length xs `div` 2)

{-# INLINE arithSum #-}
arithSum :: Integral a => a -> a -> a
arithSum a b = (b * (b + 1) - a * (a - 1)) `div` 2

mkGraph :: Ord k => [(k, [k])] -> (Graph, Vertex -> k, k -> Vertex)
mkGraph adjs = (graph, out, into)
  where
    (graph, out', into') = graphFromEdges [((), k, ks) | (k, ks) <- adjs]
    !labels = listArray (bounds graph) [k | i <- range (bounds graph), let (_, k, _) = out' i]
    out i = labels ! i
    into k = case into' k of
      Nothing -> error "mkGraph.into: no such vertex"
      Just i -> i

buildGraph :: Ord k => [k] -> [(k, k)] -> (Graph, Vertex -> k, k -> Vertex)
buildGraph vs es = mkGraph $ M.toList
  $ M.fromListWith (<>) $ map (, []) vs <> map (second pure) es

buildGraphThin :: Ord k => [k] -> [(k, k)] -> (Graph, Vertex -> k, k -> Vertex)
buildGraphThin vs es = mkGraph $ map (second S.toList) $ M.toList
  $ M.fromListWith S.union $ map (, S.empty) vs <> map (second S.singleton) es

undirected :: Graph -> Graph
undirected g = buildG (bounds g) $ edges g <> map swap (edges g)

{-# INLINE directions4 #-}
directions4 :: Num a => [(a, a)]
directions4 = [(0, 1), (1, 0), (0, -1), (-1, 0)]

{-# INLINE directions8 #-}
directions8 :: Num a => [(a, a)]
directions8 = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

{-# INLINE neighbors4 #-}
neighbors4 :: Num a => (a, a) -> [(a, a)]
neighbors4 (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions4]

{-# INLINE neighbors8 #-}
neighbors8 :: Num a => (a, a) -> [(a, a)]
neighbors8 (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions8]
