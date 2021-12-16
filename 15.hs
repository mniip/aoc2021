{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CUSKs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE UnboxedSums #-}
--{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import AOC.Imports
import qualified AOC.Imports.BS as BS
import qualified AOC.Imports.BSC as BSC
import qualified AOC.Imports.BSL as BSL
import qualified AOC.Imports.BSLC as BSLC
import qualified AOC.Imports.Env as Env
import qualified AOC.Imports.Exception as Exception
import qualified AOC.Imports.IM as IM
import qualified AOC.Imports.IS as IS
import qualified AOC.Imports.Lens as Lens
import qualified AOC.Imports.M as M
import qualified AOC.Imports.P as P
import qualified AOC.Imports.R as R
import qualified AOC.Imports.S as S
import qualified AOC.Imports.Seq as Seq
import qualified AOC.Imports.T as T
import qualified AOC.Imports.TH as TH
import qualified AOC.Imports.TL as TL
import qualified AOC.Imports.Traced as Traced
import Data.Array.ST
import qualified Data.PQueue.Prio.Min as PQ

main :: IO ()
main = aocMain readInput1 solve1 printOutput1 readInput2 solve2 printOutput2

fromFile :: String -> _
fromFile filename = readInput2 $ unsafePerformIO $ readFile filename

readInput1 :: _ -> _
readInput1 xs = listArray ((1, 1), (length xss, length $ head xss)) $ concat xss
  where xss = map digitToInt <$> lines xs

readInput2 :: _ -> _
readInput2 = readInput1

printOutput1 :: _ -> IO ()
printOutput1 = print @Int

printOutput2 :: _ -> IO ()
printOutput2 = print @Int

solve1 :: Array (Int, Int) Int -> _
solve1 arr = go S.empty (M.singleton (fst (bounds arr)) 0)
  where
    go seen dist
      | M.keysSet dist == seen = dist M.! (snd (bounds arr))
      | otherwise = go (S.insert p seen) $ visit (first succ p) $ visit (second succ p) $ visit (first pred p) $ visit (second pred p) dist
      where
        (p, v) = minimumOn snd $ M.toList $ M.filterWithKey (\p _ -> p `S.notMember` seen) dist
        visit p' m = if inRange (bounds arr) p' then M.insertWith min p' (v + arr ! p') m else m

solve2 :: _ -> _
solve2 arr' = runST $ do
  ma <- newArray (bounds arr) maxBound
  writeArray ma (fst $ bounds arr) (0 :: Int)
  go ma (S.fromList $ range $ bounds arr) (PQ.singleton 0 (fst $ bounds arr))
  where
    arr = array (fst (bounds arr'), (5*) *** (5*) $ snd (bounds arr')) [((i + dx * w, j + dy * h), (arr' ! (i, j) + dx + dy - 1) `mod` 9 + 1) | (i, j) <- range (bounds arr'), dx <- [0..4], dy <- [0..4]]
      where
        ((x1, y1), (x2, y2)) = bounds arr'
        w = x2 - x1 + 1
        h = y2 - y1 + 1
    go :: STArray s (Int, Int) Int -> S.Set (Int, Int) -> PQ.MinPQueue Int (Int, Int) -> ST s Int
    go ma unseen queue
      | Just ((v, p), queue') <- PQ.minViewWithKey queue
      = do
        let
          visit p' queue
            | inRange (bounds arr) p'
            = (> v + arr ! p') <$> readArray ma p' >>= \case
              True -> do
                writeArray ma p' (v + arr ! p')
                pure (PQ.insert (v + arr ! p') p' queue)
              False -> pure queue
            | otherwise = pure queue
        queue'' <- visit (first succ p) queue' >>= visit (first pred p) >>= visit (second succ p) >>= visit (second pred p)
        go ma (S.delete p unseen) queue''
      | otherwise
      = readArray ma (snd (bounds arr))
