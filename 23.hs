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

import AOC.Imports hiding (Empty, bottom, top)
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
import GHC.Generics
import Control.DeepSeq

data A = A | B | C | D deriving (Eq, Ord, Show, Read, Generic, NFData, Enum)
makePrisms ''A

data Room a = Empty | Initial a | Final a deriving (Eq, Ord, Show, Generic, NFData)

data St = St [[A]] (Map Int A) deriving (Eq, Ord, Show)

data Move
  = Stash Int Int
  | Transfer Int Int
  | Unstash Int Int

stashes :: [Int]
stashes = [0, 1, 3, 5, 7, 9, 10]

moves :: [Move]
moves = [Stash i s | i <- [0..3], s <- stashes]
  <> [Unstash s i | i <- [0..3], s <- stashes]
  <> [Transfer i j | i <- [0..3], j <- delete i [0..3]]

amphCost :: A -> Int
amphCost A = 1
amphCost B = 10
amphCost C = 100
amphCost D = 1000

amph :: Int -> A
amph = toEnum

urng a b = if a < b then [a..b] else [b..a]

tryMove :: Int -> St -> Move -> Maybe (Int, St)
tryMove depth (St rm st) (Stash i s)
  | all (`M.notMember` st) (urng (i * 2 + 2) s)
  , any (/= amph i) $ rm !! i
  , a:as <- rm !! i
  = Just (amphCost a * (abs (i * 2 + 2 - s) + (depth - length as)), St (rm & ix i .~ as) (M.insert s a st))
tryMove depth (St rm st) (Unstash s i)
  | Just a <- M.lookup s st
  , a == amph i
  , all (`M.notMember` st) (delete s $ urng (i * 2 + 2) s)
  , all (== amph i) $ rm !! i
  , length (rm !! i) < depth
  = Just (amphCost a * (abs (i * 2 + 2 - s) + (depth - length (rm !! i))), St (rm & ix i %~ (a:)) (M.delete s st))
tryMove depth (St rm st) (Transfer i j)
  | a:as <- rm !! i
  , a == amph j
  , all (== amph j) $ rm !! j
  , length (rm !! j) < depth
  , all (`M.notMember` st) (urng (i * 2 + 2) (j * 2 + 2))
  = Just (amphCost a * (abs (i * 2 + 2 - (j * 2 + 2)) + (depth - length (rm !! j)) + (depth - length as)), St (rm & ix j %~ (a:) & ix i .~ as) st)
tryMove _ _ _ = Nothing

tryMoves d st [] = pure (0, st)
tryMoves d st (m:ms) = do
  (c, st') <- tryMove d st m
  (c', st'') <- tryMoves d st' ms
  pure (c + c', st'')

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: _ -> _
readInput xs = let (ms, bs) = splitAt 4 $ map (read . pure) $ filter isLetter xs in St (transpose [ms, bs]) M.empty

solve1 :: _ -> Int
solve1 st = M.getSum $ fromJust $ dijkstraTo' st finalSt (transition 2)
  where
    finalSt = St (replicate 2 <$> [A, B, C, D]) M.empty

solve2 :: _ -> Int
solve2 st = M.getSum $ fromJust $ dijkstraTo' (enhance st) finalSt (transition 4)
  where
    enhance (St rm st) = St (zipWith (\r i -> take 1 r ++ i ++ drop 1 r) rm [[D, D], [C, B], [B, A], [A, C]]) st
    finalSt = St (replicate 4 <$> [A, B, C, D]) M.empty

transition d st = [(st', M.Sum cost) | mv <- moves, (cost, st') <- maybeToList $ tryMove d st mv]
