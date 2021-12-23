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

main :: IO ()
main = aocMain readInput1 solve1 printOutput1 readInput2 solve2 print

fromFile :: String -> _
fromFile filename = readInput2 $ unsafePerformIO $ readFile filename

readInput1 :: _ -> (Int, Int)
readInput1 = (\[x, y] -> (x, y)) . map (read . last . words) . lines

readInput2 :: _ -> _
readInput2 = readInput1

printOutput1 :: _ -> IO ()
printOutput1 = print @Int

printOutput2 :: _ -> IO ()
printOutput2 = print @Int

roll :: State (Int, Int) Int
roll = state $ \(k, i) -> (i, (k + 1, (i `mod` 100) + 1))

solve1 :: _ -> _
solve1 (p, q) = (`evalState` (0, 1)) $ go p 0 q 0
  where
    go p x q y = do
      d <- sum <$> replicateM 3 roll
      let p' = ((p + d - 1) `mod` 10) + 1
      let x' = x + p'
      if x' >= 1000
      then do
        (k, _) <- get
        pure $ y * k
      else go q y p' x'

bind :: Ord b => [(a, Integer)] -> (a -> [(b, Integer)]) -> [(b, Integer)]
bind xs fs = M.toList $ M.fromListWith (+) [(y, p * q) | (x, p) <- xs, (y, q) <- fs x]

solve2 (p, q) = uncurry max $ go $ accumArray (const id) 0 ((1, 0, 1, 0), (10, 30, 10, 30)) [((q, 0, p, 0), 1)]
  where
    go arr
      | all (== 0) $ elems arr
      = (0, 0)
      | otherwise =
        let arr' = array (bounds arr) [ ((p, x, q, y), sum [u * ix (q, y, ((p - d - 1) `mod` 10) + 1, x - p) | (d, u) <- [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]]) | (p, x, q, y) <- range $ bounds arr ]
            w' = sum [arr ! (p, x, q, y) | p <- [1..10], x <- [21..30], q <- [1..10], y <- [0..20]]
        in first (+w') . swap $ go arr'
      where ix (p, x, q, y) = if inRange (bounds arr) (p, x, q, y) && x <= 20 && y <= 20 then arr ! (p, x, q, y) else 0
