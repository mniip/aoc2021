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
main = aocMain readInput1 solve1 printOutput1 readInput2 solve2 printOutput2

fromFile :: String -> _
fromFile filename = readInput2 $ unsafePerformIO $ readFile filename

readInput1 :: _ -> [[(Int, Int, Int)]]
readInput1 = map (scanner . lines) . splitOn "\n\n"
  where scanner (_:xs) = map ((\[x, y, z] -> (read x,read y, read z)) . splitOn ",") xs

readInput2 :: _ -> _
readInput2 = readInput1

printOutput1 :: _ -> IO ()
printOutput1 = print @Int

printOutput2 :: _ -> IO ()
printOutput2 = print @Int

data Rot = Rot { applyRot :: (Int, Int, Int) -> (Int, Int, Int) }

instance Eq Rot where
  Rot f == Rot g = all (liftA2 (==) f g) [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

instance Show Rot where
  show (Rot f) = show (f (1, 0, 0), f (0, 1, 0), f (0, 0, 1))

rots :: [Rot]
rots = nub [Rot $ p.q.r.s.t.u | p <- ap, q <- bp, r <- ap, s <- bp, t <- ap, u <- bp]
  where
    a (x, y, z) = (y, z, x)
    b (x, y, z) = (-y, x, z)
    ap = [id, a, a . a]
    bp = [id, b, b . b, b . b . b]

instance Num a => Num (a, a, a) where
  (a, b, c) + (d, e, f) = (a + d, b + e, c + f)
  (a, b, c) - (d, e, f) = (a - d, b - e, c - f)

tmatch :: Set (Int, Int, Int) -> [(Int, Int, Int)] -> Maybe (Rot, (Int, Int, Int))
tmatch xs ys = listToMaybe $ do
  r <- rots
  x <- S.toList xs
  let rys = applyRot r <$> ys
  y <- rys
  guard (not $ null $ drop 11 [() | y' <- rys, (y' - y + x) `S.member` xs])
  pure (r, y - x)


solve1 :: _ -> _
solve1 (xs:xss) = S.size $ go (S.fromList xs) xss
  where
    go m [] = m
    go m xss = case [(i, r, d) | (i, xs) <- zip [0..] xss, (r, d) <- maybeToList $ tmatch m xs] of
      [] -> error "urk"
      (i, r, d):_ -> go (S.union m (S.fromList $ (subtract d . applyRot r) <$> (xss !! i))) $ case splitAt i xss of (ps, _:qs) -> ps ++ qs

solve2 :: _ -> _
solve2 (xs:xss) = go [(0, 0, 0)] (S.fromList xs) xss
  where
    go ps m [] = maximum [abs (a - d) + abs (b - e) + abs (c - f) | (a, b, c) <- ps, (d, e, f) <- ps]
    go ps m xss = case [(i, r, d) | (i, xs) <- zip [0..] xss, (r, d) <- maybeToList $ tmatch m xs] of
      [] -> error "urk"
      (i, r, d):_ -> go (d:ps) (S.union m (S.fromList $ (subtract d . applyRot r) <$> (xss !! i))) $ case splitAt i xss of (ps, _:qs) -> ps ++ qs
