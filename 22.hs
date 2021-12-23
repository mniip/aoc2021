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
import qualified Data.Array.Unboxed as UA

main :: IO ()
main = aocMain readInput1 solve1 printOutput1 readInput2 solve2 print

fromFile :: String -> _
fromFile filename = readInput2 $ unsafePerformIO $ readFile filename

readInput1 :: _ -> [(Bool, (Int, Int), (Int, Int), (Int, Int))]
readInput1 = map (readP ln) . lines
  where
    ln = do
      s <- (True <$ P.string "on") <|> (False <$ P.string "off")
      P.string " x="
      x1 <- P.readS_to_P reads
      P.string ".."
      x2 <- P.readS_to_P reads
      P.string ",y="
      y1 <- P.readS_to_P reads
      P.string ".."
      y2 <- P.readS_to_P reads
      P.string ",z="
      z1 <- P.readS_to_P reads
      P.string ".."
      z2 <- P.readS_to_P reads
      pure (s, (x1, x2), (y1, y2), (z1, z2))

readInput2 :: _ -> _
readInput2 = readInput1

printOutput1 :: _ -> IO ()
printOutput1 = print @Int

printOutput2 :: _ -> IO ()
printOutput2 = print @Int

solve1 :: _ -> _
solve1 = S.size . foldl' enable S.empty
  where
    enable s (st, (x1, x2), (y1, y2), (z1, z2)) = (if st then S.union else S.difference) s $ S.fromList [(x, y, z) | x <- [max (-50) x1..min 50 x2], y <- [max (-50) y1..min 50 y2],  z <- [max (-50) z1..min 50 z2]]

solve2' :: [_] -> _
solve2' = total . foldl' enable (S.empty, S.empty, S.empty, UA.listArray ((0, 0, 0), (0, 0, 0)) [False] :: UA.UArray _ _)
  where
    total (!xs, !ys, !zs, !arr)
      = sum [(S.elemAt x xs - S.elemAt (x-1) xs) * (S.elemAt y ys - S.elemAt (y-1) ys) * (S.elemAt z zs - S.elemAt (z-1) zs) | ((x, y, z), st) <- UA.assocs arr, st]
    enable (!xs, !ys, !zs, !arr) (st, (x1, x2), (y1, y2), (z1, z2))
      | (xs', arr1) <- insX x1 (xs, arr)
      , (xs'', arr2) <- insX x2 (xs', arr1)
      , (ys', arr3) <- insY y1 (ys, arr2)
      , (ys'', arr4) <- insY y2 (ys', arr3)
      , (zs', arr5) <- insZ z1 (zs, arr4)
      , (zs'', arr6) <- insZ z2 (zs', arr5)
      = (xs'', ys'', zs'', arr6 UA.// [((x, y, z), st) | x <- [S.findIndex x1 xs''+1..S.findIndex x2 xs''], y <- [S.findIndex y1 ys''+1..S.findIndex y2 ys''], z <- [S.findIndex z1 zs''+1..S.findIndex z2 zs'']])

    insX nx (xs, arr) = if S.member nx xs then (xs, arr)
      else let xs' = S.insert nx xs; i = S.findIndex nx xs' in (xs', stretchX i arr)
    insY ny (ys, arr) = if S.member ny ys then (ys, arr)
      else let ys' = S.insert ny ys; i = S.findIndex ny ys' in (ys', stretchY i arr)
    insZ nz (zs, arr) = if S.member nz zs then (zs, arr)
      else let zs' = S.insert nz zs; i = S.findIndex nz zs' in (zs', stretchZ i arr)
    stretchX i arr = UA.listArray ((x1, y1, z1), (x2+1, y2, z2)) [arr UA.! (if x <= i then x else x - 1, y, z) | x <- [x1..x2+1], y <- [y1..y2], z <- [z1..z2]]
      where ((x1, y1, z1), (x2, y2, z2)) = UA.bounds arr
    stretchY i arr = UA.listArray ((x1, y1, z1), (x2, y2+1, z2)) [arr UA.! (x, if y <= i then y else y - 1, z) | x <- [x1..x2], y <- [y1..y2+1], z <- [z1..z2]]
      where ((x1, y1, z1), (x2, y2, z2)) = UA.bounds arr
    stretchZ i arr = UA.listArray ((x1, y1, z1), (x2, y2, z2+1)) [arr UA.! (x, y, if z <= i then z else z - 1) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2+1]]
      where ((x1, y1, z1), (x2, y2, z2)) = UA.bounds arr

solve2 :: _ -> Integer
solve2 inp = total $ foldl' enable (UA.accumArray (const id) False ((0, 0, 0), (S.size xs, S.size ys, S.size zs)) [] :: UA.UArray _ _) inp
  where
    xs = S.fromList [x | (_, (x1, x2), _, _) <- inp, x <- [x1, x2+1]]
    ys = S.fromList [y | (_, _, (y1, y2), _) <- inp, y <- [y1, y2+1]]
    zs = S.fromList [z | (_, _, _, (z1, z2)) <- inp, z <- [z1, z2+1]]
    xsI = M.fromList [(S.findIndex x xs, x) | x <- S.toList xs]
    ysI = M.fromList [(S.findIndex y ys, y) | y <- S.toList ys]
    zsI = M.fromList [(S.findIndex z zs, z) | z <- S.toList zs]
    total !arr
      = sum [fromIntegral (xsI M.! x - xsI M.! (x - 1)) * fromIntegral (ysI M.! y - ysI M.! (y - 1)) * fromIntegral (zsI M.! z - zsI M.! (z - 1)) | ((x, y, z), st) <- UA.assocs arr, st]
    enable !arr (st, (x1, x2), (y1, y2), (z1, z2))
      = arr UA.// [((x, y, z), st) | x <- [S.findIndex x1 xs+1..S.findIndex (x2+1) xs], y <- [S.findIndex y1 ys+1..S.findIndex (y2+1) ys], z <- [S.findIndex z1 zs+1..S.findIndex (z2+1) zs]]
