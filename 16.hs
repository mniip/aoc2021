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

readInput1 :: _ -> _
readInput1 xs = fromHex =<< filter (not . isSpace) xs
  where
    fromHex d | n <- digitToInt d = testBit n <$> [3,2,1,0]

readInput2 :: _ -> _
readInput2 = readInput1

printOutput1 :: _ -> IO ()
printOutput1 = print @Int

printOutput2 :: _ -> IO ()
printOutput2 = print @Integer

data Packet
  = Literal Int Integer
  | Operator Int Int [Packet]
  deriving (Eq, Ord, Show)

type Parser = StateT [Bool] []

locallyP :: ([Bool] -> ([Bool], [Bool])) -> Parser a -> Parser a
locallyP f (StateT h) = StateT $ \s -> let (s', s'') = f s in h s' >>= \(r, _) -> pure (r, s'')

bt :: Parser Bool
bt = StateT $ maybeToList . uncons

bts :: Int -> Parser Int
bts n = foldl' (\n d -> n * 2 + fromEnum d) 0 <$> replicateM n bt

packet :: Parser Packet
packet = do
  ver <- bts 3
  tp <- bts 3
  case tp of
    4 -> Literal ver <$> literal 0
    _ -> do
      ltyid <- bt
      if ltyid
      then do
        subpackets <- bts 11
        Operator ver tp <$> replicateM subpackets packet
      else do
        sublength <- bts 15
        Operator ver tp <$> locallyP (take sublength &&& drop sublength) (many packet)

runParser :: [Bool] -> Parser a -> a
runParser xs (StateT h) = case h xs of
  (r, _):_ -> r
  [] -> error "No parse"

literal :: Integer -> Parser Integer
literal x = do
  keep <- bt
  dt <- bts 4
  let x' = shiftL x 4 .|. fromIntegral dt
  if keep then literal x' else pure x'

solve1 :: _ -> _
solve1 xs = go $ runParser xs packet
  where
    go (Literal ver _) = ver
    go (Operator ver _ ps) = ver + sum (go <$> ps)

solve2 :: _ -> Integer
solve2 xs = go $ runParser xs packet
  where
    go (Literal _ n) = n
    go (Operator _ 0 ps) = sum $ go <$> ps
    go (Operator _ 1 ps) = product $ go <$> ps
    go (Operator _ 2 ps) = minimum $ go <$> ps
    go (Operator _ 3 ps) = maximum $ go <$> ps
    go (Operator _ 5 (p1:p2:_)) = if go p1 > go p2 then 1 else 0
    go (Operator _ 6 (p1:p2:_)) = if go p1 < go p2 then 1 else 0
    go (Operator _ 7 (p1:p2:_)) = if go p1 == go p2 then 1 else 0
