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

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Category
import Control.Comonad
import Control.Comonad.Env hiding (ask, asks, local)
import qualified Control.Comonad.Env as Env
import Control.Comonad.Identity
import Control.Comonad.Store
import Control.Comonad.Traced hiding (listen, censor, pass, trace)
import qualified Control.Comonad.Traced as Traced
import Control.Comonad.Trans.Class
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception
import qualified Control.Exception.Lens as Exception
import Control.Lens hiding (index, indices, lazy, levels, para)
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.Cont
import qualified Control.Monad.Error.Lens as Error
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Maybe hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Select
import Control.Monad.Writer hiding (lift)
import Data.Array
import Data.Array.Lens
import Data.Bifoldable
import Data.Bifunctor hiding (first, second)
import Data.Bitraversable
import Data.Bits
import Data.Bits.Lens
import Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.ByteString.Lens
import Data.Char
import Data.Coerce
import Data.Complex
import Data.Complex.Lens
import Data.Constraint hiding ((&&&), (***), (\\))
import qualified Data.Constraint as Constraint
import Data.Dynamic
import Data.Dynamic.Lens
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Foldable
import Data.Function hiding ((.), id)
import Data.Functor.Base hiding (head, tail)
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Functor.Foldable hiding (fold)
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Graph
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.IORef
import Data.Ix
import Data.Kind
import Data.List hiding (uncons)
import Data.List.Lens
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Number.CReal
import Data.Number.Interval
import Data.Number.Symbolic
import Data.Ord
import Data.Profunctor
import Data.Ratio
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Sequence.Lens
import Data.Set.Lens
import Data.STRef
import Data.Traversable
import Data.Tree
import Data.Tree.Lens
import Data.Tuple
import Data.Typeable
import Data.Typeable.Lens
import Data.Void
import Data.Word
import Debug.Trace hiding (traceEvent)
import GHC.Exts hiding (toList, traceEvent, traceEvent)
import GHC.Int
import GHC.Prim
import GHC.Word
import Language.Haskell.TH hiding (interruptible)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Numeric
import Numeric.Lens
import Numeric.Natural
import System.Directory
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Random hiding (split)
import qualified System.Random as R
import qualified Text.ParserCombinators.ReadP as P
import Text.Printf
import Text.Read hiding ((+++), (<++), get, lift)
import Unsafe.Coerce
import System.IO.Unsafe

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  when ("1" `elem` args || null args) $ do
    printOutput $ solve1 $ readInput1 contents
  when ("2" `elem` args || null args) $ do
    printOutput $ solve2 $ readInput2 contents

fromFile filename = readInput2 $ unsafePerformIO $ readFile filename

readInput1 :: String -> [([String], [String])]
readInput1 = map ((\[a, b] -> (words a, words b)) . splitOn "|") . lines

readInput2 :: String -> _
readInput2 = map (map S.fromList *** map S.fromList) . readInput1

printOutput :: _ -> IO ()
printOutput = print @Int

solve1 :: _ -> _
solve1 xs = sum $ map (\(_, o) -> length $ filter (\w -> length w `elem` [2, 4, 3, 7]) o) xs

solve2 :: _ -> _
solve2 = sum . map (\(i, o) -> foldl' (\n d -> n * 10 + d) 0 $ map (fromJust . flip elemIndex (decode i)) o)
  where
    dds = [[0,1,2,4,5,6], [2,5], [0,2,3,4,6], [0,2,3,5,6], [1,2,3,5], [0,1,3,5,6], [0,1,3,4,5,6], [0,2,5], [0,1,2,3,4,5,6], [0,1,2,3,5,6]]
    decode :: [S.Set Char] -> [S.Set Char]
    decode i = head $ do
      p <- permutations ['a'..'g']
      forM_ dds $ \ds -> do
        guard $ S.fromList ((p !!) <$> ds) `elem` i
      pure $ (S.fromList . map (p !!)) <$> dds
