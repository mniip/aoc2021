{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module AOC.Common
  ( aocMain
  , equals
  , count
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment
import System.IO

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
count :: (a -> Bool) -> [a] -> Int
count p = foldl' (\n x -> if p x then n + 1 else n) 0
