{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import AOC.Common (aocMain)
import Control.Monad
import Data.Bits
import Data.Char
import Data.List (foldl')
import Text.Parsec

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

data OpType = Sum | Prod | Min | Max | Lit | Gt | Lt | Eq
  deriving (Eq, Ord, Show, Enum, Bounded)

type Packet = (Int, Payload)

data Payload
  = Literal Integer
  | Operator OpType [Packet]
  deriving (Eq, Ord, Show)

oneBit :: Parsec [Bool] () Bool
oneBit = tokenPrim show (\p _ _ -> incSourceColumn p 1) Just

nBits :: Int -> Parsec [Bool] () Int
nBits n = foldl' (\n d -> n * 2 + fromEnum d) 0 <$> replicateM n oneBit

packetType :: Parsec [Bool] () OpType
packetType = do
  opType <- nBits 3
  guard (opType >= fromEnum (minBound :: OpType) && opType <= fromEnum (maxBound :: OpType))
  pure (toEnum opType)

packet :: Parsec [Bool] () Packet
packet = do
  version <- nBits 3
  (version,) <$> do
    packetType >>= \case
      Lit -> Literal <$> literal 0
      opType -> Operator opType <$> do
        oneBit >>= \case
          False -> do
            length <- nBits 15
            pos1 <- sourceColumn <$> getPosition
            manyTill packet $ do
              pos2 <- sourceColumn <$> getPosition
              guard (pos2 == pos1 + length)
          True -> do
            packets <- nBits 11
            replicateM packets packet

literal :: Integer -> Parsec [Bool] () Integer
literal value = do
  keepGoing <- oneBit
  bits <- nBits 4
  let value' = shiftL value 4 .|. fromIntegral bits
  if keepGoing then literal value' else pure value'

readInput :: String -> Packet
readInput xs = case runParser packet () "" $ hexDigit =<< filter (not . isSpace) xs of
  Left err -> error $ show err
  Right p -> p
  where hexDigit d = testBit (digitToInt d) <$> [3,2,1,0]

solve1 :: Packet -> Int
solve1 = goPacket
  where
    goPacket (ver, p) = ver + goPayload p
    goPayload (Literal _) = 0
    goPayload (Operator _ ps) = sum $ goPacket <$> ps

solve2 :: Packet -> Integer
solve2 = go
  where
    go (_, Literal n) = n
    go (_, Operator Sum ps) = sum $ go <$> ps
    go (_, Operator Prod ps) = product $ go <$> ps
    go (_, Operator Min ps) = minimum $ go <$> ps
    go (_, Operator Max ps) = maximum $ go <$> ps
    go (_, Operator Gt (p1:p2:_)) = if go p1 > go p2 then 1 else 0
    go (_, Operator Lt (p1:p2:_)) = if go p1 < go p2 then 1 else 0
    go (_, Operator Eq (p1:p2:_)) = if go p1 == go p2 then 1 else 0
