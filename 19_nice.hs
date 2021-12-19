{-# LANGUAGE BangPatterns #-}

module Main where

import AOC.Common (aocMain, readP, dijkstra')
import Control.Applicative
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = aocMain readInput solve1 print readInput solve2 print

readInput :: String -> [[R3]]
readInput = readP (P.sepBy scanner $ P.char '\n')
  where
    scanner = do
      P.string "---"
      P.munch (/= '\n')
      many $ do
        x <- P.readS_to_P reads
        P.char ','
        y <- P.readS_to_P reads
        P.char ','
        z <- P.readS_to_P reads
        P.char '\n'
        pure $ R3 x y z

data R3 = R3 !Int !Int !Int deriving (Eq, Ord, Show)

instance Semigroup R3 where
  R3 x1 y1 z1 <> R3 x2 y2 z2 = R3 (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid R3 where
  mempty = R3 0 0 0

invR3 :: R3 -> R3
invR3 (R3 x y z) = R3 (-x) (-y) (-z)


data Rotation = Rotation { applyRotation :: R3 -> R3 }

instance Semigroup Rotation where
  Rotation f <> Rotation g = Rotation (f . g)

instance Monoid Rotation where
  mempty = Rotation id

invRotation :: Rotation -> Rotation
invRotation (Rotation f) = Rotation $ \v -> R3 (dotR3 dx v) (dotR3 dy v) (dotR3 dz v)
  where -- orthogonal, so transpose is inverse
    !dx = f (R3 1 0 0)
    !dy = f (R3 0 1 0)
    !dz = f (R3 0 0 1)
    dotR3 (R3 x1 y1 z1) (R3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

rotations :: [Rotation]
rotations = Rotation -- even mirrors * even permutations + odd mirrors * odd permutations
  <$> liftA2 (.) [id, invX . invY, invX . invZ, invY . invZ] [id, cycle, cycle . cycle]
  ++ liftA2 (.) [invX, invY, invZ, invX . invY . invZ] [swap, swap . cycle, swap . cycle . cycle]
  where
    invX = \(R3 x y z) -> R3 (-x) y z
    invY = \(R3 x y z) -> R3 x (-y) z
    invZ = \(R3 x y z) -> R3 x y (-z)
    cycle = \(R3 x y z) -> R3 y z x
    swap = \(R3 x y z) -> R3 y x z

instance Eq Rotation where
  Rotation f == Rotation g = f (R3 0 1 2) == g (R3 0 1 2) -- this element has trivial stabilizer

instance Ord Rotation where
  Rotation f `compare` Rotation g = f (R3 0 1 2) `compare` g (R3 0 1 2)

sortR3 :: R3 -> R3
sortR3 v = maximum $ (`applyRotation` v) <$> rotations


data Affine = Affine !Rotation !R3 deriving (Eq, Ord)

instance Semigroup Affine where
  Affine r1 d1 <> Affine r2 d2 = Affine (r1 <> r2) (d1 <> applyRotation r1 d2)

instance Monoid Affine where
  mempty = Affine mempty mempty

applyAffine :: Affine -> R3 -> R3
applyAffine (Affine r d) p = d <> applyRotation r p

invAffine :: Affine -> Affine
invAffine (Affine r d) = Affine (invRotation r) $ invR3 $ applyRotation (invRotation r) d


data Sensor = Sensor
  { beacons :: Set R3
  , deltas :: Map R3 (Set Affine)
  } deriving (Eq, Ord)

mkSensor :: [R3] -> Sensor
mkSensor xs = Sensor beacons $ M.fromListWith S.union
  [ (d, S.fromList [Affine r p | r <- rotations, applyRotation r d == q <> invR3 p])
  | p <- S.toList beacons, q <- S.toList beacons, p /= q, let d = sortR3 $ q <> invR3 p]
  where beacons = S.fromList xs

tryMatch :: Sensor -> Sensor -> Maybe Affine
tryMatch s1 s2 = find (\a -> S.size (S.intersection (beacons s1) (S.map (applyAffine a) $ beacons s2)) >= 12)
  $ S.toList $ S.unions $ M.elems common
  where
    common = M.intersectionWith transitions (deltas s1) (deltas s2)
    transitions as1 as2 = S.unions [S.map (<> invAffine a2) as1 | a2 <- S.toList as2]

chart :: Map Int Sensor -> Map Int Affine
chart ss
  | Just (init, _) <- M.lookupMin ss
  = dijkstra' init $ \i -> [(j, a) | (j, s) <- M.toList ss, a <- maybeToList $ tryMatch (ss M.! i) s]

solve1 :: [[R3]] -> Int
solve1 xss = S.size $ S.unions [S.map (applyAffine a) $ beacons $ sensors M.! i | (i, a) <- M.toList ch]
  where
    sensors = M.fromList $ zip [0..] $ mkSensor <$> xss
    ch = chart sensors

solve2 :: [[R3]] -> Int
solve2 xss = maximum [manhattan $ d1 <> invR3 d2 | Affine _ d1 <- M.elems ch, Affine _ d2 <- M.elems ch]
  where
    sensors = M.fromList $ zip [0..] $ mkSensor <$> xss
    ch = chart sensors
    manhattan (R3 x y z) = abs x + abs y + abs z
