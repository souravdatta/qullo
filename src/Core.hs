{-# LANGUAGE RankNTypes #-}

module Core (q0, q1, x, h, i, gate, counts, defaultCounts, (<=>), tensorProd, QMat) where

import Control.Monad.Random (getRandomR)
import Control.Monad.Random.Class (MonadRandom)
import Data.Complex (Complex, magnitude)
import Data.Map (Map, empty, insert, lookup, member)
import Data.Matrix
  ( Matrix,
    flatten,
    fromList,
    identity,
    multStd,
    toList,
  )
import Prelude hiding (lookup)

type QType = Complex Double

type QMat = Matrix QType

q0 :: QMat
q0 = fromList 2 1 [1, 0]

q1 :: QMat
q1 = fromList 2 1 [0, 1]

i :: QMat
i = identity 2

x :: QMat
x = fromList 2 2 [0, 1, 1, 0]

h :: QMat
h = fromList 2 2 [h_factor, h_factor, h_factor, -h_factor]
  where
    h_factor = 1 / sqrt 2

gate :: QMat -> QMat -> QMat
gate = multStd

mapWithIndex :: forall a b. (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

square :: (Num a) => a -> a
square v = v * v

genList :: [Complex Double] -> [Int]
genList xs =
  concat $
    mapWithIndex (\ind v -> replicate (round v) ind) $
      map ((*) 100 . square . magnitude) xs

newBits :: [[Int]] -> [[Int]]
newBits = concatMap (\l -> [l ++ [0], l ++ [1]])

bits :: Int -> [[Int]]
bits n = if n <= 1 then [[0], [1]] else newBits $ bits (n - 1)

castDie :: (MonadRandom m) => Int -> m Int
castDie len = getRandomR (0, len - 1)

observations :: (MonadRandom m) => Int -> [[Int]] -> [Int] -> m [[Int]]
observations 0 _ _ = do
  return []
observations n labels obs = do
  indx <- castDie (length obs)
  rest <- observations (n - 1) labels obs
  return ((labels !! (obs !! indx)) : rest)

countGroups :: [[Int]] -> Map [Int] Int
countGroups =
  foldl
    ( \count_map obs ->
        if member obs count_map
          then
            let current_count = lookup obs count_map
                new_count = case current_count of
                  Just n -> n + 1
                  Nothing -> 1
             in insert obs new_count count_map
          else insert obs 1 count_map
    )
    empty

bitlenOf :: [Complex Double] -> Int
bitlenOf lst =
  let len = length lst
      lg = logBase (2 :: Double) $ fromIntegral len
   in round lg

counts :: (MonadRandom m) => Int -> QMat -> m (Map [Int] Int)
counts shots mat = do
  let mat_list = toList mat
  let r = genList mat_list
  let bitlen = bitlenOf mat_list
  obs <- observations shots (bits bitlen) r
  return (countGroups obs)

defaultCounts :: (MonadRandom m) => QMat -> m (Map [Int] Int)
defaultCounts = counts 1024

tensorProd :: QMat -> QMat -> QMat
tensorProd m1 m2 = flatten $ fmap (\p -> fmap (p *) m2) m1

(<=>) :: QMat -> QMat -> QMat
(<=>) = tensorProd
