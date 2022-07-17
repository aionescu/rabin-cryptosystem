module Rabin.KeyGen(Key(..), genKey) where

import Data.Bits((.|.), bit, shiftL, shiftR, testBit)
import System.Random.MWC(create, createSystemRandom, GenIO)
import System.Random.Stateful(uniformRM)

import Rabin.Utils(powMod)

exp2 :: Integer -> (# Integer, Int #)
exp2 n = go (n - 1) 0
  where
    go r p
      | testBit r 0 = (# r, p #)
      | otherwise = go (r `shiftR` 1) (p + 1)

trialComposite :: Integer -> Int -> Integer -> Integer -> Bool
trialComposite d s a n
  | powMod a d n == 1 = False
  | otherwise = go 0
      where
        go i
          | i == s = True
          | powMod a (bit i * d) n == n - 1 = False
          | otherwise = go (i + 1)

millerRabin :: Int -> Integer -> GenIO -> IO Bool
millerRabin k n g
  | n == 1 = pure False
  | n == 2 = pure True
  | even n = pure False
  | n < 9 = pure True
  | otherwise = go k
    where
      (# d, s #) = exp2 n
      go 0 = pure True
      go k = uniformRM (2, n - 1) g >>= \a ->
        if trialComposite d s a n
        then pure False
        else go (k - 1)

randomBits :: Int -> GenIO -> IO Integer
randomBits n = uniformRM (bit (n - 1), bit n - 1)

random3 :: Int -> GenIO -> IO Integer
random3 bits g = (.|. 3) . (`shiftL` 2) <$> randomBits (bits - 2) g

randomPrime :: Int -> Int -> Integer -> GenIO -> IO Integer
randomPrime bits tests prev g = do
  p <- random3 bits g
  if p == prev then
    randomPrime bits tests prev g
  else
    millerRabin tests p g >>= \case
      True -> pure p
      False -> randomPrime bits tests prev g

data Key = Key !Integer !Integer

genKey :: Bool -> Int -> Int -> IO Key
genKey fixedSeed tests bits = do
  g <-
    if fixedSeed
    then create
    else createSystemRandom

  p <- randomPrime bits tests 0 g
  q <- randomPrime bits tests p g
  pure $! Key p q
