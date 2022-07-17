module Rabin.KeyGen(genKey) where

import Control.Monad.Extra(orM)
import Data.Bits((.|.), bit, shiftL, shiftR, testBit)
import System.Random.MWC(create, createSystemRandom)
import System.Random.Stateful(StatefulGen, uniformRM)

import Rabin.Utils(powMod)

exp2 :: Integer -> (Integer, Integer)
exp2 n = go (n - 1) 0
  where
    go r p
      | testBit r 0 = (r, p)
      | otherwise = go (r `shiftR` 1) (p + 1)

millerRabin :: StatefulGen g m => Int -> Integer -> g -> m Bool
millerRabin k n g
  | n == 1 = pure False
  | n == 2 = pure True
  | even n = pure False
  | n < 9 = pure True
  | otherwise = not <$> orM (replicate k testRound)
  where
    (d, s) = exp2 n
    trialComposite a = powMod a d n /= 1 && not (any (\i -> powMod a (2^i * d) n == n - 1) [0 .. s - 1])
    testRound = trialComposite <$> uniformRM (2, n - 1) g

randomBits :: StatefulGen g m => Int -> g -> m Integer
randomBits n = uniformRM (bit (n - 1), bit n - 1)

random3 :: StatefulGen g m => Int -> g -> m Integer
random3 bits g = (.|. 3) . (`shiftL` 2) <$> randomBits (bits - 2) g

randomPrime :: StatefulGen g m => Int -> Int -> Integer -> g -> m Integer
randomPrime bits tests prev g = do
  p <- random3 bits g
  if p == prev then
    randomPrime bits tests prev g
  else
    millerRabin tests p g >>= \case
      True -> pure p
      False -> randomPrime bits tests prev g

genKeyPair :: StatefulGen g m => Int -> Int -> g -> m (Integer, Integer)
genKeyPair bits tests g = do
  p <- randomPrime bits tests 0 g
  q <- randomPrime bits tests p g
  pure (p, q)

genKey :: Bool -> Int -> IO (Integer, Integer)
genKey fixedSeed bits = do
  genKeyPair bits 64 =<<
    if fixedSeed
    then create
    else createSystemRandom
