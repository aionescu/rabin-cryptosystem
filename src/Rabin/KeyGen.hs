module Rabin.KeyGen(genKeyIO) where

import Control.Monad.Extra(orM)
import Data.Bits(Bits, (.|.), bit, testBit)
import System.Random.MWC(withSystemRandomST)
import System.Random.Stateful(StatefulGen, uniformRM, UniformRange)

import Rabin.Utils((.<<.), (.>>.), pow)

exp2 :: (Bits a, Integral a) => a -> (a, a)
exp2 n = go (n - 1) 0
  where
    go r p
      | testBit r 0 = (r, p)
      | otherwise = go (r .>>. 1) (p + 1)

millerRabin :: (Integral a, Bits a, UniformRange a, StatefulGen g m) => Int -> a -> g -> m Bool
millerRabin k n g
  | n == 1 = pure False
  | n == 2 = pure True
  | even n = pure False
  | k < 9 = pure True
  | otherwise = not <$> orM (replicate k testRound)
  where
    (d, s) = exp2 n
    trialComposite a = pow a d n /= 1 && not (any (\i -> pow a (2^i * d) n == n - 1) [0 .. s - 1])
    testRound = trialComposite <$> uniformRM (2, n - 1) g

randomBits :: (Integral a, Bits a, UniformRange a, StatefulGen g m) => Int -> g -> m a
randomBits n = uniformRM (bit (n - 1), bit n - 1)

random3 :: (Integral a, Bits a, UniformRange a, StatefulGen g m) => Int -> g -> m a
random3 bits g = (.|. 3) . (.<<. 2) <$> randomBits (bits - 2) g

randomPrime :: (Integral a, Bits a, UniformRange a, StatefulGen g m) => Int -> Int -> g -> m a
randomPrime bits tests g = do
  p <- random3 bits g
  millerRabin tests p g >>= \case
    True -> pure p
    False -> randomPrime bits tests g

genKey :: (Integral a, Bits a, UniformRange a, StatefulGen g m) => Int -> Int -> g -> m (a, a)
genKey bits tests g = (,) <$> p <*> p
  where
    p = randomPrime bits tests g

genKeyIO :: Int -> IO (Integer, Integer)
genKeyIO bits = withSystemRandomST $ genKey bits 64
