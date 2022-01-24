module Rabin.KeyGen(genKeyIO) where

import Control.Monad.Extra(orM)
import Data.Bits((.|.), bit, testBit)
import System.Random.MWC(withSystemRandomST)
import System.Random.Stateful(StatefulGen, uniformRM)

import Rabin.Utils((.<<.), (.>>.), pow)

exp2 :: Integer -> (Integer, Integer)
exp2 n = go (n - 1) 0
  where
    go r p
      | testBit r 0 = (r, p)
      | otherwise = go (r .>>. 1) (p + 1)

millerRabin :: StatefulGen g m => Int -> Integer -> g -> m Bool
millerRabin k n g
  | n == 1 = pure False
  | n == 2 = pure True
  | even n = pure False
  | n < 9 = pure True
  | otherwise = not <$> orM (replicate k testRound)
  where
    (d, s) = exp2 n
    trialComposite a = pow a d n /= 1 && not (any (\i -> pow a (2^i * d) n == n - 1) [0 .. s - 1])
    testRound = trialComposite <$> uniformRM (2, n - 1) g

randomBits :: StatefulGen g m => Int -> g -> m Integer
randomBits n = uniformRM (bit (n - 1), bit n - 1)

random3 :: StatefulGen g m => Int -> g -> m Integer
random3 bits g = (.|. 3) . (.<<. 2) <$> randomBits (bits - 2) g

randomPrime :: StatefulGen g m => Int -> Int -> g -> m Integer
randomPrime bits tests g = do
  p <- random3 bits g
  millerRabin tests p g >>= \case
    True -> pure p
    False -> randomPrime bits tests g

genKey :: StatefulGen g m => Int -> Int -> g -> m (Integer, Integer)
genKey bits tests g = (,) <$> p <*> p
  where
    p = randomPrime bits tests g

genKeyIO :: Int -> IO (Integer, Integer)
genKeyIO bits = withSystemRandomST $ genKey bits 64
