module Rabin.Encryption(encrypt, decrypt) where

import Data.Bits(shiftR)
import GHC.Integer(divModInteger)

import Rabin.Utils(powMod)

egcd :: Integer -> Integer -> (# Integer, Integer #)
egcd 0 _ = (# 0, 1 #)
egcd a b = (# x - q * y, y #)
  where
    (# y, x #) = egcd r a
    (# q, r #) = b `divModInteger` a
{-# INLINE egcd #-}

chineseRemainder :: Integer -> Integer -> Integer -> Integer -> (# Integer, Integer, Integer, Integer #)
chineseRemainder p q r s = (# x, n - x, y, n - y #)
  where
    n = p * q
    (# c, d #) = egcd p q
    rdq = r * d * q
    scp = s * c * p
    x = (rdq + scp) `mod` n
    y = (rdq - scp) `mod` n
{-# INLINE chineseRemainder #-}

modSqrt :: Integer -> Integer -> Integer
modSqrt a p = powMod a ((p + 1) `shiftR` 2) p
{-# INLINE modSqrt #-}

encrypt :: Integer -> Integer -> Integer
encrypt n m = m * m `mod` n
{-# INLINE encrypt #-}

decrypt :: Integer -> Integer -> Integer -> (# Integer, Integer, Integer, Integer #)
decrypt p q c = chineseRemainder p q (modSqrt c p) (modSqrt c q)
{-# INLINE decrypt #-}
