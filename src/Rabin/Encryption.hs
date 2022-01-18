module Rabin.Encryption(encrypt, decrypt) where

import Data.Bits(Bits)

import Rabin.Utils((%), (.>>.), pow)

egcd :: Integral a => a -> a -> (a, a)
egcd 0 _ = (0, 1)
egcd a b = (x - q * y, y)
  where
    (y, x) = egcd r a
    (q, r) = b `divMod` a

chineseRemainder :: Integral a => a -> a -> a -> a -> [a]
chineseRemainder p q r s = [x, n - x, y, n - y]
  where
    n = p * q
    (c, d) = egcd p q
    rdq = r * d * q
    scp = s * c * p
    x = (rdq + scp) % n
    y = (rdq - scp) % n

modSqrt :: (Bits a, Integral a) => a -> a -> a
modSqrt a p = pow a (p + 1 .>>. 2) p

encrypt :: Integral a => a -> a -> a
encrypt n m = m * m % n

decrypt :: (Bits a, Integral a) => a -> a -> a -> [a]
decrypt p q c = chineseRemainder p q (modSqrt c p) (modSqrt c q)
