module Rabin.Encryption(encrypt, decrypt) where

import Data.Bits(Bits)

import Rabin.Utils((%), (.<<.), (.>>.), pow)

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

legendre :: (Bits a, Integral a) => a -> a -> a
legendre a p = pow a (p - 1 .>>. 1) p

modSqrt :: (Bits a, Integral a) => a -> a -> a
modSqrt a p
  | a == 0 || p == 2 || legendre a p /= 1 = error "modSqrt: No square root"
  | p % 4 == 3 = pow a (p + 1 .>>. 2) p
  | p % 8 == 5 =
      let d = pow a (p - 1 .>>. 2) p
      in if
        | d == 1 -> pow a (p + 3 .>>. 3) p
        | d == p - 1 -> pow (a .<<. 2) (p - 5 .>>. 3) p * (a .<<. 1) % p
        | otherwise -> 0
  | otherwise = error "modSqrt: Unsupported prime"

encrypt :: Integral a => a -> a -> a
encrypt n m = m * m % n

decrypt :: (Bits a, Integral a) => a -> a -> a -> [a]
decrypt p q c = chineseRemainder p q a b
  where
    a = modSqrt c p
    b = modSqrt c q
