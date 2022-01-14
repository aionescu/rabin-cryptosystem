module Rabin.Utils where

import Data.Bits(Bits, shiftL, shiftR, testBit)

(//) :: Integral a => a -> a -> a
(//) = div
infixl 7 //

(%) :: Integral a => a -> a -> a
(%) = mod
infixl 7 %

(.<<.) :: Bits a => a -> Int -> a
(.<<.) = shiftL
infix 5 .<<.

(.>>.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
infix 5 .>>.

pow :: (Bits a, Integral a) => a -> a -> a -> a
pow b' e' m = go b' e' 1
  where
    go _ 0 r = r
    go b e r = go (b * b % m) (e .>>. 1) r'
      where
        r' = if testBit e 0 then r * b % m else r
