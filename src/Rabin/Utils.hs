module Rabin.Utils where

import Data.Bits(Bits, shiftL, shiftR, testBit)

(//) :: Integral a => a -> a -> a
(//) = div
infixl 7 //
{-# SPECIALIZE (//) :: Integer -> Integer -> Integer #-}
{-# INLINE (//) #-}

(%) :: Integral a => a -> a -> a
(%) = mod
infixl 7 %
{-# SPECIALIZE (%) :: Integer -> Integer -> Integer #-}
{-# INLINE (%) #-}

(.<<.) :: Bits a => a -> Int -> a
(.<<.) = shiftL
infix 5 .<<.
{-# SPECIALIZE (.<<.) :: Integer -> Int -> Integer #-}
{-# INLINE (.<<.) #-}

(.>>.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
infix 5 .>>.
{-# SPECIALIZE (.>>.) :: Integer -> Int -> Integer #-}
{-# INLINE (.>>.) #-}

pow :: Integer -> Integer -> Integer -> Integer
pow = go 1
  where
    go r _ 0 _ = r
    go r b e m = go r' (b * b % m) (e .>>. 1) m
      where
        r' = if testBit e 0 then r * b % m else r
    {-# INLINE go #-}
{-# INLINE pow #-}
