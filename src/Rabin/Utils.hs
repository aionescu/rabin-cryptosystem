module Rabin.Utils where

import GHC.Num(integerPowMod#, integerToNatural, integerFromNatural)

powMod :: Integer -> Integer -> Integer -> Integer
powMod b e n =
  case integerPowMod# b e (integerToNatural n) of
    (# | _ #) -> error "powMod: negative e or zero n"
    (# r | #) -> integerFromNatural r
{-# INLINE powMod #-}
