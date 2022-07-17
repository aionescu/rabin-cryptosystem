module Rabin.Utils where

import GHC.Num(integerPowMod#)

powMod :: Integer -> Integer -> Integer -> Integer
powMod b e n =
  case integerPowMod# b e (fromInteger n) of
    (# | () #) -> error "powMod: negative e or zero n"
    (# r | #) -> fromIntegral r
