{-# OPTIONS_GHC -Wno-deprecations  #-}

module Rabin.Utils where

import GHC.Integer.GMP.Internals(powModInteger)

pow :: Integer -> Integer -> Integer -> Integer
pow = powModInteger
