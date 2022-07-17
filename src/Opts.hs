{-# OPTIONS_GHC -Wno-partial-fields #-}

module Opts(Opts(..), getOpts) where

import Options.Generic

data Opts w
  = GenPrivKey
    { bits :: w ::: Int <!> "256" <?> "Length of the private key, in bits"
    , tests :: w ::: Int <!> "64" <?> "The number of Miller-Rabin trial rounds"
    , fixedSeed :: w ::: Bool <?> "Use a fixed RNG seed. USE ONLY FOR TESTING!"
    }
  | GenPubKey
  | Encrypt { pubKey :: w ::: FilePath <?> "Path to the public key file" }
  | Decrypt { privKey :: w ::: FilePath <?> "Path to the private key file" }
  deriving stock Generic

instance ParseRecord (Opts Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getOpts :: IO (Opts Unwrapped)
getOpts = unwrapRecord "Rabin Cryptosystem"
