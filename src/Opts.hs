module Opts(Opts(..), getOpts) where

import Options.Generic

data Opts
  = GenPrivKey { bits :: Maybe Int }
  | GenPubKey
  | Encrypt { pubKey :: FilePath }
  | Decrypt { privKey :: FilePath }
  deriving stock Generic

instance ParseRecord Opts where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getOpts :: IO Opts
getOpts = getRecord "Rabin Cryptosystem"
