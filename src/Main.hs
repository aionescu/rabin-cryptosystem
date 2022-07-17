module Main(main) where

import Control.Monad(when)
import Data.Bifunctor(bimap)
import Data.Bits(shiftR)
import Data.ByteString.Lazy qualified as B
import System.Exit(exitFailure)
import System.IO(stdout)

import Opts(Opts(..), getOpts)
import Rabin.KeyGen(genKey)
import Rabin.Encoding(decodeInteger, encodeInteger, decryptBytes, encryptBytes)

genPrivKey :: Bool -> Int -> IO ()
genPrivKey fixedSeed bits = do
  when (bits `notElem` [128, 256, 512, 1024]) do
    putStrLn "Error: Private key size must be 128, 256, 512 or 1024"
    exitFailure

  (p, q) <- genKey fixedSeed bits
  let privKeySize = fromIntegral $ bits `shiftR` 4

  B.hPut stdout $ encodeInteger privKeySize p <> encodeInteger privKeySize q

genPubKey :: IO ()
genPubKey = do
  privKey <- B.getContents
  let
    pubKeySize = B.length privKey
    privKeySize = pubKeySize `shiftR` 1

  let (p, q) = bimap decodeInteger decodeInteger $ B.splitAt privKeySize privKey
  B.hPut stdout $ encodeInteger pubKeySize $ p * q

encrypt :: FilePath -> IO ()
encrypt pubKey = do
  nBin <- B.readFile pubKey
  msg <- B.getContents

  let n = decodeInteger nBin
  B.hPut stdout $ encryptBytes (B.length nBin) n msg

decrypt :: FilePath -> IO ()
decrypt privKey = do
  bin <- B.readFile privKey

  let pubKeySize = B.length bin
  let privKeySize = pubKeySize `shiftR` 1
  let (p, q) = bimap decodeInteger decodeInteger $ B.splitAt privKeySize bin

  msg <- B.getContents

  B.hPut stdout $ decryptBytes pubKeySize p q msg

main :: IO ()
main =
  getOpts >>= \case
    GenPrivKey{..} -> genPrivKey fixedSeed bits
    GenPubKey -> genPubKey
    Encrypt{..} -> encrypt pubKey
    Decrypt{..} -> decrypt privKey
