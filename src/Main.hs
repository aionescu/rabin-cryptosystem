module Main(main) where

import Control.Monad(unless)
import Data.Bifunctor(bimap)
import Data.Bits((.>>.))
import Data.ByteString.Lazy qualified as B
import System.Exit(exitFailure)

import Opts(Opts(..), getOpts)
import Rabin.KeyGen(Key(..), genKey)
import Rabin.Encoding(decodeInteger, encodeInteger, decryptBytes, encryptBytes)

genPrivKey :: Bool -> Int -> Int -> IO ()
genPrivKey fixedSeed tests bits = do
  unless (bits `elem` [128, 256, 512, 1024]) do
    putStrLn "Error: Private key size must be 128, 256, 512 or 1024"
    exitFailure

  Key p q <- genKey fixedSeed tests bits
  let privKeySize = fromIntegral $ bits .>>. 4

  B.putStr $ encodeInteger privKeySize p <> encodeInteger privKeySize q

genPubKey :: IO ()
genPubKey = do
  privKey <- B.getContents
  let
    pubKeySize = B.length privKey
    privKeySize = pubKeySize .>>. 1
    (p, q) = bimap decodeInteger decodeInteger $ B.splitAt privKeySize privKey

  B.putStr $ encodeInteger pubKeySize $ p * q

encrypt :: FilePath -> IO ()
encrypt pubKey = do
  nBin <- B.readFile pubKey
  msg <- B.getContents

  let n = decodeInteger nBin
  B.putStr $ encryptBytes (B.length nBin) n msg

decrypt :: FilePath -> IO ()
decrypt privKey = do
  bin <- B.readFile privKey

  let pubKeySize = B.length bin
  let privKeySize = pubKeySize .>>. 1
  let (p, q) = bimap decodeInteger decodeInteger $ B.splitAt privKeySize bin

  msg <- B.getContents
  B.putStr $ decryptBytes pubKeySize p q msg

main :: IO ()
main =
  getOpts >>= \case
    GenPrivKey{..} -> genPrivKey fixedSeed tests bits
    GenPubKey -> genPubKey
    Encrypt{..} -> encrypt pubKey
    Decrypt{..} -> decrypt privKey
