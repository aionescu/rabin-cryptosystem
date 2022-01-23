module Rabin.Encoding(encryptBytes, decryptBytes, encodeInteger, decodeInteger, encodingInfo) where

import Control.Category((>>>))
import Data.Bits(shiftL, shiftR)
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Int(Int64)
import Data.Maybe(mapMaybe)

import Rabin.Encryption(decrypt, encrypt)
import Rabin.Utils((%), (.>>.))

decodeInteger :: ByteString -> Integer
decodeInteger = B.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0

encodeInteger :: Int64 -> Integer -> ByteString
encodeInteger size i = B.replicate (size - B.length bs) 0 <> bs
  where
    bs = B.reverse $ B.unfoldr f i

    f 0 = Nothing
    f s = Just (fromIntegral $ s % 256, s `shiftR` 8)

chunks :: Int64 -> ByteString -> [ByteString]
chunks k s
  | B.null s = []
  | otherwise = chunk : chunks k rest
  where
    (chunk, rest) = B.splitAt k s

toBlocks :: Int64 -> ByteString -> [ByteString]
toBlocks blockSize bs =
  chunks blockSize
  $ B.cons (fromIntegral padding) bs
  <> B.replicate padding 0
  where
    padding =
      case (B.length bs + 1) % blockSize of
        0 -> 0
        extra -> blockSize - extra

ofBlocks :: Int64 -> ByteString -> [ByteString]
ofBlocks cipherSize bs
  | B.length bs % cipherSize /= 0 = error "ofBlocks: Invalid block size"
  | otherwise = chunks cipherSize bs

removePadding :: ByteString -> ByteString
removePadding =
  B.uncons >>> \case
    Nothing -> error "removePadding: Empty list"
    Just (padding, bs) -> B.dropEnd (fromIntegral padding) bs

addRedundancy :: Int64 -> ByteString -> ByteString
addRedundancy amount b = B.take amount b <> b

checkRedundancy :: Int64 -> ByteString -> Maybe ByteString
checkRedundancy amount b
  | b0 == B.take amount b' = Just b'
  | otherwise = Nothing
  where
    (b0, b') = B.splitAt amount b

encryptBlock :: Int64 -> Int64 -> Integer -> ByteString -> ByteString
encryptBlock cipherSize redundancy n =
  encodeInteger cipherSize
  . encrypt n
  . decodeInteger
  . addRedundancy redundancy

decryptBlock :: Int64 -> Int64 -> Integer -> Integer -> ByteString -> ByteString
decryptBlock blockSize redundancy p q =
  disambiguate
  . mapMaybe removeRedundancy
  . decrypt p q
  . decodeInteger
  where
    removeRedundancy = checkRedundancy redundancy . encodeInteger (redundancy + blockSize)
    disambiguate = \case
      [] -> error "decryptBlock: No square roots"
      [m] -> m
      _ -> error "decryptBlock: Multiple square roots"

encryptBytes :: Int64 -> Integer -> ByteString -> ByteString
encryptBytes (encodingInfo -> (blockSize, redundancy, cipherSize)) n =
  foldMap (encryptBlock cipherSize redundancy n)
  . toBlocks blockSize

decryptBytes :: Int64 -> Integer -> Integer -> ByteString -> ByteString
decryptBytes (encodingInfo -> (blockSize, redundancy, cipherSize)) p q =
  removePadding
  . foldMap (decryptBlock blockSize redundancy p q)
  . ofBlocks cipherSize

encodingInfo :: Int64 -> (Int64, Int64, Int64)
encodingInfo pubKeyBytes = (pubKeyBytes - redundancy - 1, redundancy, pubKeyBytes)
  where
    redundancy = pubKeyBytes .>>. 4
