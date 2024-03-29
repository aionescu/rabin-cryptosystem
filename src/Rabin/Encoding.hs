module Rabin.Encoding(encryptBytes, decryptBytes, encodeInteger, decodeInteger) where

import Data.Bits((.<<.), (.>>.))
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Int(Int64)

import Rabin.Encryption(decrypt, encrypt)

decodeInteger :: ByteString -> Integer
decodeInteger = B.foldl' (\i b -> (i .<<. 8) + fromIntegral b) 0

encodeInteger :: Int64 -> Integer -> ByteString
encodeInteger size i = snd $ B.mapAccumR (\i _ -> (i .>>. 8, fromInteger i)) i $ B.replicate size 0

chunks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
chunks f k s
  | B.null s = ""
  | otherwise = f chunk <> chunks f k rest
  where
    (chunk, rest) = B.splitAt k s

toBlocks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
toBlocks f blockSize bs = chunks f blockSize $ pad blockSize bs
  where
    -- Pad according to the PKCS#7 scheme
    pad :: Int64 -> ByteString -> ByteString
    pad blockSize bs
      | diff == 0 = bs
      | otherwise = bs <> B.replicate (blockSize - diff) 0x04
      where
        diff = B.length bs `mod` blockSize

ofBlocks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
ofBlocks f cipherSize bs
  | B.length bs `mod` cipherSize /= 0 = error "ofBlocks: Invalid block size"
  | otherwise = B.dropWhileEnd (== 0x04) $ chunks f cipherSize bs

-- https://www.diva-portal.org/smash/get/diva2:1581080/FULLTEXT01.pdf
-- This paper experimentally shows that padding the message between 1's
-- produces very low collision rates. The paper seems to do the padding
-- in decimal, but I'm doing it in binary here, padding with 255's (i.e. all-1 bytes).
addRedundancy :: ByteString -> ByteString -> ByteString
addRedundancy padding b = padding <> b <> padding

checkRedundancy :: Int64 -> ByteString -> (# ByteString | () #)
checkRedundancy amount b
  | B.all (== 255) (B.take half b) && B.all (== 255) (B.takeEnd half b) = (# B.drop half (B.dropEnd half b) | #)
  | otherwise = (# | () #)
  where
    half = amount .>>. 1

encryptBlock :: Int64 -> Integer -> ByteString -> ByteString -> ByteString
encryptBlock cipherSize n redundancy =
  encodeInteger cipherSize
  . encrypt n
  . decodeInteger
  . addRedundancy redundancy

decryptBlock :: Int64 -> Int64 -> Integer -> Integer -> ByteString -> ByteString
decryptBlock blockSize redundancy p q block =
  disambiguate (decrypt p q $ decodeInteger block)
  where
    fullBlockSize = redundancy + blockSize
    removeRedundancy c = checkRedundancy redundancy $ encodeInteger fullBlockSize c

    disambiguate :: (# Integer, Integer, Integer, Integer #) -> ByteString
    disambiguate (# a, b, c, d #) =
      case (# removeRedundancy a, removeRedundancy b, removeRedundancy c, removeRedundancy d #) of
        (# (# r | #), (# | _ #), (# | _ #), (# | _ #) #) -> r
        (# (# | _ #), (# r | #), (# | _ #), (# | _ #) #) -> r
        (# (# | _ #), (# | _ #), (# r | #), (# | _ #) #) -> r
        (# (# | _ #), (# | _ #), (# | _ #), (# r | #) #) -> r
        (# (# | _ #), (# | _ #), (# | _ #), (# | _ #) #) -> error "decryptBlock: No square roots"
        _                                                -> error "decryptBlock: Multiple square roots"

encryptBytes :: Int64 -> Integer -> ByteString -> ByteString
encryptBytes (encodingInfo -> (# blockSize, redundancy, cipherSize #)) n =
  toBlocks (encryptBlock cipherSize n padding) blockSize
  where
    padding = B.replicate (redundancy .>>. 1) 255

decryptBytes :: Int64 -> Integer -> Integer -> ByteString -> ByteString
decryptBytes (encodingInfo -> (# blockSize, redundancy, cipherSize #)) p q =
  ofBlocks (decryptBlock blockSize redundancy p q) cipherSize

encodingInfo :: Int64 -> (# Int64, Int64, Int64 #)
encodingInfo pubKeyBytes = (# pubKeyBytes - redundancy - 1, redundancy, pubKeyBytes #)
  where
    redundancy = pubKeyBytes .>>. 4
