module Rabin.Encoding(encryptBytes, decryptBytes, encodeInteger, decodeInteger) where

import Data.Bits(shiftL, shiftR, (.&.))
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Int(Int64)

import Rabin.Encryption(decrypt, encrypt)

decodeInteger :: ByteString -> Integer
decodeInteger = B.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0

encodeInteger :: Int64 -> Integer -> ByteString
encodeInteger size i = B.replicate (size - B.length bs) 0 <> bs
  where
    bs = B.reverse $ B.unfoldr f i

    f 0 = Nothing
    f s = Just (fromIntegral $ s .&. 255, s `shiftR` 8)

chunks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
chunks f k s
  | B.null s = ""
  | otherwise = f chunk <> chunks f k rest
  where
    (chunk, rest) = B.splitAt k s
{-# INLINE chunks #-}

toBlocks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
toBlocks f blockSize bs =
  chunks f blockSize
  $ B.cons (fromIntegral padding) bs
  <> B.replicate padding 0
  where
    padding =
      case (B.length bs + 1) `mod` blockSize of
        0 -> 0
        extra -> blockSize - extra
{-# INLINE toBlocks #-}

ofBlocks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
ofBlocks f cipherSize bs
  | B.length bs `mod` cipherSize /= 0 = error "ofBlocks: Invalid block size"
  | otherwise = removePadding $ chunks f cipherSize bs
{-# INLINE ofBlocks #-}

removePadding :: ByteString -> ByteString
removePadding b =
  case B.uncons b of
    Nothing -> error "removePadding: Empty list"
    Just (padding, bs) -> B.dropEnd (fromIntegral padding) bs

addRedundancy :: Int64 -> ByteString -> ByteString
addRedundancy amount b = B.take amount b <> b

checkRedundancy :: Int64 -> ByteString -> (# ByteString | () #)
checkRedundancy amount b
  | b0 `B.isPrefixOf` b' = (# b' | #)
  | otherwise = (# | () #)
  where
    (b0, b') = B.splitAt amount b

encryptBlock :: Int64 -> Int64 -> Integer -> ByteString -> ByteString
encryptBlock cipherSize redundancy n =
  encodeInteger cipherSize
  . encrypt n
  . decodeInteger
  . addRedundancy redundancy
{-# INLINE encryptBlock #-}

decryptBlock :: Int64 -> Int64 -> Integer -> Integer -> ByteString -> ByteString
decryptBlock blockSize redundancy p q block =
  disambiguate (decrypt p q $ decodeInteger block)
  where
    removeRedundancy c = checkRedundancy redundancy $ encodeInteger (redundancy + blockSize) c

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
  toBlocks (encryptBlock cipherSize redundancy n) blockSize

decryptBytes :: Int64 -> Integer -> Integer -> ByteString -> ByteString
decryptBytes (encodingInfo -> (# blockSize, redundancy, cipherSize #)) p q =
  ofBlocks (decryptBlock blockSize redundancy p q) cipherSize

encodingInfo :: Int64 -> (# Int64, Int64, Int64 #)
encodingInfo pubKeyBytes = (# pubKeyBytes - redundancy - 1, redundancy, pubKeyBytes #)
  where
    redundancy = pubKeyBytes `shiftR` 4
