module Rabin.Encoding(encryptBytes, decryptBytes, encodeInteger, decodeInteger) where

import Data.Bits((.<<.), (.>>.))
import Data.ByteString.Internal(c2w)
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Int(Int64)
import Data.Word(Word8)

import Rabin.Encryption(decrypt, encrypt)

decodeInteger :: ByteString -> Integer
decodeInteger = B.foldl' (\i b -> (i .<<. 8) + fromIntegral b) 0

encodeInteger :: Int64 -> Integer -> ByteString
encodeInteger size i = B.replicate (size - B.length bs) 0 <> bs
  where
    bs = B.reverse $ B.unfoldr f i

    f 0 = Nothing
    f s = Just (fromInteger s, s .>>. 8)

chunks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
chunks f k s
  | B.null s = ""
  | otherwise = f chunk <> chunks f k rest
  where
    (chunk, rest) = B.splitAt k s
{-# INLINE chunks #-}

eot :: Word8
eot = c2w '\EOT'

toBlocks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
toBlocks f blockSize bs = chunks f blockSize $ pad blockSize bs
  where
    pad :: Int64 -> ByteString -> ByteString
    pad blockSize bs
      | diff == 0 = bs
      | otherwise = bs <> B.replicate (blockSize - diff) eot
      where
        diff = B.length bs `mod` blockSize
    {-# INLINE pad #-}
{-# INLINE toBlocks #-}

ofBlocks :: (ByteString -> ByteString) -> Int64 -> ByteString -> ByteString
ofBlocks f cipherSize bs
  | B.length bs `mod` cipherSize /= 0 = error "ofBlocks: Invalid block size"
  | otherwise = B.dropWhileEnd (== eot) $ chunks f cipherSize bs
{-# INLINE ofBlocks #-}

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
    fullBlockSize = redundancy + blockSize
    removeRedundancy c = checkRedundancy redundancy $ encodeInteger fullBlockSize c
    {-# INLINE removeRedundancy #-}

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
    redundancy = pubKeyBytes .>>. 4
