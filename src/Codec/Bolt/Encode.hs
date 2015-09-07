{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Bolt.Encode(
  Encoder,
  BoltOut,
  null,
  false,
  true,
  float64,
  tinyInt,
  int8,
  int16,
  int32,
  int64
) where

import           Data.Char               (toUpper)
import           Data.List               (intersperse)
import           Numeric                 (showHex)
import           Prelude                 hiding (null, putChar)

import           Data.Bits
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as L
import           Data.Int
import           Data.Monoid

data BoltOutRep = BNull BoltOutRep
                | BFalse BoltOutRep
                | BTrue BoltOutRep
                | BFloat64 Double BoltOutRep
                | BTinyInt Int8 BoltOutRep
                | BInt8 Int8 BoltOutRep
                | BInt16 Int16 BoltOutRep
                | BInt32 Int32 BoltOutRep
                | BInt64 Int64 BoltOutRep
                | BEnd

type Encoder t = t -> BoltOut

newtype BoltOut = BoltOut { unOut :: BoltOutRep -> BoltOutRep }

instance Monoid BoltOut where
  {-# INLINE mempty #-}
  mempty            = BoltOut id
  {-# INLINE mappend #-}
  b1 `mappend` b2   = BoltOut (unOut b1 . unOut b2)
  {-# INLINE mconcat #-}
  mconcat           = foldr mappend mempty

instance Show BoltOut where
  show = concat . intersperse " " . map hexWord8 . L.unpack . B.toLazyByteString . toBuilder
    where
      hexWord8 w = pad (map toUpper (showHex w ""))
      pad cs = replicate (2 - length cs) '0' ++ cs

toBuilder :: BoltOut -> B.Builder
toBuilder vs0 = step (unOut vs0 BEnd)
  where
    step (BNull cont) = B.word8 0xc0 <> step cont
    step (BFloat64 value cont) = B.word8 0xc1 <> B.doubleBE value <> step cont
    step (BFalse cont) = B.word8 0xc2 <> step cont
    step (BTrue cont) = B.word8 0xc3 <> step cont
    step (BTinyInt value cont) | not $ testBit value 7 = B.int8 value <> step cont
    step (BTinyInt value cont) | value .&. (-16) == (-16) = B.int8 value <> step cont
    step (BTinyInt value cont) = B.word8 0xc8 <> B.int8 value <> step cont
    step (BInt8 value cont) = B.word8 0xc8 <> B.int8 value <> step cont
    step (BInt16 value cont) = B.word8 0xc9 <> B.int16BE value <> step cont
    step (BInt32 value cont) = B.word8 0xca <> B.int32BE value <> step cont
    step (BInt64 value cont) = B.word8 0xcb <> B.int64BE value <> step cont
    step BEnd = mempty

{-INLINE null-}
null :: BoltOut
null = BoltOut $ BNull

{-INLINE false-}
false :: BoltOut
false = BoltOut $ BFalse

{-INLINE true-}
true :: BoltOut
true = BoltOut $ BTrue

{-INLINE float64-}
float64 :: Double -> BoltOut
float64 value = BoltOut $ (BFloat64 value)

{-INLINE tinyInt-}
tinyInt :: Int8 -> BoltOut
tinyInt value = BoltOut $ (BTinyInt value)

{-INLINE int8-}
int8 :: Int8 -> BoltOut
int8 value = BoltOut $ (BInt8 value)

{-INLINE int16-}
int16 :: Int16 -> BoltOut
int16 value = BoltOut $ (BInt16 value)

{-INLINE int32-}
int32 :: Int32 -> BoltOut
int32 value = BoltOut $ (BInt32 value)

{-INLINE int64-}
int64 :: Int64 -> BoltOut
int64 value = BoltOut $ (BInt64 value)
