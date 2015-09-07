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
  int64,
  text,
  string,
  list
) where

import           Data.Char               (toUpper)
import           Data.List               (intersperse)
import           Numeric                 (showHex)
import           Prelude                 hiding (null, putChar)

import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as L
import           Data.Int
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Text.Encoding      as TE

data BoltOutRep = BNull BoltOutRep
                | BFalse BoltOutRep
                | BTrue BoltOutRep
                | BFloat64 Double BoltOutRep
                | BTinyInt Int8 BoltOutRep
                | BInt8 Int8 BoltOutRep
                | BInt16 Int16 BoltOutRep
                | BInt32 Int32 BoltOutRep
                | BInt64 Int64 BoltOutRep
                | BText T.Text BoltOutRep
                | BList [BoltOut] BoltOutRep
                | BEnd

type Encoder t = t -> BoltOut

newtype BoltOut = BoltOut { unOut :: BoltOutRep -> BoltOutRep }

instance Monoid BoltOut where
  {-# INLINE mempty #-}
  mempty            = BoltOut id
  {-# INLINE mappend #-}
  b1 `mappend` b2   = BoltOut (unOut b1 . unOut b2)
  {-# INLINE mconcat #-}
  mconcat           = foldl mappend mempty

instance Show BoltOut where
  show = concat . intersperse " " . map hexWord8 . L.unpack . BB.toLazyByteString . toBuilder
    where
      hexWord8 w = pad (map toUpper (showHex w ""))
      pad cs = replicate (2 - length cs) '0' ++ cs

toBuilder :: BoltOut -> BB.Builder
toBuilder vs0 = step (unOut vs0 BEnd)
  where
    step (BNull cont) = BB.word8 0xc0 <> step cont
    step (BFloat64 value cont) = BB.word8 0xc1 <> BB.doubleBE value <> step cont
    step (BFalse cont) = BB.word8 0xc2 <> step cont
    step (BTrue cont) = BB.word8 0xc3 <> step cont
    step (BTinyInt value cont) | not $ testBit value 7 = BB.int8 value <> step cont
    step (BTinyInt value cont) | value .&. (-16) == (-16) = BB.int8 value <> step cont
    step (BTinyInt value cont) = BB.word8 0xc8 <> BB.int8 value <> step cont
    step (BInt8 value cont) = BB.word8 0xc8 <> BB.int8 value <> step cont
    step (BInt16 value cont) = BB.word8 0xc9 <> BB.int16BE value <> step cont
    step (BInt32 value cont) = BB.word8 0xca <> BB.int32BE value <> step cont
    step (BInt64 value cont) = BB.word8 0xcb <> BB.int64BE value <> step cont
    step (BText value cont) = buildText numBytes bytes <> step cont
      where
        bytes = TE.encodeUtf8 value
        numBytes = B.length bytes
    step (BList [] cont) = BB.word8 0x90 <> step cont
    step (BList elts cont) = buildList elts 0 mempty
      where
        buildList :: [BoltOut] -> Int -> BoltOut -> BB.Builder
        buildList [] numElts folded | numElts <= 15 = BB.word8 (0x90 .|. (fromIntegral numElts)) <> (buildCont folded)
        buildList [] numElts folded | numElts <= 255 = BB.word8 0xd4 <> BB.word8 (fromIntegral numElts) <> (buildCont folded)
        buildList [] numElts folded | numElts <= 65535 = BB.word8 0xd5 <> BB.word16BE (fromIntegral numElts) <> (buildCont folded)
        buildList [] numElts folded = BB.word8 0xd6 <> BB.word32BE (fromIntegral numElts) <> (buildCont folded)
        buildList (b:bs) numElts folded = buildList bs (numElts + 1) (folded `mappend` b)
        buildCont :: BoltOut -> BB.Builder
        buildCont folded = step ((unOut folded) cont)
    step BEnd = mempty

buildText :: Int -> B.ByteString -> BB.Builder
buildText numBytes _ | numBytes == 0 = BB.word8 0x80
buildText numBytes bytes | numBytes <= 15 = BB.word8 (0x80 .|. (fromIntegral numBytes)) <> BB.byteString bytes
buildText numBytes bytes | numBytes <= 255 = BB.word8 0xd0 <> BB.word8 (fromIntegral numBytes) <> BB.byteString bytes
buildText numBytes bytes | numBytes <= 65535 = BB.word8 0xd1 <> BB.word16BE (fromIntegral numBytes) <> BB.byteString bytes
buildText numBytes bytes = BB.word8 0xd2 <> BB.word32BE (fromIntegral numBytes) <> BB.byteString bytes

{-# INLINE null #-}
null :: BoltOut
null = BoltOut $ BNull

{-# INLINE false #-}
false :: BoltOut
false = BoltOut $ BFalse

{-# INLINE true #-}
true :: BoltOut
true = BoltOut $ BTrue

{-# INLINE float64 #-}
float64 :: Double -> BoltOut
float64 value = BoltOut $ (BFloat64 value)

{-# INLINE tinyInt #-}
tinyInt :: Int8 -> BoltOut
tinyInt value = BoltOut $ (BTinyInt value)

{-# INLINE int8 #-}
int8 :: Int8 -> BoltOut
int8 value = BoltOut $ (BInt8 value)

{-# INLINE int16 #-}
int16 :: Int16 -> BoltOut
int16 value = BoltOut $ (BInt16 value)

{-# INLINE int32 #-}
int32 :: Int32 -> BoltOut
int32 value = BoltOut $ (BInt32 value)

{-# INLINE int64 #-}
int64 :: Int64 -> BoltOut
int64 value = BoltOut $ (BInt64 value)

{-# INLINE text #-}
text :: T.Text -> BoltOut
text value = BoltOut $ (BText value)

{-# INLINE string #-}
string :: String -> BoltOut
string value = BoltOut $ (BText (T.pack value))

{-# INLINE list #-}
list :: [BoltOut] -> BoltOut
list elts = BoltOut $ (BList elts)
