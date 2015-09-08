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
  list,
  map,
  (@@)
) where

import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as L
import           Data.Char               (toUpper)
import           Data.Int
import           Data.List               (intersperse)
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Text.Encoding      as TE
import           Numeric                 (showHex)
import           Prelude                 hiding (map, null, putChar)
import qualified Prelude                 as PRE

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
                | BMap [(BoltOut, BoltOut)] BoltOutRep
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

run :: BoltOut -> BoltOutRep -> BoltOutRep
run boltOut cont = (unOut boltOut) cont

instance Show BoltOut where
  show = concat . intersperse " " . PRE.map hexWord8 . L.unpack . BB.toLazyByteString . toBuilder
    where
      hexWord8 w = pad (PRE.map toUpper (showHex w ""))
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
    step (BText value cont) =
      let encodedBytes = TE.encodeUtf8 value
      in build (B.length encodedBytes) encodedBytes <> step cont
      where
        build :: Int -> B.ByteString -> BB.Builder
        build numBytes _ | numBytes == 0 = BB.word8 0x80
        build numBytes bytes | numBytes <= 15 = BB.word8 (0x80 .|. (fromIntegral numBytes)) <> BB.byteString bytes
        build numBytes bytes | numBytes <= 255 = BB.word8 0xd0 <> BB.word8 (fromIntegral numBytes) <> BB.byteString bytes
        build numBytes bytes | numBytes <= 65535 = BB.word8 0xd1 <> BB.word16BE (fromIntegral numBytes) <> BB.byteString bytes
        build numBytes bytes = BB.word8 0xd2 <> BB.word32BE (fromIntegral numBytes) <> BB.byteString bytes
    step (BList [] cont) = BB.word8 0x90 <> step cont
    step (BList elts cont) = build elts 0 mempty
      where
        build :: [BoltOut] -> Int -> BoltOut -> BB.Builder
        build [] numElts folded | numElts <= 15 = BB.word8 (0x90 .|. (fromIntegral numElts)) <> step (run folded cont)
        build [] numElts folded | numElts <= 255 = BB.word8 0xd4 <> BB.word8 (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded | numElts <= 65535 = BB.word8 0xd5 <> BB.word16BE (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded = BB.word8 0xd6 <> BB.word32BE (fromIntegral numElts) <> step (run folded cont)
        build (b:bs) numElts folded = build bs (numElts + 1) (folded `mappend` b)
    step (BMap [] cont) = BB.word8 0xa0 <> step cont
    step (BMap elts cont) = build elts 0 mempty
      where
        build :: [(BoltOut, BoltOut)] -> Int -> BoltOut -> BB.Builder
        build [] numElts folded | numElts <= 15 = BB.word8 (0xa0 .|. (fromIntegral numElts)) <> step (run folded cont)
        build [] numElts folded | numElts <= 255 = BB.word8 0xd8 <> BB.word8 (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded | numElts <= 65535 = BB.word8 0xd9 <> BB.word16BE (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded = BB.word8 0xda <> BB.word32BE (fromIntegral numElts) <> step (run folded cont)
        build ((key, value):bs) numElts folded = build bs (numElts + 1) (folded `mappend` key `mappend` value)
    step BEnd = mempty

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

{-# INLINE (@@) #-}
(@@) ::  BoltOut -> BoltOut -> (BoltOut, BoltOut)
infix 0 @@
l @@ r = (l, r)

{-# INLINE map #-}
map :: [(BoltOut, BoltOut)] -> BoltOut
map elts = BoltOut $ (BMap elts)
