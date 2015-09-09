module Codec.Packstream.Encode(
  Encoder,
  PackStream,
  null,
  false,
  true,
  float64,
  tinyInt,
  int8,
  int16,
  int32,
  int64,
  int,
  text,
  string,
  list,
  map,
  (@@),
  structure
) where

import           Codec.Packstream.Signature
import           Data.Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as L
import           Data.Char                  (toUpper)
import           Data.Int
import           Data.List                  (intersperse)
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Encoding         as TE
import           Data.Word
import           Numeric                    (showHex)
import           Prelude                    hiding (map, null, putChar)
import qualified Prelude                    as PRE

data PackStreamRep = PNull PackStreamRep
                   | PFalse PackStreamRep
                   | PTrue PackStreamRep
                   | PFloat64 Double PackStreamRep
                   | PTinyInt Int8 PackStreamRep
                   | PInt8 Int8 PackStreamRep
                   | PInt16 Int16 PackStreamRep
                   | PInt32 Int32 PackStreamRep
                   | PInt64 Int64 PackStreamRep
                   | PText T.Text PackStreamRep
                   | PList [PackStream] PackStreamRep
                   | PMap [(PackStream, PackStream)] PackStreamRep
                   | PStructure Signature [PackStream] PackStreamRep
                   | PEnd

type Encoder t = t -> PackStream

newtype PackStream = PackStream { unOut :: PackStreamRep -> PackStreamRep }

instance Monoid PackStream where
  {-# INLINE mempty #-}
  mempty            = PackStream id
  {-# INLINE mappend #-}
  b1 `mappend` b2   = PackStream (unOut b1 . unOut b2)
  {-# INLINE mconcat #-}
  mconcat           = foldl mappend mempty

run :: PackStream -> PackStreamRep -> PackStreamRep
run boltOut cont = (unOut boltOut) cont

instance Show PackStream where
  show = concat . intersperse " " . PRE.map hexWord8 . L.unpack . BB.toLazyByteString . toBuilder
    where
      hexWord8 w = pad (PRE.map toUpper (showHex w ""))
      pad cs = replicate (2 - length cs) '0' ++ cs

toBuilder :: PackStream -> BB.Builder
toBuilder vs0 = step (unOut vs0 PEnd)
  where
    step (PNull cont) = BB.word8 0xc0 <> step cont
    step (PFloat64 value cont) = BB.word8 0xc1 <> BB.doubleBE value <> step cont
    step (PFalse cont) = BB.word8 0xc2 <> step cont
    step (PTrue cont) = BB.word8 0xc3 <> step cont
    step (PTinyInt value cont) | not $ testBit value 7 = BB.int8 value <> step cont
    step (PTinyInt value cont) | ((fromIntegral value) :: Word8) .&. 240 == 240 = BB.int8 value <> step cont
    step (PTinyInt value cont) = BB.word8 0xc8 <> BB.int8 value <> step cont
    step (PInt8 value cont) = BB.word8 0xc8 <> BB.int8 value <> step cont
    step (PInt16 value cont) = BB.word8 0xc9 <> BB.int16BE value <> step cont
    step (PInt32 value cont) = BB.word8 0xca <> BB.int32BE value <> step cont
    step (PInt64 value cont) = BB.word8 0xcb <> BB.int64BE value <> step cont
    step (PText value cont) =
      let encodedBytes = TE.encodeUtf8 value
      in build (B.length encodedBytes) encodedBytes <> step cont
      where
        build :: Int -> B.ByteString -> BB.Builder
        build numBytes _ | numBytes == 0 = BB.word8 0x80
        build numBytes bytes | numBytes <= 15 = BB.word8 (0x80 .|. (fromIntegral numBytes)) <> BB.byteString bytes
        build numBytes bytes | numBytes <= 255 = BB.word8 0xd0 <> BB.word8 (fromIntegral numBytes) <> BB.byteString bytes
        build numBytes bytes | numBytes <= 65535 = BB.word8 0xd1 <> BB.word16BE (fromIntegral numBytes) <> BB.byteString bytes
        build numBytes bytes | numBytes <= 2147483647 = BB.word8 0xd2 <> BB.word32BE (fromIntegral numBytes) <> BB.byteString bytes
        build _ _ = error "Cannot encode strings which encoded using utf8 are longer than 2147483647 bytes"
    step (PList [] cont) = BB.word8 0x90 <> step cont
    step (PList elts cont) = build elts 0 mempty
      where
        build :: [PackStream] -> Int -> PackStream -> BB.Builder
        build (b:bs) numElts folded = build bs (numElts + 1) (folded `mappend` b)
        build [] numElts folded | numElts <= 15 = BB.word8 (0x90 .|. (fromIntegral numElts)) <> step (run folded cont)
        build [] numElts folded | numElts <= 255 = BB.word8 0xd4 <> BB.word8 (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded | numElts <= 65535 = BB.word8 0xd5 <> BB.word16BE (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded | numElts <= 2147483647 = BB.word8 0xd6 <> BB.word32BE (fromIntegral numElts) <> step (run folded cont)
        build [] _ _ = error "Cannot encode more than 2147483647 elements in a list"
    step (PMap [] cont) = BB.word8 0xa0 <> step cont
    step (PMap elts cont) = build elts 0 mempty
      where
        build :: [(PackStream, PackStream)] -> Int -> PackStream -> BB.Builder
        build ((key, value):bs) numElts folded = build bs (numElts + 1) (folded `mappend` key `mappend` value)
        build [] numElts folded | numElts <= 15 = BB.word8 (0xa0 .|. (fromIntegral numElts)) <> step (run folded cont)
        build [] numElts folded | numElts <= 255 = BB.word8 0xd8 <> BB.word8 (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded | numElts <= 65535 = BB.word8 0xd9 <> BB.word16BE (fromIntegral numElts) <> step (run folded cont)
        build [] numElts folded | numElts <= 2147483647 = BB.word8 0xda <> BB.word32BE (fromIntegral numElts) <> step (run folded cont)
        build [] _ _ = error "Cannot encode more than 2147483647 entries in a map"
    step (PStructure sig [] cont) = BB.word8 0xb0 <> BB.word8 (signatureByte sig) <> step cont
    step (PStructure sig elts cont) = build elts 0 mempty
      where
        build :: [PackStream] -> Int -> PackStream -> BB.Builder
        build (b:bs) numElts folded = build bs (numElts + 1) (folded `mappend` b)
        build [] numElts folded | numElts <= 15 = BB.word8 (0xb0 .|. (fromIntegral numElts)) <> BB.word8 (signatureByte sig) <> step (run folded cont)
        build [] numElts folded | numElts <= 255 = BB.word8 0xdc <> BB.word8 (fromIntegral numElts) <> BB.word8 (signatureByte sig) <> step (run folded cont)
        build [] numElts folded | numElts <= 65535 = BB.word8 0xdd <> BB.word16BE (fromIntegral numElts) <> BB.word8 (signatureByte sig) <> step (run folded cont)
        build [] _ _ = error "Cannot encode more than 65535 elements in a structure"
    step PEnd = mempty

{-# INLINE null #-}
null :: PackStream
null = PackStream $ PNull

{-# INLINE false #-}
false :: PackStream
false = PackStream $ PFalse

{-# INLINE true #-}
true :: PackStream
true = PackStream $ PTrue

{-# INLINE float64 #-}
float64 :: Double -> PackStream
float64 value = PackStream $ PFloat64 value

{-# INLINE tinyInt #-}
tinyInt :: Int8 -> PackStream
tinyInt value = PackStream $ PTinyInt value

{-# INLINE int8 #-}
int8 :: Int8 -> PackStream
int8 value = PackStream $ PInt8 value

{-# INLINE int16 #-}
int16 :: Int16 -> PackStream
int16 value = PackStream $ PInt16 value

{-# INLINE int32 #-}
int32 :: Int32 -> PackStream
int32 value = PackStream $ PInt32 value

{-# INLINE int64 #-}
int64 :: Int64 -> PackStream
int64 value = PackStream $ PInt64 value

int :: Int -> PackStream
int value =
  if fromIntegral value8 == value64 then
    tinyInt value8
  else if fromIntegral value16 == value64 then
    int16 value16
  else if fromIntegral value32 == value64 then
    int32 value32
  else
    int64 value64
  where
    value8 = fromIntegral value :: Int8
    value16 = fromIntegral value :: Int16
    value32 = fromIntegral value :: Int32
    value64 = fromIntegral value :: Int64

{-# INLINE text #-}
text :: T.Text -> PackStream
text value = PackStream $ PText value

{-# INLINE string #-}
string :: String -> PackStream
string value = PackStream $ PText (T.pack value)

{-# INLINE list #-}
list :: [PackStream] -> PackStream
list elts = PackStream $ PList elts

{-# INLINE (@@) #-}
(@@) ::  PackStream -> PackStream -> (PackStream, PackStream)
infix 0 @@
l @@ r = (l, r)

{-# INLINE map #-}
map :: [(PackStream, PackStream)] -> PackStream
map elts = PackStream $ PMap elts

{-# INLINE structure #-}
structure :: Signature -> [PackStream] -> PackStream
structure sig elts = PackStream $ PStructure sig elts
