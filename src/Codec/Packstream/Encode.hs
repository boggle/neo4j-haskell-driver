module Codec.Packstream.Encode(
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
  listStream,
  map,
  mapStream,
  (@@),
  structure,
  encode,
  encodePair,
  fitsTinyInt
) where

import           Codec.Packstream.Marker
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
import           Numeric                    (showHex)
import           Prelude                    hiding (map, null)
import qualified Prelude                    as PRE

data PackStreamRep = PNull PackStreamRep
                   | PFalse PackStreamRep
                   | PTrue PackStreamRep
                   | PFloat64 Double PackStreamRep
                   | PTinyInt Int8 PackStreamRep
                   | PInt Int PackStreamRep
                   | PInt8 Int8 PackStreamRep
                   | PInt16 Int16 PackStreamRep
                   | PInt32 Int32 PackStreamRep
                   | PInt64 Int64 PackStreamRep
                   | PText T.Text PackStreamRep
                   | PList [PackStream] PackStreamRep
                   | PListStream [PackStream] PackStreamRep
                   | PMap [(PackStream, PackStream)] PackStreamRep
                   | PMapStream [(PackStream, PackStream)] PackStreamRep
                   | PStructure Signature [PackStream] PackStreamRep
                   | PEnd

newtype PackStream = PackStream { unPackStream :: PackStreamRep -> PackStreamRep }

instance Monoid PackStream where
  {-# INLINE mempty #-}
  mempty            = PackStream id
  {-# INLINE mappend #-}
  b1 `mappend` b2   = PackStream (unPackStream b1 . unPackStream b2)
  {-# INLINE mconcat #-}
  mconcat           = foldl mappend mempty

instance Show PackStream where
  show = concat . intersperse " " . PRE.map hexWord8 . L.unpack . BB.toLazyByteString . encode
    where
      hexWord8 w = pad (PRE.map toUpper (showHex w ""))
      pad cs = replicate (2 - length cs) '0' ++ cs

encode :: PackStream -> BB.Builder
encode vs0 = step (unPackStream vs0 PEnd)
  where
    step (PNull cont) = mark _NULL cont
    step (PFloat64 value cont) = wrap _FLOAT_64 cont $ BB.doubleBE value
    step (PFalse cont) = mark _FALSE cont
    step (PTrue cont) = mark _TRUE cont
    step (PTinyInt value cont) | fitsTinyInt value = write cont $ BB.int8 value
    step (PTinyInt value cont) = wrap _INT_8 cont $ BB.int8 value
    step (PInt8 value cont) = wrap _INT_8 cont $ BB.int8 value
    step (PInt16 value cont) = wrap _INT_16 cont $ BB.int16BE value
    step (PInt32 value cont) = wrap _INT_32 cont $ BB.int32BE value
    step (PInt64 value cont) = wrap _INT_64 cont $ BB.int64BE value
    step (PInt value cont) = case value of
        _ | value64 == fromIntegral value8 -> (if fitsTinyInt value8 then write else wrap _INT_8) cont $ BB.int8 value8
        _ | value64 == fromIntegral value16 -> wrap _INT_16 cont $ BB.int16BE value16
        _ | value64 == fromIntegral value32 -> wrap _INT_32 cont $ BB.int32BE value32
        _ -> wrap _INT_64 cont $ BB.int64BE value64
        where
          value64 = fromIntegral value :: Int64
          value32 = fromIntegral value :: Int32
          value16 = fromIntegral value :: Int16
          value8 = fromIntegral value :: Int8
    step (PText value cont) = build (B.length encodedBytes) encodedBytes
      where
        build :: Int -> B.ByteString -> BB.Builder
        build numBytes _ | numBytes == 0 = mark _TINY_TEXT_FIRST cont
        build numBytes bytes | numBytes <= 15 = wrap (_TINY_TEXT_FIRST .|. (fromIntegral numBytes)) cont $ BB.byteString bytes
        build numBytes bytes | numBytes <= 255 = wrap _TEXT_8 cont $ BB.word8 (fromIntegral numBytes) <> BB.byteString bytes
        build numBytes bytes | numBytes <= 65535 = wrap _TEXT_16 cont $ BB.word16BE (fromIntegral numBytes) <> BB.byteString bytes
        build numBytes bytes | numBytes <= 2147483647 = wrap _TEXT_32 cont $ BB.word32BE (fromIntegral numBytes) <> BB.byteString bytes
        build _ _ = error "Cannot encode strings which encoded using utf8 are longer than 2147483647 bytes"
        encodedBytes = TE.encodeUtf8 value
    step (PList [] cont) = mark _TINY_LIST_FIRST cont
    step (PList elts cont) = build elts 0 mempty
      where
        build :: [PackStream] -> Int -> PackStream -> BB.Builder
        build (b:bs) numElts folded = build bs (numElts + 1) (folded `mappend` b)
        build [] numElts folded | numElts <= 15 = mark (_TINY_LIST_FIRST  .|. (fromIntegral numElts)) $ unPackStream folded cont
        build [] numElts folded | numElts <= 255 = wrap _LIST_8 (unPackStream folded cont) $ BB.word8 (fromIntegral numElts)
        build [] numElts folded | numElts <= 65535 = wrap _LIST_16 (unPackStream folded cont) $ BB.word16BE (fromIntegral numElts)
        build [] numElts folded | numElts <= 2147483647 = wrap _LIST_32 (unPackStream folded cont) $ BB.word32BE (fromIntegral numElts)
        build bs _ folded = stream _LIST_STREAM cont $ encode (foldl (<>) folded bs)
    step (PListStream elts cont) = stream _LIST_STREAM cont $ foldl (<>) mempty (PRE.map encode elts)
    step (PMap [] cont) = BB.word8 _TINY_MAP_FIRST <> step cont
    step (PMap elts cont) = build elts 0 mempty
      where
        build :: [(PackStream, PackStream)] -> Int -> PackStream -> BB.Builder
        build ((key, value):bs) numElts folded = build bs (numElts + 1) (folded `mappend` key `mappend` value)
        build [] numElts folded | numElts <= 15 = mark (_TINY_MAP_FIRST .|. (fromIntegral numElts)) $ unPackStream folded cont
        build [] numElts folded | numElts <= 255 = wrap _MAP_8 (unPackStream folded cont) $ BB.word8 (fromIntegral numElts)
        build [] numElts folded | numElts <= 65535 = wrap _MAP_16 (unPackStream folded cont) $ BB.word16BE (fromIntegral numElts)
        build [] numElts folded | numElts <= 2147483647 = wrap _MAP_32 (unPackStream folded cont) $ BB.word32BE (fromIntegral numElts)
        build bs _ folded = stream _MAP_STREAM (unPackStream folded cont) $ foldl (<>) (encode folded) (PRE.map encodePair bs)
    step (PMapStream elts cont) = stream _MAP_STREAM cont $ foldl (<>) mempty (PRE.map encodePair elts)
    step (PStructure sig [] cont) = BB.word8 0xb0 <> BB.word8 (signatureByte sig) <> step cont
    step (PStructure sig elts cont) = build elts 0 mempty
      where
        build :: [PackStream] -> Int -> PackStream -> BB.Builder
        build (b:bs) numElts folded = build bs (numElts + 1) (folded `mappend` b)
        build [] numElts folded | numElts <= 15 = wrap (_TINY_STRUCT_FIRST .|. (fromIntegral numElts)) (unPackStream folded cont) $ BB.word8 (signatureByte sig)
        build [] numElts folded | numElts <= 255 = wrap _STRUCT_8 (unPackStream folded cont) $ BB.word8 (fromIntegral numElts) <> BB.word8 (signatureByte sig)
        build [] numElts folded | numElts <= 65535 = wrap _STRUCT_16 (unPackStream folded cont) $ BB.word16BE (fromIntegral numElts) <> BB.word8 (signatureByte sig)
        build [] _ _ = error "Cannot encode more than 65535 elements in a structure"
    step PEnd = mempty
    write cont builder = builder <> step cont
    mark marker cont = write cont $ BB.word8 marker
    wrap marker cont payload = write cont $ BB.word8 marker <> payload
    stream marker cont payload = wrap marker cont $ payload <> (BB.word8 _END_OF_STREAM)

{-# INLINE fitsTinyInt #-}
fitsTinyInt :: Int8 -> Bool
fitsTinyInt value = positivelyTiny || negativelyTiny
  where
    positivelyTiny = not $ testBit value 7
    negativelyTiny = (fromIntegral value) .&. _NEG_TINY_INT_FIRST == _NEG_TINY_INT_FIRST

{-# INLINE encodePair #-}
encodePair :: (PackStream, PackStream) -> BB.Builder
encodePair (l, r) = encode l <> encode r

{-# INLINE null #-}
null :: PackStream
null = PackStream PNull

{-# INLINE false #-}
false :: PackStream
false = PackStream PFalse

{-# INLINE true #-}
true :: PackStream
true = PackStream PTrue

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

{-# INLINE int #-}
int :: Int -> PackStream
int value = PackStream $ PInt value

{-# INLINE text #-}
text :: T.Text -> PackStream
text value = PackStream $ PText value

{-# INLINE string #-}
string :: String -> PackStream
string value = PackStream $ PText (T.pack value)

{-# INLINE list #-}
list :: [PackStream] -> PackStream
list elts = PackStream $ PList elts

{-# INLINE listStream #-}
listStream :: [PackStream] -> PackStream
listStream elts = PackStream $ PListStream elts

{-# INLINE (@@) #-}
(@@) ::  PackStream -> PackStream -> (PackStream, PackStream)
infix 0 @@
l @@ r = (l, r)

{-# INLINE map #-}
map :: [(PackStream, PackStream)] -> PackStream
map elts = PackStream $ PMap elts

{-# INLINE mapStream #-}
mapStream :: [(PackStream, PackStream)] -> PackStream
mapStream elts = PackStream $ PMapStream elts

{-# INLINE structure #-}
structure :: Signature -> [PackStream] -> PackStream
structure sig elts = PackStream $ PStructure sig elts
