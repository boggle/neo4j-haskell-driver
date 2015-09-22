module Codec.Packstream.Coding(
  putNull,
  getNull,
  putBool,
  getBool,
  putFloat64,
  getFloat64,
  putTinyInt,
  getTinyInt,
  isTinyInt,
  putInt8,
  getInt8,
  putInt16,
  getInt16,
  putInt32,
  getInt32,
  putInt64,
  getInt64,
  putVector,
  getVector,
  streamList,
  unStreamList
) where

import           Codec.Packstream.Expect
import           Codec.Packstream.Marker
import           Control.Applicative
import           Control.Monad
import           Data.Binary             (put)
import qualified Data.Binary.Get         as G
import qualified Data.Binary.IEEE754     as IEEE754
import qualified Data.Binary.Put         as P
import           Data.Bits
import           Data.Int
import qualified Data.Vector             as V
import           Data.Word

putNull :: P.Put
putNull = put _NULL

getNull :: G.Get ()
getNull = expect _NULL

putBool :: Bool -> P.Put
putBool b = put marker where marker = if b then _TRUE else _FALSE

getBool :: G.Get Bool
getBool =
  False <$ expect _FALSE <|>
  True  <$ expect _TRUE

putFloat64 :: Double -> P.Put
putFloat64 d = put _FLOAT_64 *> IEEE754.putFloat64be d

getFloat64 :: G.Get Double
getFloat64 = expect _FLOAT_64 *> IEEE754.getFloat64be

putTinyInt :: Int8 -> Maybe P.Put
putTinyInt i8
  | isTinyInt i8 = Just . P.putWord8 . fromIntegral $ i8
  | otherwise    = Nothing

getTinyInt :: G.Get Int8
getTinyInt = (fromIntegral <$> G.getWord8) >>= \i8 -> if isTinyInt i8 then return i8 else empty

{-# INLINE isTinyInt #-}
isTinyInt :: Int8 -> Bool
isTinyInt i8 = positivelyTiny || negativelyTiny
  where
    positivelyTiny = not $ testBit w8 7
    negativelyTiny = markerByte _NEG_TINY_INT_FIRST == hi w8
    w8 = fromIntegral i8

{-# INLINE hi #-}
hi :: Word8 -> Word8
hi value = value .&. 0xf0

{-# INLINE lo #-}
lo :: Word8 -> Word8
lo value = value .&. 0x0f

putInt8 :: Int8 -> P.Put
putInt8 i8 = put _INT_8 *> P.putWord8 (fromIntegral i8)

getInt8 :: G.Get Int8
getInt8 = expect _INT_8 *> (fromIntegral <$> G.getWord8)

putInt16 :: Int16 -> P.Put
putInt16 i16 = put _INT_16 *> P.putWord16be (fromIntegral i16)

getInt16 :: G.Get Int16
getInt16 = expect _INT_16 *> (fromIntegral <$> G.getWord16be)

putInt32 :: Int32 -> P.Put
putInt32 i32 = put _INT_32 *> P.putWord32be (fromIntegral i32)

getInt32 :: G.Get Int32
getInt32 = expect _INT_32 *> (fromIntegral <$> G.getWord32be)

putInt64 :: Int64 -> P.Put
putInt64 i64 = put _INT_64 *> P.putWord64be (fromIntegral i64)

getInt64 :: G.Get Int64
getInt64 = expect _INT_64 *> (fromIntegral <$> G.getWord64be)

putVector :: V.Vector P.Put -> P.Put
putVector vec = case V.length vec of
    numElts | numElts == 0          -> put _TINY_LIST_FIRST
    numElts | numElts <= 15         -> putTiny _TINY_LIST_FIRST numElts $ \put0 -> V.foldl (*>) put0 vec
    numElts | numElts <= 255        -> V.foldl (*>) (put _LIST_8 *> P.putWord8 (fromIntegral numElts)) vec
    numElts | numElts <= 65535      -> V.foldl (*>) (put _LIST_16 *> P.putWord16be (fromIntegral numElts)) vec
    numElts | numElts <= 2147483647 -> V.foldl (*>) (put _LIST_32 *> P.putWord32be (fromIntegral numElts)) vec
    _                               -> V.foldl (*>) (put _LIST_STREAM) vec *> put _END_OF_STREAM

getVector :: G.Get a -> G.Get (V.Vector a)
getVector getElt = getTinyVector <|> getVector8 <|> getVector16 <|> getVector32 <|> getVectorStream
  where
    getTinyVector = getTiny _TINY_LIST_FIRST $ \times -> V.replicateM times getElt
    getVector8 = expect _LIST_8 >> liftM fromIntegral G.getWord8 >>= \times -> V.replicateM times getElt
    getVector16 = expect _LIST_16 >> liftM fromIntegral G.getWord16be >>= \times -> V.replicateM times getElt
    getVector32 = expect _LIST_32 >> liftM fromIntegral G.getWord32be >>= \times -> V.replicateM times getElt
    getVectorStream = expect _LIST_STREAM *> getVectorStreamElts getElt
    getVectorStreamElts get = (Just <$> get <|> Nothing <$ expect _END_OF_STREAM) >>= \case
      Just v  -> liftM (V.cons v) $ getVectorStreamElts get
      Nothing -> return V.empty

streamList :: [P.Put] -> P.Put
streamList elts = foldl (*>) (put _LIST_STREAM) elts *> put _END_OF_STREAM

unStreamList :: G.Get a -> G.Get [a]
unStreamList getElt = expect _LIST_STREAM *> unStreamElts getElt
  where
    unStreamElts get = (Just <$> get <|> Nothing <$ expect _END_OF_STREAM) >>= \case
      Just elt -> liftM (elt :) $ unStreamElts get
      Nothing  -> return []

{-# INLINE putTiny #-}
putTiny :: Marker -> Int -> (P.Put -> P.Put) -> P.Put
putTiny marker times cont = cont $ P.putWord8 $ markerByte marker .|. fromIntegral times

{-# INLINE getTiny #-}
getTiny :: Marker -> (Int -> G.Get a) -> G.Get a
getTiny mask getElt = G.getWord8 >>= \w8 -> if markerByte mask == hi w8 then getElt (fromIntegral . lo $ w8) else empty

-- encode :: PackStream -> BB.Builder
-- encode vs0 = step (unPackStream vs0 PEnd)
--   where
--     step (PText value cont) = build (B.length encodedBytes) encodedBytes
--       where
--         build :: Int -> B.ByteString -> BB.Builder
--         build numBytes _ | numBytes == 0 = mark _TINY_TEXT_FIRST cont
--         build numBytes bytes | numBytes <= 15 = wrap (_TINY_TEXT_FIRST .|. (fromIntegral numBytes)) cont $ BB.byteString bytes
--         build numBytes bytes | numBytes <= 255 = wrap _TEXT_8 cont $ BB.word8 (fromIntegral numBytes) <> BB.byteString bytes
--         build numBytes bytes | numBytes <= 65535 = wrap _TEXT_64 cont $ BB.word64BE (fromIntegral numBytes) <> BB.byteString bytes
--         build numBytes bytes | numBytes <= 2147483647 = wrap _TEXT_32 cont $ BB.word32BE (fromIntegral numBytes) <> BB.byteString bytes
--         build _ _ = error "Cannot encode strings which encoded using utf8 are longer than 2147483647 bytes"
--         encodedBytes = TE.encodeUtf8 value
--     step (PList [] cont) = mark _TINY_LIST_FIRST cont
--     step (PList elts cont) = build elts 0 mempty
--       where
--         build :: [PackStream] -> Int -> PackStream -> BB.Builder
--         build (b:bs) numElts folded = build bs (numElts + 1) (folded `mappend` b)
--         build [] numElts folded | numElts <= 15 = mark (_TINY_LIST_FIRST  .|. (fromIntegral numElts)) $ unPackStream folded cont
--         build [] numElts folded | numElts <= 255 = wrap _LIST_8 (unPackStream folded cont) $ BB.word8 (fromIntegral numElts)
--         build [] numElts folded | numElts <= 65535 = wrap _LIST_64 (unPackStream folded cont) $ BB.word64BE (fromIntegral numElts)
--         build [] numElts folded | numElts <= 2147483647 = wrap _LIST_32 (unPackStream folded cont) $ BB.word32BE (fromIntegral numElts)
--         build bs _ folded = stream _LIST_STREAM cont $ encode (foldl (<>) folded bs)
--     step (PListStream elts cont) = stream _LIST_STREAM cont $ foldl (<>) mempty (PRE.map encode elts)
--     step (PMap [] cont) = BB.word8 _TINY_MAP_FIRST <> step cont
--     step (PMap elts cont) = build elts 0 mempty
--       where
--         build :: [(PackStream, PackStream)] -> Int -> PackStream -> BB.Builder
--         build ((key, value):bs) numElts folded = build bs (numElts + 1) (folded `mappend` key `mappend` value)
--         build [] numElts folded | numElts <= 15 = mark (_TINY_MAP_FIRST .|. (fromIntegral numElts)) $ unPackStream folded cont
--         build [] numElts folded | numElts <= 255 = wrap _MAP_8 (unPackStream folded cont) $ BB.word8 (fromIntegral numElts)
--         build [] numElts folded | numElts <= 65535 = wrap _MAP_64 (unPackStream folded cont) $ BB.word64BE (fromIntegral numElts)
--         build [] numElts folded | numElts <= 2147483647 = wrap _MAP_32 (unPackStream folded cont) $ BB.word32BE (fromIntegral numElts)
--         build bs _ folded = stream _MAP_STREAM (unPackStream folded cont) $ foldl (<>) (encode folded) (PRE.map encodePair bs)
--     step (PMapStream elts cont) = stream _MAP_STREAM cont $ foldl (<>) mempty (PRE.map encodePair elts)
--     step (PStructure sig [] cont) = BB.word8 0xb0 <> BB.word8 (signatureByte sig) <> step cont
--     step (PStructure sig elts cont) = build elts 0 mempty
--       where
--         build :: [PackStream] -> Int -> PackStream -> BB.Builder
--         build (b:bs) numElts folded = build bs (numElts + 1) (folded `mappend` b)
--         build [] numElts folded | numElts <= 15 = wrap (_TINY_STRUCT_FIRST .|. (fromIntegral numElts)) (unPackStream folded cont) $ BB.word8 (signatureByte sig)
--         build [] numElts folded | numElts <= 255 = wrap _STRUCT_8 (unPackStream folded cont) $ BB.word8 (fromIntegral numElts) <> BB.word8 (signatureByte sig)
--         build [] numElts folded | numElts <= 65535 = wrap _STRUCT_64 (unPackStream folded cont) $ BB.word64BE (fromIntegral numElts) <> BB.word8 (signatureByte sig)
--         build [] _ _ = error "Cannot encode more than 65535 elements in a structure"
--     step PEnd = mempty
--     write cont builder = builder <> step cont
--     mark marker cont = write cont $ BB.word8 marker
--     wrap marker cont payload = write cont $ BB.word8 marker <> payload
--     stream marker cont payload = wrap marker cont $ payload <> (BB.word8 _END_OF_STREAM)
--
-- {-# INLINE encodePair #-}
-- encodePair :: (PackStream, PackStream) -> BB.Builder
-- encodePair (l, r) = encode l <> encode r
