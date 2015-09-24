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
  putText,
  getText,
  putVector,
  getVector,
  streamList,
  unStreamList,
  putEntry,
  getEntry,
  putMap,
  getMap,
  streamMap,
  unStreamMap
) where

import           Codec.Packstream.Expect
import           Codec.Packstream.Marker
import           Control.Applicative
import           Control.Monad
import           Data.Binary             (put)
import qualified Data.ByteString         as B
import qualified Data.Binary.Get         as G
import qualified Data.Binary.IEEE754     as IEEE754
import qualified Data.Binary.Put         as P
import           Data.Bits
import           Data.Int
import qualified Data.Vector             as V
import qualified Data.Text               as T
import           Data.Text.Encoding      (encodeUtf8, decodeUtf8)
import           Data.Word

{-# INLINE putNull #-}
putNull :: P.Put
putNull = put _NULL

{-# INLINE getNull #-}
getNull :: G.Get ()
getNull = expect _NULL

{-# INLINE putBool #-}
putBool :: Bool -> P.Put
putBool b = put marker where marker = if b then _TRUE else _FALSE

{-# INLINE getBool #-}
getBool :: G.Get Bool
getBool =
  False <$ expect _FALSE <|>
  True  <$ expect _TRUE

{-# INLINE putFloat64 #-}
putFloat64 :: Double -> P.Put
putFloat64 d = put _FLOAT_64 *> IEEE754.putFloat64be d

{-# INLINE getFloat64 #-}
getFloat64 :: G.Get Double
getFloat64 = expect _FLOAT_64 *> IEEE754.getFloat64be

{-# INLINE putTinyInt #-}
putTinyInt :: Int8 -> Maybe P.Put
putTinyInt i8
  | isTinyInt i8 = Just . P.putWord8 . fromIntegral $ i8
  | otherwise    = Nothing

{-# INLINE getTinyInt #-}
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

{-# INLINE putInt8 #-}
putInt8 :: Int8 -> P.Put
putInt8 i8 = put _INT_8 *> P.putWord8 (fromIntegral i8)

{-# INLINE getInt8 #-}
getInt8 :: G.Get Int8
getInt8 = expect _INT_8 *> (fromIntegral <$> G.getWord8)

{-# INLINE putInt16 #-}
putInt16 :: Int16 -> P.Put
putInt16 i16 = put _INT_16 *> P.putWord16be (fromIntegral i16)

{-# INLINE getInt16 #-}
getInt16 :: G.Get Int16
getInt16 = expect _INT_16 *> (fromIntegral <$> G.getWord16be)

{-# INLINE putInt32 #-}
putInt32 :: Int32 -> P.Put
putInt32 i32 = put _INT_32 *> P.putWord32be (fromIntegral i32)

{-# INLINE getInt32 #-}
getInt32 :: G.Get Int32
getInt32 = expect _INT_32 *> (fromIntegral <$> G.getWord32be)

{-# INLINE putInt64 #-}
putInt64 :: Int64 -> P.Put
putInt64 i64 = put _INT_64 *> P.putWord64be (fromIntegral i64)

{-# INLINE getInt64 #-}
getInt64 :: G.Get Int64
getInt64 = expect _INT_64 *> (fromIntegral <$> G.getWord64be)

{-# INLINEABLE putText #-}
putText :: T.Text -> P.Put
putText txt | T.null txt =  put _TINY_TEXT_FIRST
putText txt = case B.length bytes of
  size | size <= 15         -> putTiny _TINY_TEXT_FIRST size $ \put0 -> put0 *> P.putByteString bytes
  size | size <= 255        -> put _TEXT_8 *> P.putWord8 (fromIntegral size) *> P.putByteString bytes
  size | size <= 65535      -> put _TEXT_16 *> P.putWord16be (fromIntegral size) *> P.putByteString bytes
  size | size <= 2147483647 -> put _TEXT_32 *> P.putWord32be (fromIntegral size) *> P.putByteString bytes
  _                         -> error "Cannot encode strings which encoded using utf8 are longer than 2147483647 bytes"
  where
    bytes = encodeUtf8 txt

{-# INLINEABLE getText #-}
getText :: G.Get T.Text
getText = fmap decodeUtf8 bytes
  where
    bytes = getTiny _TINY_TEXT_FIRST G.getByteString
         <|> (expect _TEXT_8 *> liftM fromIntegral G.getWord8 >>= G.getByteString)
         <|> (expect _TEXT_16 *> liftM fromIntegral G.getWord16be >>= G.getByteString)
         <|> (expect _TEXT_32 *> liftM fromIntegral G.getWord32be >>= G.getByteString)

{-# INLINEABLE putVector #-}
putVector :: V.Vector P.Put -> P.Put
putVector vec = case V.length vec of
    numElts | numElts == 0          -> put _TINY_LIST_FIRST
    numElts | numElts <= 15         -> putTiny _TINY_LIST_FIRST numElts $ \put0 -> V.foldl (*>) put0 vec
    numElts | numElts <= 255        -> V.foldl (*>) (put _LIST_8 *> P.putWord8 (fromIntegral numElts)) vec
    numElts | numElts <= 65535      -> V.foldl (*>) (put _LIST_16 *> P.putWord16be (fromIntegral numElts)) vec
    numElts | numElts <= 2147483647 -> V.foldl (*>) (put _LIST_32 *> P.putWord32be (fromIntegral numElts)) vec
    _                               -> V.foldl (*>) (put _LIST_STREAM) vec *> put _END_OF_STREAM

{-# INLINEABLE getVector #-}
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

{-# INLINEABLE streamList #-}
streamList :: [P.Put] -> P.Put
streamList elts = foldl (*>) (put _LIST_STREAM) elts *> put _END_OF_STREAM

{-# INLINEABLE unStreamList #-}
unStreamList :: G.Get a -> G.Get [a]
unStreamList getElt = expect _LIST_STREAM *> unStreamElts getElt
  where
    unStreamElts get = (Just <$> get <|> Nothing <$ expect _END_OF_STREAM) >>= \case
      Just elt -> liftM (elt :) $ unStreamElts get
      Nothing  -> return []

{-# INLINE putEntry #-}
putEntry :: P.Put -> P.Put -> P.Put
putEntry k v = k *> v

{-# INLINE getEntry #-}
getEntry :: G.Get a -> G.Get b -> G.Get (a, b)
getEntry getKey getValue = do
  k <- getKey
  v <- getValue
  return (k, v)

{-# INLINEABLE putMap #-}
putMap :: V.Vector P.Put -> P.Put
putMap vec = case V.length vec of
    numEntries | numEntries == 0          -> put _TINY_MAP_FIRST
    numEntries | numEntries <= 15         -> putTiny _TINY_MAP_FIRST numEntries $ \put0 -> V.foldl (*>) put0 vec
    numEntries | numEntries <= 255        -> V.foldl (*>) (put _MAP_8 *> P.putWord8 (fromIntegral numEntries)) vec
    numEntries | numEntries <= 65535      -> V.foldl (*>) (put _MAP_16 *> P.putWord16be (fromIntegral numEntries)) vec
    numEntries | numEntries <= 2147483647 -> V.foldl (*>) (put _MAP_32 *> P.putWord32be (fromIntegral numEntries)) vec
    _                                     -> V.foldl (*>) (put _MAP_STREAM) vec *> put _END_OF_STREAM

{-# INLINEABLE getMap #-}
getMap :: G.Get (a, b) -> G.Get (V.Vector (a, b))
getMap getPair = getTinyMap <|> getMap8 <|> getMap16 <|> getMap32 <|> getMapStream
  where
    getTinyMap = getTiny _TINY_MAP_FIRST $ \times -> V.replicateM times getPair
    getMap8 = expect _MAP_8 >> liftM fromIntegral G.getWord8 >>= \times -> V.replicateM times getPair
    getMap16 = expect _MAP_16 >> liftM fromIntegral G.getWord16be >>= \times -> V.replicateM times getPair
    getMap32 = expect _MAP_32 >> liftM fromIntegral G.getWord32be >>= \times -> V.replicateM times getPair
    getMapStream = expect _MAP_STREAM *> getMapStreamEntries getPair
    getMapStreamEntries get = (Just <$> get <|> Nothing <$ expect _END_OF_STREAM) >>= \case
      Just v  -> liftM (V.cons v) $ getMapStreamEntries get
      Nothing -> return V.empty

{-# INLINEABLE streamMap #-}
streamMap :: [P.Put] -> P.Put
streamMap elts = foldl (*>) (put _MAP_STREAM) elts *> put _END_OF_STREAM

{-# INLINEABLE unStreamMap #-}
unStreamMap :: G.Get (a, b) -> G.Get [(a, b)]
unStreamMap getPair = expect _MAP_STREAM *> unStreamEntries getPair
  where
    unStreamEntries get = (Just <$> get <|> Nothing <$ expect _END_OF_STREAM) >>= \case
      Just elt -> liftM (elt :) $ unStreamEntries get
      Nothing  -> return []

{-# INLINE putTiny #-}
putTiny :: Marker -> Int -> (P.Put -> P.Put) -> P.Put
putTiny marker times cont = cont $ P.putWord8 $ markerByte marker .|. fromIntegral times

{-# INLINE getTiny #-}
getTiny :: Marker -> (Int -> G.Get a) -> G.Get a
getTiny mask getElt = G.getWord8 >>= \w8 -> if markerByte mask == hi w8 then getElt (fromIntegral . lo $ w8) else empty

-- {-# INLINEABLE putStruct #-}
-- putStruct :: Signature -> V.Vector P.Put -> P.Put
-- putMap vec = case V.length vec of
--     numEntries | numEntries == 0          -> put _TINY_MAP_FIRST
--     numEntries | numEntries <= 15         -> putTiny _TINY_MAP_FIRST numEntries $ \put0 -> V.foldl (*>) put0 vec
--     numEntries | numEntries <= 255        -> V.foldl (*>) (put _MAP_8 *> P.putWord8 (fromIntegral numEntries)) vec
--     numEntries | numEntries <= 65535      -> V.foldl (*>) (put _MAP_16 *> P.putWord16be (fromIntegral numEntries)) vec
--     numEntries | numEntries <= 2147483647 -> V.foldl (*>) (put _MAP_32 *> P.putWord32be (fromIntegral numEntries)) vec
--     _                                     -> V.foldl (*>) (put _MAP_STREAM) vec *> put _END_OF_STREAM
--
-- {-# INLINEABLE getStruct #-}
-- getStruct :: Signature -> G.Get (V.Vector (a, b))
-- getMap getPair = getTinyMap <|> getMap8 <|> getMap16 <|> getMap32 <|> getMapStream
--   where
--     getTinyMap = getTiny _TINY_MAP_FIRST $ \times -> V.replicateM times getPair
--     getMap8 = expect _MAP_8 >> liftM fromIntegral G.getWord8 >>= \times -> V.replicateM times getPair
--     getMap16 = expect _MAP_16 >> liftM fromIntegral G.getWord16be >>= \times -> V.replicateM times getPair
--     getMap32 = expect _MAP_32 >> liftM fromIntegral G.getWord32be >>= \times -> V.replicateM times getPair
--     getMapStream = expect _MAP_STREAM *> getMapStreamEntries getPair
--     getMapStreamEntries get = (Just <$> get <|> Nothing <$ expect _END_OF_STREAM) >>= \case
--       Just v  -> liftM (V.cons v) $ getMapStreamEntries get
--       Nothing -> return V.empty


-- encode :: PackStream -> BB.Builder
-- encode vs0 = step (unPackStream vs0 PEnd)
--   where
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
-- {-# INLINEABLE encodePair #-}
-- encodePair :: (PackStream, PackStream) -> BB.Builder
-- encodePair (l, r) = encode l <> encode r
