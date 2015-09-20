module Codec.Bolt.Value(
  BoltValue,
  BValue(..),
  pack,
  unpack
)

where

import qualified Codec.Packstream.Value as PSV
import           Control.Applicative
import           Data.Binary
import           Data.Int
import           Data.Maybe
import qualified Data.Vector as V

data BValue = BNull
            | BBool                  !Bool
            | BDouble {-# UNPACK #-} !Double
            | BInt8   {-# UNPACK #-} !Int8
            | BInt16  {-# UNPACK #-} !Int16
            | BInt32  {-# UNPACK #-} !Int32
            | BInt64  {-# UNPACK #-} !Int64
            | BVector {-# UNPACK #-} !(V.Vector BValue)
            | BList                  [BValue]
            deriving (Show)

instance Binary BValue where
  put = putValue
  get = getValue

putValue :: BValue -> Put
putValue = \case
  BNull       -> PSV.putNull
  BBool b     -> PSV.putBool b
  BDouble d   -> PSV.putFloat64 d
  BInt8 i8    -> fromMaybe (PSV.putInt8 i8) (PSV.putTinyInt i8)
  BInt16 i16  -> PSV.putInt16 i16
  BInt32 i32  -> PSV.putInt32 i32
  BInt64 i64  -> PSV.putInt64 i64
  BVector vec -> PSV.putVector $ V.map putValue vec
  BList lst   -> PSV.streamList $ map putValue lst

getValue :: Get BValue
getValue =
      BNull   <$  PSV.getNull
  <|> BBool   <$> PSV.getBool
  <|> BDouble <$> PSV.getFloat64
  <|> BInt8   <$> PSV.getTinyInt
  <|> BInt8   <$> PSV.getInt8
  <|> BInt16  <$> PSV.getInt16
  <|> BInt32  <$> PSV.getInt32
  <|> BInt64  <$> PSV.getInt64
  <|> BVector <$> PSV.getVector getValue
  <|> BList   <$> PSV.unStreamList getValue

class BoltValue a where
  pack :: a -> BValue
  unpack :: BValue -> Maybe a

instance BoltValue BValue where
  pack = id
  unpack = Just

instance BoltValue a => BoltValue (Maybe a) where
  pack = \case
    Just v  -> pack v
    Nothing -> BNull
  unpack = \case
    BNull  -> Nothing
    bvalue -> unpack bvalue

instance BoltValue Bool where
    pack = BBool
    unpack = \case
      BBool b -> Just b
      _       -> Nothing

instance BoltValue Double where
    pack = BDouble
    unpack = \case
      BDouble d -> Just d
      _         -> Nothing

instance BoltValue Int8 where
    pack = BInt8
    unpack = \case
      BInt8 i8 -> Just i8
      _        -> Nothing

instance BoltValue Int16 where
    pack i16
      | i16 == fromIntegral i8 = BInt8 i8
      | otherwise              = BInt16 i16
      where
        i8 = fromIntegral i16 :: Int8
    unpack = \case
      BInt8 i8   -> Just (fromIntegral i8)
      BInt16 i16 -> Just i16
      BInt32 i32 -> let i16 = fromIntegral i32 :: Int16 in if fromIntegral i16 == i32 then Just i16 else Nothing
      BInt64 i64 -> let i16 = fromIntegral i64 :: Int16 in if fromIntegral i16 == i64 then Just i16 else Nothing
      _          -> Nothing

instance BoltValue Int32 where
    pack i32
      | i32 == fromIntegral i8  = BInt8 i8
      | i32 == fromIntegral i16 = BInt16 i16
      | otherwise               = BInt32 i32
      where
        i8 = fromIntegral i32 :: Int8
        i16 = fromIntegral i32 :: Int16
    unpack = \case
      BInt8 i8   -> Just (fromIntegral i8)
      BInt16 i16 -> Just (fromIntegral i16)
      BInt32 i32 -> Just i32
      BInt64 i64 -> let i32 = fromIntegral i64 :: Int32 in if fromIntegral i32 == i64 then Just i32 else Nothing
      _          -> Nothing

instance BoltValue Int64 where
    pack i64
      | i64 == fromIntegral i8  = BInt8 i8
      | i64 == fromIntegral i16 = BInt16 i16
      | i64 == fromIntegral i32 = BInt32 i32
      | otherwise               = BInt64 i64
      where
        i8 = fromIntegral i64 :: Int8
        i16 = fromIntegral i64 :: Int16
        i32 = fromIntegral i64 :: Int32
    unpack = \case
      BInt8 i8   -> Just (fromIntegral i8)
      BInt16 i16 -> Just (fromIntegral i16)
      BInt32 i32 -> Just (fromIntegral i32)
      BInt64 i64 -> Just i64
      _          -> Nothing

instance BoltValue Int where
    pack int
      | int == fromIntegral i8  = BInt8 i8
      | int == fromIntegral i16 = BInt16 i16
      | int == fromIntegral i32 = BInt32 i32
      | otherwise               = BInt64 i64
      where
        i8 = fromIntegral int :: Int8
        i16 = fromIntegral int :: Int16
        i32 = fromIntegral int :: Int32
        i64 = fromIntegral int :: Int64
    unpack = \case
      BInt8 i8   -> Just (fromIntegral i8)
      BInt16 i16 -> Just (fromIntegral i16)
      BInt32 i32 -> let int = fromIntegral i32 :: Int in
                    if fromIntegral int == i32 then Just int else Nothing
      BInt64 i64 -> let int = fromIntegral i64 :: Int in
                    if fromIntegral int == i64 then Just int else Nothing
      _          -> Nothing

instance BoltValue a => BoltValue (V.Vector a) where
  pack vec = BVector $ V.map pack vec
  unpack = \case
    BVector vec -> mapM unpack vec
    _           -> Nothing

instance BoltValue a => BoltValue [a] where
  pack lst = BList $ map pack lst
  unpack = \case
    BList lst -> mapM unpack lst
    _         -> Nothing
--
-- newtype Value = MkValue { valuePackStream :: PE.PackStream }
--
-- type ValueEncoder d = d -> Value
--
-- class ValueEncodable d where
--   encodeValue :: ValueEncoder d
--
-- data Val = forall a. (ValueEncodable a) => Val a
--
-- newtype StreamedValue v = MkStreamedValue { streamedValue :: v }
--
-- class StreamableValue d where
--   streamed :: d -> StreamedValue d
--
-- instance ValueEncodable d => StreamableValue ([] d) where
--   streamed = MkStreamedValue
--
-- instance ValueEncodable d => StreamableValue (M.Map String d) where
--   streamed = MkStreamedValue
--
-- instance ValueEncodable d => StreamableValue (M.Map T.Text d) where
--   streamed = MkStreamedValue
--
-- instance ValueEncodable Bool where
--   encodeValue False = MkValue PE.false
--   encodeValue True = MkValue PE.true
--
-- _packValue :: ValueEncodable d => d -> PE.PackStream
-- _packValue = valuePackStream . encodeValue
--
-- instance ValueEncodable Val where
--   encodeValue (Val v) = encodeValue v
--
-- instance ValueEncodable Int8 where encodeValue = MkValue . PE.int8
-- instance ValueEncodable Int16 where encodeValue = MkValue . PE.int16
-- instance ValueEncodable Int32 where encodeValue = MkValue . PE.int32
-- instance ValueEncodable Int64 where encodeValue = MkValue . PE.int64
-- instance ValueEncodable Double where encodeValue = MkValue . PE.float64
-- instance ValueEncodable Int where encodeValue = MkValue . PE.int
--
-- instance ValueEncodable String where encodeValue = MkValue . PE.string
-- instance ValueEncodable T.Text where encodeValue = MkValue . PE.text
--
-- instance ValueEncodable d => ValueEncodable (Maybe d) where
--   encodeValue (Just d) = encodeValue d
--   encodeValue Nothing = MkValue PE.null
--
-- instance ValueEncodable d => ValueEncodable [d] where
--   encodeValue vs = MkValue . PE.list $ map _packValue vs
--
-- instance ValueEncodable d => ValueEncodable (StreamedValue [d]) where
--   encodeValue vs = MkValue . PE.listStream $ map _packValue $ streamedValue vs
--
-- instance ValueEncodable v => ValueEncodable (M.Map String v) where
--   encodeValue m = MkValue . PE.map $ M.foldrWithKey collect [] m
--     where
--       collect key val pairs = (_packValue key, _packValue val) : pairs
--
-- instance ValueEncodable v => ValueEncodable (StreamedValue (M.Map String v)) where
--   encodeValue m = MkValue . PE.map $ M.foldrWithKey collect [] $ streamedValue m
--     where
--       collect key val pairs = (_packValue key, _packValue val) : pairs
--
-- instance ValueEncodable v => ValueEncodable (M.Map T.Text v) where
--   encodeValue m = MkValue . PE.map $ M.foldrWithKey collect [] m
--     where
--       collect key val pairs = (_packValue key, _packValue val) : pairs
--
-- instance ValueEncodable v => ValueEncodable (StreamedValue (M.Map T.Text v)) where
--   encodeValue m = MkValue . PE.map $ M.foldrWithKey collect [] $ streamedValue m
--     where
--       collect key val pairs = (_packValue key, _packValue val) : pairs
