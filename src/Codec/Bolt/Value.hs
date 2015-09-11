{-# LANGUAGE IncoherentInstances #-}
module Codec.Bolt.Value(
  Value,
  valuePackStream,
  StreamedValue,
  streamed,
  ValueEncoder,
  ValueEncodable,
  encodeValue
)

where

import qualified Codec.Packstream.Encode as PE
import           Data.Int
import qualified Data.Map                as M
import qualified Data.Text               as T

newtype Value = MkValue { valuePackStream :: PE.PackStream }

type ValueEncoder d = d -> Value

class ValueEncodable d where
  encodeValue :: ValueEncoder d

newtype StreamedValue v = MkStreamedValue { streamedValue :: v }

class StreamableValue d where
  streamed :: d -> StreamedValue d

instance ValueEncodable d => StreamableValue ([] d) where
  streamed ds = MkStreamedValue ds

instance ValueEncodable d => StreamableValue (M.Map String d) where
  streamed = MkStreamedValue

instance ValueEncodable d => StreamableValue (M.Map T.Text d) where
  streamed = MkStreamedValue

instance ValueEncodable Bool where
  encodeValue False = MkValue PE.false
  encodeValue True = MkValue PE.true

instance ValueEncodable Int8 where encodeValue = MkValue . PE.int8
instance ValueEncodable Int16 where encodeValue = MkValue . PE.int16
instance ValueEncodable Int32 where encodeValue = MkValue . PE.int32
instance ValueEncodable Int64 where encodeValue = MkValue . PE.int64

instance ValueEncodable Double where encodeValue = MkValue . PE.float64

instance ValueEncodable Int where encodeValue = MkValue . PE.int

instance ValueEncodable String where encodeValue = MkValue . PE.string
instance ValueEncodable T.Text where encodeValue = MkValue . PE.text

_packValue :: ValueEncodable d => d -> PE.PackStream
_packValue = valuePackStream . encodeValue

instance ValueEncodable d => ValueEncodable (Maybe d) where
  encodeValue (Just d) = encodeValue d
  encodeValue Nothing = MkValue PE.null

instance ValueEncodable d => ValueEncodable [d] where
  encodeValue vs = MkValue . PE.list $ map _packValue vs

instance ValueEncodable d => ValueEncodable (StreamedValue [d]) where
  encodeValue vs = MkValue . PE.listStream $ map _packValue $ streamedValue vs

instance ValueEncodable d => ValueEncodable (M.Map String d) where
  encodeValue m = MkValue . PE.map $ elts
    where
      elts = M.foldrWithKey collect [] m
      collect k v pairs = (PE.string k PE.@@ _packValue v) : pairs

instance ValueEncodable d => ValueEncodable (StreamedValue (M.Map String d)) where
  encodeValue m = MkValue . PE.mapStream $ elts
    where
      elts = M.foldrWithKey collect [] $ streamedValue m
      collect k v pairs = (PE.string k PE.@@ _packValue v) : pairs

instance ValueEncodable d => ValueEncodable (M.Map T.Text d) where
  encodeValue m = MkValue . PE.map $ elts
    where
      elts = M.foldrWithKey collect [] m
      collect k v pairs = (PE.text k PE.@@ _packValue v) : pairs

instance ValueEncodable d => ValueEncodable (StreamedValue (M.Map T.Text d)) where
  encodeValue m = MkValue . PE.mapStream $ elts
    where
      elts = M.foldrWithKey collect [] $ streamedValue m
      collect k v pairs = (PE.text k PE.@@ _packValue v) : pairs
