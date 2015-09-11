{-# LANGUAGE IncoherentInstances #-}
module Codec.Bolt.Value(
  Value,
  valuePackStream,
  ValueEncoder,
  ValueEncodable,
  encodeValue,
  StreamedValue,
  streamed
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
  streamed = MkStreamedValue

instance ValueEncodable d => StreamableValue (M.Map String d) where
  streamed = MkStreamedValue

instance ValueEncodable d => StreamableValue (M.Map T.Text d) where
  streamed = MkStreamedValue

instance ValueEncodable Bool where
  encodeValue False = MkValue PE.false
  encodeValue True = MkValue PE.true

_packValue :: ValueEncodable d => d -> PE.PackStream
_packValue = valuePackStream . encodeValue

instance ValueEncodable Int8 where encodeValue = MkValue . PE.int8
instance ValueEncodable Int16 where encodeValue = MkValue . PE.int16
instance ValueEncodable Int32 where encodeValue = MkValue . PE.int32
instance ValueEncodable Int64 where encodeValue = MkValue . PE.int64
instance ValueEncodable Double where encodeValue = MkValue . PE.float64
instance ValueEncodable Int where encodeValue = MkValue . PE.int

instance ValueEncodable String where encodeValue = MkValue . PE.string
instance ValueEncodable T.Text where encodeValue = MkValue . PE.text

instance ValueEncodable d => ValueEncodable (Maybe d) where
  encodeValue (Just d) = encodeValue d
  encodeValue Nothing = MkValue PE.null

instance ValueEncodable d => ValueEncodable [d] where
  encodeValue vs = MkValue . PE.list $ map _packValue vs

instance ValueEncodable d => ValueEncodable (StreamedValue [d]) where
  encodeValue vs = MkValue . PE.listStream $ map _packValue $ streamedValue vs

instance ValueEncodable v => ValueEncodable (M.Map String v) where
  encodeValue m = MkValue . PE.map $ (M.foldrWithKey collect [] m)
    where
      collect :: ValueEncodable v => String -> v -> [(PE.PackStream, PE.PackStream)] -> [(PE.PackStream, PE.PackStream)]
      collect key val pairs = (_packValue key, _packValue val) : pairs

instance ValueEncodable v => ValueEncodable (StreamedValue (M.Map String v)) where
  encodeValue m = MkValue . PE.map $ (M.foldrWithKey collect [] $ streamedValue m)
    where
      collect :: ValueEncodable v => String -> v -> [(PE.PackStream, PE.PackStream)] -> [(PE.PackStream, PE.PackStream)]
      collect key val pairs = (_packValue key, _packValue val) : pairs

instance ValueEncodable v => ValueEncodable (M.Map T.Text v) where
  encodeValue m = MkValue . PE.map $ (M.foldrWithKey collect [] m)
    where
      collect :: ValueEncodable v => T.Text -> v -> [(PE.PackStream, PE.PackStream)] -> [(PE.PackStream, PE.PackStream)]
      collect key val pairs = (_packValue key, _packValue val) : pairs

instance ValueEncodable v => ValueEncodable (StreamedValue (M.Map T.Text v)) where
  encodeValue m = MkValue . PE.map $ (M.foldrWithKey collect [] $ streamedValue m)
    where
      collect :: ValueEncodable v => T.Text -> v -> [(PE.PackStream, PE.PackStream)] -> [(PE.PackStream, PE.PackStream)]
      collect key val pairs = (_packValue key, _packValue val) : pairs
