module Codec.Bolt.PTS(
  Value(..),
  CodecValue,
  Codec,
  toValue,
  fromValue,
  encodeValue,
  decodeValue
)

where

import           Codec.Packstream.Atom
import           Control.Applicative
import           Data.Binary
import           Data.Int
import qualified Data.Text             as T
import qualified Data.Vector           as V

data Value  = NULL
            | BOOL                   !Bool
            | FLOAT   {-# UNPACK #-} !Double
            | INTEGER {-# UNPACK #-} !Int64
            | TEXT    {-# UNPACK #-} !T.Text
            | LIST    {-# UNPACK #-} !(V.Vector Value)
            deriving (Show, Eq)

newtype CodecValue = Encode { unEncode :: Atom }

instance Binary CodecValue where
  put = put . unEncode
  get = liftA Encode get

class Codec a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a
  {-# INLINEABLE encodeValue #-}
  encodeValue :: a -> CodecValue
  encodeValue = encodeValue . toValue
  {-# INLINEABLE decodeValue #-}
  decodeValue :: CodecValue -> Maybe a
  decodeValue bin = decodeValue bin >>= fromValue

instance Codec Value where
  toValue = id
  fromValue = Just
  encodeValue = Encode . toAtom
  decodeValue = Just . fromAtom . unEncode

toAtom :: Value -> Atom
toAtom NULL        = ANull
toAtom (BOOL b)    = ABool b
toAtom (FLOAT v)   = ADouble v
toAtom (INTEGER v) = case v of
        _ | v == fromIntegral v8  -> AInt8 v8
        _ | v == fromIntegral v16 -> AInt16 v16
        _ | v == fromIntegral v32 -> AInt32 v32
        _                         -> AInt64 v
        where
          v8 = fromIntegral v :: Int8
          v16 = fromIntegral v :: Int16
          v32 = fromIntegral v :: Int32
toAtom (TEXT txt)  = AText txt
toAtom (LIST vs)   = AVector $ V.map toAtom vs

fromAtom :: Atom -> Value
fromAtom ANull        = NULL
fromAtom (ABool b)    = BOOL b
fromAtom (ADouble v)  = FLOAT v
fromAtom (AInt8 v)    = INTEGER $ fromIntegral v
fromAtom (AInt16 v)   = INTEGER $ fromIntegral v
fromAtom (AInt32 v)   = INTEGER $ fromIntegral v
fromAtom (AInt64 v)   = INTEGER v
fromAtom (AText txt)  = TEXT txt
fromAtom (AVector vs) = LIST $ V.map fromAtom vs
fromAtom (AList vs)   = LIST $ V.fromList $ map fromAtom vs

instance Codec () where
  toValue _ = NULL
  fromValue _ = Just ()
  encodeValue _ = Encode ANull
  decodeValue _ = Just ()

instance Codec a => Codec (Maybe a) where
  toValue (Just v) = toValue v
  toValue Nothing = NULL
  fromValue NULL = Nothing
  fromValue v = Just $ fromValue v
  encodeValue (Just v) = encodeValue v
  encodeValue _ = Encode ANull
  decodeValue (Encode ANull) = Nothing
  decodeValue (Encode v) = Just $ fromValue $ fromAtom v

instance (Codec a, Codec b) => Codec (Either a b) where
  toValue = either toValue toValue
  fromValue v = fmap Left (fromValue v) <|> fmap Right (fromValue v)

instance Codec Bool where
  toValue = BOOL
  fromValue (BOOL b) = Just b
  fromValue _ = Nothing
  encodeValue b = Encode $ ABool b
  decodeValue (Encode (ABool b)) = Just b
  decodeValue _ = Nothing

instance Codec Double where
  toValue = FLOAT
  fromValue (FLOAT v) = Just v
  fromValue _ = Nothing
  encodeValue v = Encode $ ADouble v
  decodeValue (Encode (ADouble v)) = Just v
  decodeValue _ = Nothing

instance Codec Int8 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int8
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing
  encodeValue v = Encode $ AInt8 v

instance Codec Int16 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int16
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec Int32 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int32
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec Int64 where
  toValue = INTEGER
  fromValue (INTEGER v) = Just v
  fromValue _ = Nothing

instance Codec T.Text where
  toValue = TEXT
  fromValue (TEXT txt) = Just txt
  fromValue _ = Nothing
  encodeValue txt = Encode $ AText txt
  decodeValue (Encode (AText txt)) = Just txt
  decodeValue _ = Nothing

instance Codec Int where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec a => Codec (V.Vector a) where
  toValue vs = LIST $ V.map toValue vs
  fromValue (LIST vs) = V.mapM fromValue vs
  fromValue _ = Nothing
  encodeValue vs = Encode $ AVector $ V.map (unEncode . encodeValue) vs
  decodeValue (Encode (AList vs)) = mapM (decodeValue . Encode) $ V.fromList vs
  decodeValue (Encode (AVector vs)) = mapM (decodeValue . Encode) vs
  decodeValue _ = Nothing

instance Codec a => Codec [a] where
  toValue vs = LIST $ V.fromList $ map toValue vs
  fromValue (LIST vs) = mapM fromValue $ V.toList vs
  fromValue _ = Nothing
  encodeValue vs = Encode $ AList $ map (unEncode . encodeValue) vs
  decodeValue (Encode (AList vs)) = mapM (decodeValue . Encode) vs
  decodeValue (Encode (AVector vs)) = mapM (decodeValue . Encode) $ V.toList vs
  decodeValue _ = Nothing

  -- encodeValue vs = Encode $ AVector $ V.map (unEncode . encodeValue) vs

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
