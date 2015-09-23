module Codec.Bolt.PTSValue(
  PTSValue(..),
  PTSBinary,
  PTSCodec,
  ptsEncode,
  ptsDecode,
  toPTSValue,
  fromPTSValue,
  toAtom,
  fromAtom
)

where

import           Codec.Packstream.Atom
import           Control.Applicative
import           Data.Binary
import           Data.Int
import qualified Data.Vector           as V

data PTSValue = PTSNull
              | PTSBool                   !Bool
              | PTSFloat   {-# UNPACK #-} !Double
              | PTSInteger {-# UNPACK #-} !Int64
              | PTSList    {-# UNPACK #-} !(V.Vector PTSValue)
              deriving (Show, Eq)

newtype PTSBinary = MkPTSBinary { atom :: Atom }

instance Binary PTSBinary where
  put = put . atom
  get = liftA MkPTSBinary get

class PTSCodec a where
  toPTSValue :: a -> PTSValue
  fromPTSValue :: PTSValue -> Maybe a
  ptsEncode :: a -> PTSBinary
  ptsEncode = ptsEncode . toPTSValue
  ptsDecode :: PTSBinary -> Maybe a
  ptsDecode bin = ptsDecode bin >>= fromPTSValue

instance PTSCodec PTSValue where
  toPTSValue = id
  fromPTSValue = Just
  ptsEncode = MkPTSBinary . toAtom
  ptsDecode = Just . fromAtom . atom

toAtom :: PTSValue -> Atom
toAtom PTSNull        = ANull
toAtom (PTSBool b)    = ABool b
toAtom (PTSFloat v)   = ADouble v
toAtom (PTSInteger v) = case v of
        _ | v == fromIntegral v8  -> AInt8 v8
        _ | v == fromIntegral v16 -> AInt16 v16
        _ | v == fromIntegral v32 -> AInt32 v32
        _                         -> AInt64 v
        where
          v8 = fromIntegral v :: Int8
          v16 = fromIntegral v :: Int16
          v32 = fromIntegral v :: Int32
toAtom (PTSList vs)   = AVector$ V.map toAtom vs

fromAtom :: Atom -> PTSValue
fromAtom ANull        = PTSNull
fromAtom (ABool b)    = PTSBool b
fromAtom (ADouble v)  = PTSFloat v
fromAtom (AInt64 v)   = PTSInteger v
fromAtom (AInt32 v)   = PTSInteger $ fromIntegral v
fromAtom (AInt16 v)   = PTSInteger $ fromIntegral v
fromAtom (AInt8 v)    = PTSInteger $ fromIntegral v
fromAtom (AVector vs) = PTSList $ V.map fromAtom vs
fromAtom (AList vs)   = PTSList $ V.fromList $ map fromAtom vs

instance PTSCodec () where
  toPTSValue _ = PTSNull
  fromPTSValue _ = Just ()
  ptsEncode _ = MkPTSBinary ANull
  ptsDecode _ = Just ()

instance PTSCodec a => PTSCodec (Maybe a) where
  toPTSValue (Just v) = toPTSValue v
  toPTSValue Nothing = PTSNull
  fromPTSValue PTSNull = Nothing
  fromPTSValue v = Just $ fromPTSValue v
  ptsEncode (Just v) = ptsEncode v
  ptsEncode _ = MkPTSBinary ANull
  ptsDecode (MkPTSBinary ANull) = Nothing
  ptsDecode (MkPTSBinary v) = Just $ fromPTSValue $ fromAtom v

instance (PTSCodec a, PTSCodec b) => PTSCodec (Either a b) where
  toPTSValue = either toPTSValue toPTSValue
  fromPTSValue v = fmap Left (fromPTSValue v) <|> fmap Right (fromPTSValue v)

instance PTSCodec Bool where
  toPTSValue = PTSBool
  fromPTSValue (PTSBool b) = Just b
  fromPTSValue _ = Nothing
  ptsEncode b = MkPTSBinary $ ABool b
  ptsDecode (MkPTSBinary (ABool b)) = Just b
  ptsDecode _ = Nothing

instance PTSCodec Double where
  toPTSValue = PTSFloat
  fromPTSValue (PTSFloat v) = Just v
  fromPTSValue _ = Nothing
  ptsEncode v = MkPTSBinary $ ADouble v
  ptsDecode (MkPTSBinary (ADouble v)) = Just v
  ptsDecode _ = Nothing

instance PTSCodec Int8 where
  toPTSValue = PTSInteger . fromIntegral
  fromPTSValue (PTSInteger v) =
    let w = fromIntegral v :: Int8
    in if v == fromIntegral w then Just w else Nothing
  fromPTSValue _ = Nothing

instance PTSCodec Int16 where
  toPTSValue = PTSInteger . fromIntegral
  fromPTSValue (PTSInteger v) =
    let w = fromIntegral v :: Int16
    in if v == fromIntegral w then Just w else Nothing
  fromPTSValue _ = Nothing

instance PTSCodec Int32 where
  toPTSValue = PTSInteger . fromIntegral
  fromPTSValue (PTSInteger v) =
    let w = fromIntegral v :: Int32
    in if v == fromIntegral w then Just w else Nothing
  fromPTSValue _ = Nothing

instance PTSCodec Int64 where
  toPTSValue = PTSInteger
  fromPTSValue (PTSInteger v) = Just v
  fromPTSValue _ = Nothing

instance PTSCodec Int where
  toPTSValue = PTSInteger . fromIntegral
  fromPTSValue (PTSInteger v) =
    let w = fromIntegral v :: Int
    in if v == fromIntegral w then Just w else Nothing
  fromPTSValue _ = Nothing

instance PTSCodec a => PTSCodec (V.Vector a) where
  toPTSValue vs = PTSList $ V.map toPTSValue vs
  fromPTSValue (PTSList vs) = V.mapM fromPTSValue vs
  fromPTSValue _ = Nothing
  ptsEncode vs = MkPTSBinary $ AVector $ V.map (atom . ptsEncode) vs
  ptsDecode (MkPTSBinary (AList vs)) = mapM (ptsDecode . MkPTSBinary) $ V.fromList vs
  ptsDecode (MkPTSBinary (AVector vs)) = mapM (ptsDecode . MkPTSBinary) vs
  ptsDecode _ = Nothing

instance PTSCodec a => PTSCodec [a] where
  toPTSValue vs = PTSList $ V.fromList $ map toPTSValue vs
  fromPTSValue (PTSList vs) = mapM fromPTSValue $ V.toList vs
  fromPTSValue _ = Nothing
  ptsEncode vs = MkPTSBinary $ AList $ map (atom . ptsEncode) vs
  ptsDecode (MkPTSBinary (AList vs)) = mapM (ptsDecode . MkPTSBinary) vs
  ptsDecode (MkPTSBinary (AVector vs)) = mapM (ptsDecode . MkPTSBinary) $ V.toList vs
  ptsDecode _ = Nothing

  -- ptsEncode vs = MkPTSBinary $ AVector $ V.map (atom . ptsEncode) vs

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
