{-# LANGUAGE FlexibleInstances #-}
module Codec.Bolt.PTS(
  Value(..),
  LazyMap,
  unLazyMap,
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
import           Control.Monad
import           Data.Binary
import           Data.Int
import qualified Data.Map.Lazy         as LM
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import qualified Data.Vector           as V

data Value  = NULL
            | BOOL                   !Bool
            | FLOAT   {-# UNPACK #-} !Double
            | INTEGER {-# UNPACK #-} !Int64
            | TEXT    {-# UNPACK #-} !T.Text
            | LIST    {-# UNPACK #-} !(V.Vector Value)
            | MAP                    !(M.Map T.Text Value)
            deriving (Show, Eq)

newtype LazyMap a = LazyMap { unLazyMap :: LM.Map T.Text a }

instance Monoid a => Monoid (LazyMap a) where
  mempty = LazyMap mempty
  (LazyMap l) `mappend` (LazyMap r) = LazyMap $ l `mappend` r

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
toAtom (TEXT txt) = AText txt
toAtom (LIST vs)  = AVector $ V.map toAtom vs
toAtom (MAP m)    = AMap $ M.foldMapWithKey toEntryVector m

toEntryVector :: a -> Value -> V.Vector (a, Atom)
toEntryVector k v = return (k, toAtom v)

fromAtom :: Atom -> Value
fromAtom ANull             = NULL
fromAtom (ABool b)         = BOOL b
fromAtom (ADouble v)       = FLOAT v
fromAtom (AInt8 v)         = INTEGER $ fromIntegral v
fromAtom (AInt16 v)        = INTEGER $ fromIntegral v
fromAtom (AInt32 v)        = INTEGER $ fromIntegral v
fromAtom (AInt64 v)        = INTEGER v
fromAtom (AText txt)       = TEXT txt
fromAtom (AVector vs)      = LIST $ V.map fromAtom vs
fromAtom (AList vs)        = LIST $ V.fromList $ map fromAtom vs
fromAtom (AMap vs)         = MAP $ V.foldl insertFromEntry M.empty vs
fromAtom (AStreamedMap vs) = MAP $ foldl insertFromEntry M.empty vs

insertFromEntry :: M.Map T.Text Value -> (T.Text, Atom) -> M.Map T.Text Value
insertFromEntry m (k, v) = M.insert k (fromAtom v) m

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

instance Codec a => Codec (M.Map T.Text a) where
  toValue m = MAP $ M.map toValue m
  fromValue (MAP m) = M.foldMapWithKey foldEntry m
    where foldEntry k v = fmap (M.singleton k) (fromValue v)
  fromValue _ = Nothing
  encodeValue m = Encode $ AMap $ V.fromList $ map mapEntry $ M.toList m
    where mapEntry (k, v) = (k, unEncode $ encodeValue v)
  decodeValue (Encode (AMap vs)) = V.foldM foldEntry M.empty vs
    where foldEntry m (k, v) = fmap (\x -> M.insert k x m) $ decodeValue $ Encode v
  decodeValue (Encode (AStreamedMap vs)) = foldM foldEntry M.empty vs
    where foldEntry m (k, v) = fmap (\x -> M.insert k x m) $ decodeValue $ Encode v
  decodeValue _ = Nothing

instance Codec a => Codec (LazyMap (M.Map T.Text a)) where
  toValue (LazyMap m) = MAP $ LM.map toValue m
  fromValue (MAP m) = M.foldMapWithKey foldEntry m
    where foldEntry k v = fmap LazyMap $ fmap (LM.singleton k) (fromValue v)
  fromValue _ = Nothing
  encodeValue (LazyMap m) = Encode $ AStreamedMap $ map mapEntry $ LM.toList m
    where mapEntry (k, v) = (k, unEncode $ encodeValue v)
  decodeValue (Encode (AMap vs)) = LazyMap <$> V.foldM foldEntry LM.empty vs
    where foldEntry m (k, v) = fmap (\x -> LM.insert k x m) $ decodeValue $ Encode v
  decodeValue (Encode (AStreamedMap vs)) = LazyMap <$> foldM foldEntry LM.empty vs
    where foldEntry m (k, v) = fmap (\x -> LM.insert k x m) $ decodeValue $ Encode v
  decodeValue _ = Nothing
