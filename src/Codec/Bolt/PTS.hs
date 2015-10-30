module Codec.Bolt.PTS(
  Value(..),
  -- packValue,
  -- unpackValue,
  Codec,
  DynValue,
  EncodedValue,
  toValue,
  fromValue,
  encodeValue,
  decodeValue
)

where

import           Codec.Bolt.PGM
import           Codec.Bolt.Atomic
import qualified Codec.Bolt.Internal.IndexedMap as IM
import           Codec.Packstream.Atom
import           Codec.Packstream.Signature
import           Control.Applicative
import           Control.Monad
import           Control.Arrow
import           Data.Binary
import           Data.Int
import qualified Data.Map.Lazy                  as LM
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromJust)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V

data Value  = NULL
            | BOOL                        !Bool
            | FLOAT        {-# UNPACK #-} !Double
            | INTEGER      {-# UNPACK #-} !Int64
            | TEXT         {-# UNPACK #-} !T.Text
            | LIST         {-# UNPACK #-} !(V.Vector DynValue)
            | MAP                         !(Properties DynValue)
            | NODE         {-# UNPACK #-} !(Node DynValue)
            | RELATIONSHIP {-# UNPACK #-} !(Relationship DynValue)
            | PATH         {-# UNPACK #-} !(Path DynValue)
            deriving (Show, Eq, Ord)

instance Atomic Value where
  atomize = undefined
  construct = undefined

data DynValue = forall a. (Codec a) => DynValue a

instance Show DynValue where
  show (DynValue v) = show $ toValue v

instance Eq DynValue where
  (==) l r = toValue l == toValue r

instance Ord DynValue where
  compare l r = compare (toValue l) (toValue r)

instance Atomic DynValue where
  atomize (DynValue v) = unEncode $ encodeValue v
  construct a = DynValue <$> (construct a :: Maybe Value)

-- packValue :: Value -> Atom
-- packValue NULL        = ANull
-- packValue (BOOL b)    = ABool b
-- packValue (FLOAT v)   = ADouble v
-- packValue (INTEGER v) = case v of
--         _ | v == fromIntegral v8  -> AInt8 v8
--         _ | v == fromIntegral v16 -> AInt16 v16
--         _ | v == fromIntegral v32 -> AInt32 v32
--         _                         -> AInt64 v
--         where
--           v8 = fromIntegral v :: Int8
--           v16 = fromIntegral v :: Int16
--           v32 = fromIntegral v :: Int32
-- packValue (TEXT txt) = AText txt
-- packValue (LIST vs)  = AVector $ V.map pack vs
-- packValue (MAP m)    = AMap $ M.foldMapWithKey packFlatDynEntryM m
-- packValue (NODE n)   = pack n
-- packValue (RELATIONSHIP rel) = pack rel
-- -- packValue (PATH path) = pack path
--
-- pack :: Codec a => a -> Atom
-- pack = decodeAtom . encodeValue
--
-- packEntry :: Codec v => (k, v) -> (k, Atom)
-- packEntry (k, v) = (k, pack v)
--
-- packFlatDynEntryM :: Monad m => a -> DynValue -> m (a, Atom)
-- packFlatDynEntryM k v = return (k, pack v)
--
-- unpackValue :: Atom -> Value
-- unpackValue ANull             = NULL
-- unpackValue (ABool b)         = BOOL b
-- unpackValue (ADouble v)       = FLOAT v
-- unpackValue (AInt8 v)         = INTEGER $ fromIntegral v
-- unpackValue (AInt16 v)        = INTEGER $ fromIntegral v
-- unpackValue (AInt32 v)        = INTEGER $ fromIntegral v
-- unpackValue (AInt64 v)        = INTEGER v
-- unpackValue (AText txt)       = TEXT txt
-- unpackValue (AVector vs)      = LIST $ V.map unpackDynValue vs
-- unpackValue (AList vs)        = LIST $ V.fromList $ map unpackDynValue vs
-- unpackValue (AMap vs)         = MAP $ V.foldl insertUnpackedDynEntry M.empty vs
-- unpackValue (AStreamedMap vs) = MAP $ foldl insertUnpackedDynEntry M.empty vs
-- unpackValue (struct @ (AStructure sig _)) | sig == _NODE = fromJust $ NODE <$> unpack struct
-- unpackValue (struct @ (AStructure sig _)) | sig == _RELATIONSHIP = fromJust $ RELATIONSHIP <$> unpack struct
-- -- unpackValue (struct @ (AStructure sig _)) | sig == _PATH = fromJust $ PATH <$> unpack struct
-- unpackValue _ = error "Structure with unsupported signature cannot be unpacked to a PTS Value"
--
-- unpack :: Codec a => Atom -> Maybe a
-- unpack = decodeValue . Encode
--
-- unpackDynValue :: Atom -> DynValue
-- unpackDynValue = DynValue . unpackValue
--
-- insertUnpackedDynEntry :: M.Map T.Text DynValue -> (T.Text, Atom) -> M.Map T.Text DynValue
-- insertUnpackedDynEntry m (k, v) = M.insert k (unpackDynValue v) m
--
-- insertUnpackedEntry :: (Ord k, Codec v) => M.Map k v -> (k, Atom) -> Maybe (M.Map k v)
-- insertUnpackedEntry m (k, a) =  (\v -> M.insert k v m) <$> unpack a
--
-- lazyInsertUnpackedEntry :: (Ord k, Codec v) => LM.Map k v -> (k, Atom) -> Maybe (LM.Map k v)
-- lazyInsertUnpackedEntry m (k, a) =  (\v -> LM.insert k v m) <$> unpack a
--
-- atomizeDynValue :: DynValue -> Atom
-- atomizeDynValue = decodeAtom . encodeValue
--
-- constructDynValue :: Atom -> Maybe DynValue
-- constructDynValue a = DynValue <$> (construct a :: Maybe Value)

-- LAW: fromValue $ toValue v = Just v
-- LAW: decodeValue $ encodeValue v = Just v
-- LAW: fromValue $ decodeValue $ encodeValue $ toValue v = Just v
-- LAW: fromDynValue $ toDynValue v = Just v
class EncodeableValue a => Codec a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a
  {-# INLINEABLE toDynValue #-}
  toDynValue :: a -> DynValue
  toDynValue = DynValue
  {-# INLINEABLE fromDynValue #-}
  fromDynValue :: DynValue -> Maybe a
  fromDynValue (DynValue v) = decodeValue $ encodeValue v

  -- {-# INLINEABLE encodeValue #-}
  -- encodeValue :: a -> EncodedValue
  -- encodeValue = encodeValue . toValue
  -- {-# INLINEABLE decodeValue #-}
  -- decodeValue :: EncodedValue -> Maybe a
  -- decodeValue bin = decodeValue bin >>= fromValue

-- {-# INLINE atomizeViaCodec #-}
-- atomizeViaCodec :: Codec a => a -> Atom
-- atomizeViaCodec = decodeAtom . encodeValue
--
-- {-# INLINE constructViaCodec #-}
-- constructViaCodec :: Codec a => Atom -> Maybe a
-- constructViaCodec = decodeValue . Encode

instance Codec Value where
  toValue = id
  fromValue = Just

instance Codec DynValue where
  toValue (DynValue v) = toValue v
  fromValue = Just . DynValue

instance Codec () where
  toValue _ = NULL
  fromValue _ = Just ()

instance Codec a => Codec (Maybe a) where
  toValue (Just v) = toValue v
  toValue Nothing = NULL
  fromValue NULL = Nothing
  fromValue v = Just $ fromValue v

instance (Codec a, Codec b) => Codec (Either a b) where
  toValue = either toValue toValue
  fromValue v = Left <$> fromValue v <|> Right <$> fromValue v

instance Codec Id where
  toValue v = INTEGER $ getId v
  fromValue (INTEGER i) = Just $ Id i
  fromValue _ = Nothing

instance Codec Bool where
  toValue = BOOL
  fromValue (BOOL b) = Just b
  fromValue _ = Nothing

instance Codec T.Text where
  toValue = TEXT
  fromValue (TEXT txt) = Just txt
  fromValue _ = Nothing

instance Codec Double where
  toValue = FLOAT
  fromValue (FLOAT v) = Just v
  fromValue _ = Nothing

instance Codec Int8 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int8
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

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

instance Codec Int where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec a => Codec (V.Vector a) where
  toValue vs = LIST $ V.map toDynValue vs
  fromValue (LIST vs) = V.mapM fromDynValue vs
  fromValue _ = Nothing

instance Codec a => Codec [a] where
  toValue vs = LIST $ V.fromList $ map toDynValue vs
  fromValue (LIST vs) = mapM fromDynValue $ V.toList vs
  fromValue _ = Nothing

instance Codec a => Codec (Map a) where
  toValue m = MAP $ M.map toDynValue m
  fromValue (MAP m) = M.foldMapWithKey foldEntry m
    where foldEntry k v = M.singleton k <$> fromDynValue v
  fromValue _ = Nothing

instance Codec a => Codec (LazyMap a) where
  toValue lm = MAP $ LM.map toDynValue $ lazyMap lm
  fromValue (MAP m) = LM.foldMapWithKey foldEntry m
    where foldEntry k v = LazyMap . LM.singleton k <$> fromDynValue v
  fromValue _ = Nothing

instance Codec (Node DynValue) where
  toValue = NODE
  fromValue (NODE n) = Just n
  fromValue _ = Nothing

instance Codec (Relationship DynValue) where
  toValue = RELATIONSHIP
  fromValue (RELATIONSHIP rel) = Just rel
  fromValue _ = Nothing

-- instance Codec Path where
--   toValue = PATH
--   fromValue (PATH path) = Just path
--   fromValue _ = Nothing
--   encodeValue path = Encode $
--     AStructure _PATH $ V.cons (pack $ IM.values $ pathNodesMap path)
--                      $ V.cons (AVector $ V.map packAsUnboundRelationship $ IM.values $ pathRelationshipsMap path)
--                      $ V.singleton (pack $ pathElements path)
  -- decodeValue (Encode (AStructure sig args)) | sig == _PATH =
  --   do
  --     pathNodes <- args V.!? 0 >>= unpack
  --     unboundRels <- args V.!? 1 >>= unpack
  --     elts <- args V.!? 2 >>= unpack
  --     unpackPath pathNodes unboundRels elts
  -- decodeValue _ = Nothing
--
-- data UnboundRelationship = UnboundRelationship {
--   unboundRelationshipId         :: Id,
--   unboundRelationshipType       :: T.Text,
--   unboundRelationshipProperties :: M.Map T.Text DynValue
-- }

-- instance Codec UnboundRelationship where
--   toValue =

-- packAsUnboundRelationship :: Relationship DynValue -> Atom
-- packAsUnboundRelationship rel =
--  AStructure _UNBOUND_RELATIONSHIP
--    $ V.cons (pack $ relationshipId rel)
--    $ V.cons (pack $ relationshipType rel)
--    $ V.singleton (pack $ relationshipProperties rel)
--
-- unpackPath :: V.Vector Node -> V.Vector Atom -> V.Vector Int -> Maybe Path
-- unpackPath nodes unboundRels elts = undefined

newtype EncodedValue = Encode { unEncode :: Atom }

newtype PackedValue a = Pack { unPack :: a }

instance EncodeableValue a => Binary (PackedValue a) where
  put = put . unEncode . encodeValue . unPack
  get = do
      atom <- get :: Get Atom
      case decodeValue $ Encode atom of
        Just v  -> return $ Pack v
        Nothing -> empty

{-# INLINE encodeAtomic #-}
encodeAtomic :: Atomic a => a -> EncodedValue
encodeAtomic = Encode . atomize

{-# INLINE decodeAtomic #-}
decodeAtomic :: Atomic a => EncodedValue -> Maybe a
decodeAtomic = construct . unEncode

class Atomic a => EncodeableValue a where
  encodeValue :: a -> EncodedValue
  decodeValue :: EncodedValue -> Maybe a

instance EncodeableValue Value where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue DynValue where
  encodeValue (DynValue v) = encodeValue v
  decodeValue enc = toDynValue <$> (decodeValue enc :: Maybe Value)

instance EncodeableValue () where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue a => EncodeableValue (Maybe a) where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance (EncodeableValue a, EncodeableValue b) => EncodeableValue (Either a b) where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Id where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Bool where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue T.Text where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Double where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Int8 where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Int16 where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Int32 where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Int64 where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue Int where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue a => EncodeableValue (V.Vector a) where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue a => EncodeableValue [a] where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue a => EncodeableValue (Map a) where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue a => EncodeableValue (LazyMap a) where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue a => EncodeableValue (Node a) where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic

instance EncodeableValue a => EncodeableValue (Relationship a) where
  encodeValue = encodeAtomic
  decodeValue = decodeAtomic
