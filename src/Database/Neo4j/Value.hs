module Database.Neo4j.Value(
  Map,
  LazyMap(..),
  AnyProperties,
  AnyNode,
  AnyRelationship,
  AnyStep,
  AnySegment,
  AnyPath,
  Value(..),
  AnyValue(..),
  Valued(..),
  ValueCodec(..),
  EncodedValue,
  module Database.Neo4j.Internal.PGM
)

where

import           Control.Applicative
import           Data.Int
import qualified Data.Map.Lazy                                as LM
import qualified Data.Map.Strict                              as M
import qualified Data.Text                                    as T
import qualified Data.Vector                                  as V
import           Database.Neo4j.Internal.PGM
import           Database.Neo4j.Internal.ValueCodec
import           Database.Neo4j.Internal.Packstream.Atomic    (Atomic, atomize, construct)
import qualified Database.Neo4j.Internal.Packstream.Atomic    as ATOMIC

data Value  = NULL
            | BOOL                        !Bool
            | FLOAT        {-# UNPACK #-} !Double
            | INTEGER      {-# UNPACK #-} !Int64
            | TEXT         {-# UNPACK #-} !T.Text
            | LIST         {-# UNPACK #-} !(V.Vector AnyValue)
            | MAP                         !AnyProperties
            | NODE         {-# UNPACK #-} !AnyNode
            | RELATIONSHIP {-# UNPACK #-} !AnyRelationship
            | PATH         {-# UNPACK #-} !AnyPath
            deriving (Show, Eq, Ord)

-- -- LAW: fromValue $ toValue v = Just v
-- -- LAW: fromValue $ decodeValue $ encodeValue $ toValue v = Just v
-- -- LAW: fromAnyValue $ toAnyValue v = Just v
class ValueCodec a => Valued a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a
  {-# INLINEABLE toAnyValue #-}
  toAnyValue :: a -> AnyValue
  toAnyValue = VALUE
  {-# INLINEABLE fromAnyValue #-}
  fromAnyValue :: AnyValue -> Maybe a
  fromAnyValue (VALUE v) = decodeValue $ encodeValue v

type Map = ATOMIC.Map AnyValue

newtype LazyMap = LazyMap { lazyMap :: LM.Map T.Text AnyValue }

instance Monoid LazyMap where
  mempty = LazyMap mempty
  (LazyMap l) `mappend` (LazyMap r) = LazyMap $ l `mappend` r

instance Atomic LazyMap where
  atomize (LazyMap m) = atomize $ ATOMIC.Lazy m
  construct a = LazyMap <$> ATOMIC.lazy <$> construct a

instance ValueCodec LazyMap where

type AnyProperties = Properties AnyValue
type AnyNode = Node AnyValue
type AnyRelationship = Relationship AnyValue
type AnyStep = Step AnyValue
type AnySegment = Segment AnyValue
type AnyPath = Path AnyValue

instance Atomic Value where
  atomize NULL = atomize ()
  atomize (TEXT t) = atomize t
  atomize (INTEGER i) = atomize i
  atomize (FLOAT f) = atomize f
  atomize (LIST l) = atomize l
  atomize (NODE n) = atomize n
  atomize (RELATIONSHIP r) = atomize r
  atomize (PATH p) = atomize p
  atomize (MAP m) = atomize m
  atomize (BOOL b) = atomize b
  construct a =  const NULL <$> (construct a :: Maybe ())
             <|> TEXT <$> construct a
             <|> INTEGER <$> construct a
             <|> FLOAT <$> construct a
             <|> NODE <$> construct a
             <|> RELATIONSHIP <$> construct a
             <|> PATH <$> construct a
             <|> LIST <$> construct a
             <|> MAP <$> construct a
             <|> BOOL <$> construct a

instance ValueCodec Value where

data AnyValue where
  VALUE :: (Valued a) => a -> AnyValue

instance Show AnyValue where
  show (VALUE v) = show $ toValue v

instance Eq AnyValue where
  (==) l r = toValue l == toValue r

instance Ord AnyValue where
  compare l r = compare (toValue l) (toValue r)

instance Atomic AnyValue where
  atomize (VALUE v) = unEncode $ encodeValue v
  construct a = VALUE <$> (construct a :: Maybe Value)

instance ValueCodec AnyValue where

instance Valued Value where
  toValue = id
  fromValue = Just

instance Valued AnyValue where
  toValue (VALUE v) = toValue v
  fromValue = Just . VALUE

instance Valued () where
  toValue _ = NULL
  fromValue _ = Just ()

instance Valued a => Valued (Maybe a) where
  toValue (Just v) = toValue v
  toValue Nothing = NULL
  fromValue NULL = Nothing
  fromValue v = Just $ fromValue v

instance (Valued a, Valued b) => Valued (Either a b) where
  toValue = either toValue toValue
  fromValue v = Left <$> fromValue v <|> Right <$> fromValue v

instance Valued Id where
  toValue v = INTEGER $ getId v
  fromValue (INTEGER i) = Just $ Id i
  fromValue _ = Nothing

instance Valued Bool where
  toValue = BOOL
  fromValue (BOOL b) = Just b
  fromValue _ = Nothing

instance Valued T.Text where
  toValue = TEXT
  fromValue (TEXT txt) = Just txt
  fromValue _ = Nothing

instance Valued Double where
  toValue = FLOAT
  fromValue (FLOAT v) = Just v
  fromValue _ = Nothing

instance Valued Int8 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int8
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Valued Int16 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int16
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Valued Int32 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int32
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Valued Int64 where
  toValue = INTEGER
  fromValue (INTEGER v) = Just v
  fromValue _ = Nothing

instance Valued Int where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Valued a => Valued (V.Vector a) where
  toValue vs = LIST $ V.map toAnyValue vs
  fromValue (LIST vs) = V.mapM fromAnyValue vs
  fromValue _ = Nothing

instance Valued a => Valued [a] where
  toValue vs = LIST $ V.fromList $ map toAnyValue vs
  fromValue (LIST vs) = mapM fromAnyValue $ V.toList vs
  fromValue _ = Nothing

instance Valued Map where
  toValue m = MAP $ M.map toAnyValue m
  fromValue (MAP m) = M.foldMapWithKey foldEntry m
    where foldEntry k v = M.singleton k <$> fromAnyValue v
  fromValue _ = Nothing

instance Valued LazyMap where
  toValue lm = MAP $ LM.map toAnyValue $ lazyMap lm
  fromValue (MAP m) = LM.foldMapWithKey foldEntry m
    where foldEntry k v = LazyMap . LM.singleton k <$> fromAnyValue v
  fromValue _ = Nothing

instance Valued AnyNode where
  toValue = NODE
  fromValue (NODE n) = Just n
  fromValue _ = Nothing

instance Valued AnyRelationship where
  toValue = RELATIONSHIP
  fromValue (RELATIONSHIP rel) = Just rel
  fromValue _ = Nothing
