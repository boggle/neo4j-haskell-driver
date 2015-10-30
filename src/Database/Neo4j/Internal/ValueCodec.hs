module Database.Neo4j.Internal.ValueCodec(
  EncodedValue(..),
  ValueCodec(..)
)

where

import           Data.Int
import qualified Data.Map.Lazy                             as LM
import qualified Data.Text                                 as T
import qualified Data.Vector                               as V
import           Database.Neo4j.Internal.Packstream.Atom
import           Database.Neo4j.Internal.Packstream.Atomic
import qualified Database.Neo4j.Internal.Packstream.Atomic as ATOMIC

newtype EncodedValue = Encode { unEncode :: Atom }

-- -- LAW: decodeValue $ encodeValue v = Just v
class Atomic a => ValueCodec a where
  {-# INLINE encodeValue #-}
  encodeValue :: a -> EncodedValue
  encodeValue = Encode . atomize
  {-# INLINE decodeValue #-}
  decodeValue :: EncodedValue -> Maybe a
  decodeValue = construct . unEncode

instance ValueCodec () where
instance ValueCodec a => ValueCodec (Maybe a) where
instance (ValueCodec a, ValueCodec b) => ValueCodec (Either a b) where
instance ValueCodec Bool where
instance ValueCodec T.Text where
instance ValueCodec Double where
instance ValueCodec Int8 where
instance ValueCodec Int16 where
instance ValueCodec Int32 where
instance ValueCodec Int64 where
instance ValueCodec Int where
instance ValueCodec a => ValueCodec (V.Vector a) where
instance ValueCodec a => ValueCodec [a] where
instance ValueCodec a => ValueCodec (ATOMIC.Map a) where
instance ValueCodec a => ValueCodec (ATOMIC.Lazy (LM.Map T.Text a)) where
