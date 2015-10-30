module Database.Neo4j.Util.BinaryValue(
  BinaryValue,
  toBinaryValue,
  fromBinaryValue
) where

import           Control.Applicative                     (liftA)
import           Data.Binary                             (Binary, get, put)
import           Database.Neo4j.Internal.Packstream.Atom (Atom)
import           Database.Neo4j.Internal.ValueCodec
import           Database.Neo4j.Util.ShowBytes           (showBytes)

newtype BinaryValue a = MkBinaryValue { binaryValue :: Atom }

toBinaryValue :: ValueCodec a => a -> BinaryValue a
toBinaryValue = MkBinaryValue . unEncode . encodeValue

fromBinaryValue :: ValueCodec a => BinaryValue a -> Maybe a
fromBinaryValue = decodeValue . Encode . binaryValue

instance ValueCodec a => Show (BinaryValue a) where
  show = showBytes

instance ValueCodec a => Binary (BinaryValue a) where
  put = put . binaryValue
  get = liftA MkBinaryValue get
