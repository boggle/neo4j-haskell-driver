module Database.Neo4j.Internal.Packstream.Atom(
  Atom(..)
) where

import           Control.Applicative
import           Data.Binary             (Binary, get, put)
import           Data.Int
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Vector             as V

import qualified Database.Neo4j.Internal.Packstream.Coding as Coding
import           Database.Neo4j.Internal.Packstream.Signature

data Atom = ANull
          | ABool                     !Bool
          | ADouble    {-# UNPACK #-} !Double
          | AInt8      {-# UNPACK #-} !Int8
          | AInt16     {-# UNPACK #-} !Int16
          | AInt32     {-# UNPACK #-} !Int32
          | AInt64     {-# UNPACK #-} !Int64
          | AText      {-# UNPACK #-} !T.Text
          | AVector    {-# UNPACK #-} !(V.Vector Atom)
          | AList                     [Atom]
          | AMap       {-# UNPACK #-} !(V.Vector (T.Text, Atom))
          | AStreamedMap              [(T.Text, Atom)]
          | AStructure {-# UNPACK #-} !Signature {-# UNPACK #-} !(V.Vector Atom)
          deriving (Eq, Ord, Show)

instance Binary Atom where
  put = \case
    ANull               -> Coding.putNull
    AStructure sig vec  -> Coding.putStructure sig $ V.map put vec
    AText txt           -> Coding.putText txt
    ABool b             -> Coding.putBool b
    AInt8 i8            -> fromMaybe (Coding.putInt8 i8) (Coding.putTinyInt i8)
    AInt16 i16          -> Coding.putInt16 i16
    AInt32 i32          -> Coding.putInt32 i32
    AInt64 i64          -> Coding.putInt64 i64
    ADouble d           -> Coding.putFloat64 d
    AVector vec         -> Coding.putVector $ V.map put vec
    AList lst           -> Coding.streamList $ map put lst
    AMap vec            -> Coding.putMap $ V.map putEntry vec
    AStreamedMap lst    -> Coding.streamMap $ map putEntry lst
    where
      putEntry (k, v) = Coding.putEntry (Coding.putText k) (put v)
  get =
        ANull        <$  Coding.getNull
    <|> mkStructure  <$> Coding.getStructure get
    <|> AText        <$> Coding.getText
    <|> ABool        <$> Coding.getBool
    <|> AInt8        <$> Coding.getTinyInt
    <|> AInt8        <$> Coding.getInt8
    <|> AInt16       <$> Coding.getInt16
    <|> AInt32       <$> Coding.getInt32
    <|> AInt64       <$> Coding.getInt64
    <|> ADouble      <$> Coding.getFloat64
    <|> AVector      <$> Coding.getVector get
    <|> AList        <$> Coding.unStreamList get
    <|> AMap         <$> Coding.getMap getEntry
    <|> AStreamedMap <$> Coding.unStreamMap getEntry
    where
      mkStructure (sig, vec) = AStructure sig vec
      getEntry = Coding.getEntry Coding.getText get
