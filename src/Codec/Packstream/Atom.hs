module Codec.Packstream.Atom(
  Atom(..)
) where

import qualified Codec.Packstream.Coding as Coding
import           Control.Applicative
import           Data.Binary             (Binary, get, put)
import           Data.Int
import           Data.Maybe
import qualified Data.Vector as V

data Atom = ANull
          | ABool                  !Bool
          | ADouble {-# UNPACK #-} !Double
          | AInt8   {-# UNPACK #-} !Int8
          | AInt16  {-# UNPACK #-} !Int16
          | AInt32  {-# UNPACK #-} !Int32
          | AInt64  {-# UNPACK #-} !Int64
          | AVector {-# UNPACK #-} !(V.Vector Atom)
          | AList                  [Atom]
          deriving (Show, Eq)

instance Binary Atom where
  put = \case
    ANull       -> Coding.putNull
    ABool b     -> Coding.putBool b
    ADouble d   -> Coding.putFloat64 d
    AInt8 i8    -> fromMaybe (Coding.putInt8 i8) (Coding.putTinyInt i8)
    AInt16 i16  -> Coding.putInt16 i16
    AInt32 i32  -> Coding.putInt32 i32
    AInt64 i64  -> Coding.putInt64 i64
    AVector vec -> Coding.putVector $ V.map put vec
    AList lst   -> Coding.streamList $ map put lst
  get =
        ANull   <$  Coding.getNull
    <|> ABool   <$> Coding.getBool
    <|> ADouble <$> Coding.getFloat64
    <|> AInt8   <$> Coding.getTinyInt
    <|> AInt8   <$> Coding.getInt8
    <|> AInt16  <$> Coding.getInt16
    <|> AInt32  <$> Coding.getInt32
    <|> AInt64  <$> Coding.getInt64
    <|> AVector <$> Coding.getVector get
    <|> AList   <$> Coding.unStreamList get
