module Codec.Packstream.Atom(
  Atom(..)
) where

import qualified Codec.Packstream.Coding as Coding
import           Control.Applicative
import           Data.Binary             (Binary, get, put)
import           Data.Int
import           Data.Maybe
import qualified Data.Vector as V

data Atom = PSNull
          | PSBool                  !Bool
          | PSDouble {-# UNPACK #-} !Double
          | PSInt8   {-# UNPACK #-} !Int8
          | PSInt16  {-# UNPACK #-} !Int16
          | PSInt32  {-# UNPACK #-} !Int32
          | PSInt64  {-# UNPACK #-} !Int64
          | PSVector {-# UNPACK #-} !(V.Vector Atom)
          | PSList                  [Atom]
          deriving (Show, Eq)

instance Binary Atom where
  put = \case
    PSNull       -> Coding.putNull
    PSBool b     -> Coding.putBool b
    PSDouble d   -> Coding.putFloat64 d
    PSInt8 i8    -> fromMaybe (Coding.putInt8 i8) (Coding.putTinyInt i8)
    PSInt16 i16  -> Coding.putInt16 i16
    PSInt32 i32  -> Coding.putInt32 i32
    PSInt64 i64  -> Coding.putInt64 i64
    PSVector vec -> Coding.putVector $ V.map put vec
    PSList lst   -> Coding.streamList $ map put lst
  get =
        PSNull   <$  Coding.getNull
    <|> PSBool   <$> Coding.getBool
    <|> PSDouble <$> Coding.getFloat64
    <|> PSInt8   <$> Coding.getTinyInt
    <|> PSInt8   <$> Coding.getInt8
    <|> PSInt16  <$> Coding.getInt16
    <|> PSInt32  <$> Coding.getInt32
    <|> PSInt64  <$> Coding.getInt64
    <|> PSVector <$> Coding.getVector get
    <|> PSList   <$> Coding.unStreamList get
