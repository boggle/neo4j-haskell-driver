{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Bolt.Encode(
  Encoder,
  BoltOut,
  null,
  false,
  true,
  double
) where

import           Data.Char               (toUpper)
import           Data.List               (intersperse)
import           Numeric                 (showHex)
import           Prelude                 hiding (null, putChar)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as L
import           Data.Monoid

data BoltOutRep = BNull BoltOutRep
                | BFalse BoltOutRep
                | BTrue BoltOutRep
                | BDouble Double BoltOutRep
                | BEnd

type Encoder t = t -> BoltOut

newtype BoltOut = BoltOut { unOut :: BoltOutRep -> BoltOutRep }

instance Monoid BoltOut where
  {-# INLINE mempty #-}
  mempty            = BoltOut id
  {-# INLINE mappend #-}
  b1 `mappend` b2   = BoltOut (unOut b1 . unOut b2)
  {-# INLINE mconcat #-}
  mconcat           = foldr mappend mempty

instance Show BoltOut where
  show = concat . intersperse " " . map hexWord8 . L.unpack . B.toLazyByteString . toBuilder
    where
      hexWord8 w = pad (map toUpper (showHex w ""))
      pad cs = replicate (2 - length cs) '0' ++ cs

toBuilder :: BoltOut -> B.Builder
toBuilder vs0 = step (unOut vs0 BEnd)
  where
    step (BNull cont) = B.word8 0xc0 <> step cont
    step (BDouble value cont) = B.word8 0xc1 <> (B.doubleBE value) <> step cont
    step (BFalse cont) = B.word8 0xc2 <> step cont
    step (BTrue cont) = B.word8 0xc3 <> step cont
    step BEnd = mempty

{-INLINE null-}
null :: BoltOut
null = BoltOut $ BNull

{-INLINE false-}
false :: BoltOut
false = BoltOut $ BFalse

{-INLINE true-}
true :: BoltOut
true = BoltOut $ BTrue

{-INLINE float-}
double :: Double -> BoltOut
double value = BoltOut $ (BDouble value)
