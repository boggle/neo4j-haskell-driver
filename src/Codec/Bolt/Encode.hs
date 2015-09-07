{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Bolt.Encode(
  Encoder,
  BoltOut,
  true,
  false,
  null
) where

import           Numeric                 (showHex)
import           Prelude                 hiding (null, putChar)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as L
import           Data.Monoid

data BoltOutRep = BTrue BoltOutRep
                | BFalse BoltOutRep
                | BNull BoltOutRep
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
  show = (concatMap hexWord8) . L.unpack . B.toLazyByteString . toBuilder
    where
      hexWord8 w = pad (showHex w "")
      pad cs = replicate (2 - length cs) '0' ++ cs

toBuilder :: BoltOut -> B.Builder
toBuilder vs0 = step (unOut vs0 BEnd)
  where
    step (BTrue cont) = (B.word8 0xC3) <> step cont
    step (BFalse cont) = (B.word8 0xC2) <> step cont
    step (BNull cont) = (B.word8 0xC0) <> step cont
    step BEnd = mempty


{-INLINE true-}
true :: BoltOut
true = BoltOut $ BTrue

{-INLINE false-}
false :: BoltOut
false = BoltOut $ BFalse

{-INLINE null-}
null :: BoltOut
null = BoltOut $ BNull
