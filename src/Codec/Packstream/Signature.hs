module Codec.Packstream.Signature(
  Signature,
  signature,
  signatureByte,
  putSignature,
  getSignature,
  expectSignature
) where

import           Control.Monad
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import           Data.Word

-- TODO 0 Test signatures
newtype Signature = MkSignature { signatureByte :: Word8 } deriving (Eq, Show)

{-# INLINE signature #-}
signature :: Word8 -> Signature
signature = MkSignature

{-# INLINE putSignature #-}
putSignature :: Signature -> P.Put
putSignature = P.putWord8 . signatureByte

{-# INLINE getSignature #-}
getSignature :: G.Get Signature
getSignature = liftM signature G.getWord8

{-# INLINE expectSignature #-}
expectSignature :: Signature -> G.Get ()
expectSignature expected = getSignature >>= \actual -> guard (expected == actual)
