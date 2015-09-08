module Codec.Packstream.Signature(
  Signature,
  signature,
  signatureByte
) where

import           Data.Word

data Signature = Sig Word8 deriving (Eq, Show)

{-# INLINE signature #-}
signature :: Word8 -> Signature
signature word = Sig word

{-# INLINE signatureByte #-}
signatureByte :: Signature -> Word8
signatureByte (Sig byte) = byte
