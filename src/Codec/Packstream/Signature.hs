module Codec.Packstream.Signature(
  Signature,
  signature,
  signatureByte
) where

import           Control.Monad
import           Data.Binary     (Binary, get, put)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import           Data.Word

newtype Signature = MkSignature { signatureByte :: Word8 } deriving (Eq, Show)

{-# INLINE signature #-}
signature :: Word8 -> Signature
signature = MkSignature

instance Binary Signature where
  {-# INLINE put #-}
  put = P.putWord8 . signatureByte
  {-# INLINE get #-}
  get = liftM signature G.getWord8
