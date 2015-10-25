module Codec.Packstream.Signature(
  Signature,
  signature,
  signatureByte,
  allSignatures,
  -- keep in sync w allSignatures
  _NODE,
  _PATH,
  _RELATIONSHIP,
  _UNBOUND_RELATIONSHIP,
  _ACK_FAILURE_MESSAGE,
  _RUN_MESSAGE,
  _DISCARD_ALL_MESSAGE,
  _PULL_ALL_MESSAGE,
  _SUCCESS_MESSAGE,
  _RECORD_MESSAGE,
  _IGNORED_MESSAGE,
  _FAILURE_MESSAGE
) where

import           Control.Monad
import           Data.Binary     (Binary, get, put)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import           Data.Word

newtype Signature = MkSignature { signatureByte :: Word8 } deriving (Eq, Ord, Show)

allSignatures :: [Signature]
allSignatures = [
    _NODE,
    _PATH,
    _RELATIONSHIP,
    _ACK_FAILURE_MESSAGE,
    _RUN_MESSAGE,
    _DISCARD_ALL_MESSAGE,
    _PULL_ALL_MESSAGE,
    _SUCCESS_MESSAGE,
    _RECORD_MESSAGE,
    _IGNORED_MESSAGE,
    _FAILURE_MESSAGE
  ]

{-# INLINE signature #-}
signature :: Word8 -> Signature
signature = MkSignature

instance Binary Signature where
  {-# INLINE put #-}
  put = P.putWord8 . signatureByte
  {-# INLINE get #-}
  get = liftM signature G.getWord8

_NODE :: Signature
_NODE = MkSignature 0x4e

_PATH :: Signature
_PATH = MkSignature 0x50

_RELATIONSHIP :: Signature
_RELATIONSHIP = MkSignature 0x52

_UNBOUND_RELATIONSHIP :: Signature
_UNBOUND_RELATIONSHIP = MkSignature 0x72

_ACK_FAILURE_MESSAGE :: Signature
_ACK_FAILURE_MESSAGE = MkSignature 0x0f

_RUN_MESSAGE :: Signature
_RUN_MESSAGE = MkSignature 0x10

_DISCARD_ALL_MESSAGE :: Signature
_DISCARD_ALL_MESSAGE = MkSignature 0x2f

_PULL_ALL_MESSAGE :: Signature
_PULL_ALL_MESSAGE = MkSignature 0x3f

_SUCCESS_MESSAGE :: Signature
_SUCCESS_MESSAGE = MkSignature 0x70

_RECORD_MESSAGE :: Signature
_RECORD_MESSAGE = MkSignature 0x71

_IGNORED_MESSAGE :: Signature
_IGNORED_MESSAGE = MkSignature 0x7e

_FAILURE_MESSAGE :: Signature
_FAILURE_MESSAGE = MkSignature 0x7f
