module Codec.Packstream.Marker (
  Marker,
  allMarkers,
  markerByte,
  putMarker,
  getMarker,
  expectMarker,
  -- keep in sync w allMarkers
  _POS_TINY_INT_FIRST,
  _POS_TINY_INT_LAST,
  _TINY_TEXT_FIRST,
  _TINY_TEXT_LAST,
  _TINY_LIST_FIRST,
  _TINY_LIST_LAST,
  _TINY_MAP_FIRST,
  _TINY_MAP_LAST,
  _TINY_STRUCT_FIRST,
  _TINY_STRUCT_LAST,
  _NULL,
  _FLOAT_64,
  _FALSE,
  _TRUE,
  _INT_8,
  _INT_16,
  _INT_32,
  _INT_64,
  _TEXT_8,
  _TEXT_16,
  _TEXT_32,
  _END_OF_TEXT,
  _LIST_8,
  _LIST_16,
  _LIST_32,
  _LIST_STREAM,
  _MAP_8,
  _MAP_16,
  _MAP_32,
  _MAP_STREAM,
  _STRUCT_8,
  _STRUCT_16,
  _END_OF_STREAM,
  _NEG_TINY_INT_FIRST,
  _NEG_TINY_INT_LAST
) where

import           Control.Monad
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import           Data.Word

newtype Marker = MkMarker { markerByte :: Word8 } deriving (Eq, Show)

allMarkers :: [Marker]
allMarkers =
  [
    -- keep in sync w list of exported markers
    _POS_TINY_INT_FIRST,
    _POS_TINY_INT_LAST,
    _TINY_TEXT_FIRST,
    _TINY_TEXT_LAST,
    _TINY_LIST_FIRST,
    _TINY_LIST_LAST,
    _TINY_MAP_FIRST,
    _TINY_MAP_LAST,
    _TINY_STRUCT_FIRST,
    _TINY_STRUCT_LAST,
    _NULL,
    _FLOAT_64,
    _FALSE,
    _TRUE,
    _INT_8,
    _INT_16,
    _INT_32,
    _INT_64,
    _TEXT_8,
    _TEXT_16,
    _TEXT_32,
    _END_OF_TEXT,
    _LIST_8,
    _LIST_16,
    _LIST_32,
    _LIST_STREAM,
    _MAP_8,
    _MAP_16,
    _MAP_32,
    _MAP_STREAM,
    _STRUCT_8,
    _STRUCT_16,
    _END_OF_STREAM,
    _NEG_TINY_INT_FIRST,
    _NEG_TINY_INT_LAST
  ]

{-# INLINE putMarker #-}
putMarker :: Marker -> P.Put
putMarker = P.putWord8 . markerByte

{-# INLINE getMarker #-}
getMarker :: G.Get Marker
getMarker = liftM MkMarker G.getWord8

{-# INLINE expectMarker #-}
expectMarker :: Marker -> G.Get ()
expectMarker expected = getMarker >>= \actual -> guard (expected == actual)

_POS_TINY_INT_FIRST :: Marker
_POS_TINY_INT_FIRST = MkMarker 0x00

_POS_TINY_INT_LAST :: Marker
_POS_TINY_INT_LAST = MkMarker 0x7f

_TINY_TEXT_FIRST :: Marker
_TINY_TEXT_FIRST = MkMarker 0x80

_TINY_TEXT_LAST :: Marker
_TINY_TEXT_LAST = MkMarker 0x8f

_TINY_LIST_FIRST :: Marker
_TINY_LIST_FIRST = MkMarker 0x90

_TINY_LIST_LAST :: Marker
_TINY_LIST_LAST = MkMarker 0x9f

_TINY_MAP_FIRST :: Marker
_TINY_MAP_FIRST = MkMarker 0xa0

_TINY_MAP_LAST :: Marker
_TINY_MAP_LAST = MkMarker 0xaf

_TINY_STRUCT_FIRST :: Marker
_TINY_STRUCT_FIRST = MkMarker 0xb0

_TINY_STRUCT_LAST :: Marker
_TINY_STRUCT_LAST = MkMarker 0xbf

_NULL :: Marker
_NULL = MkMarker 0xc0

_FLOAT_64 :: Marker
_FLOAT_64 = MkMarker 0xc1

_FALSE :: Marker
_FALSE = MkMarker 0xc2

_TRUE :: Marker
_TRUE = MkMarker 0xc3

_INT_8 :: Marker
_INT_8 = MkMarker 0xc8

_INT_16 :: Marker
_INT_16 = MkMarker 0xc9

_INT_32 :: Marker
_INT_32 = MkMarker 0xca

_INT_64 :: Marker
_INT_64 = MkMarker 0xcb

_TEXT_8 :: Marker
_TEXT_8 = MkMarker 0xd0

_TEXT_16 :: Marker
_TEXT_16 = MkMarker 0xd1

_TEXT_32 :: Marker
_TEXT_32 = MkMarker 0xd2

_END_OF_TEXT :: Marker
_END_OF_TEXT = MkMarker 0x00

_LIST_8 :: Marker
_LIST_8 = MkMarker 0xd4

_LIST_16 :: Marker
_LIST_16 = MkMarker 0xd5

_LIST_32 :: Marker
_LIST_32 = MkMarker 0xd6

_LIST_STREAM :: Marker
_LIST_STREAM = MkMarker 0xd7

_MAP_8 :: Marker
_MAP_8 = MkMarker 0xd8

_MAP_16 :: Marker
_MAP_16 = MkMarker 0xd9

_MAP_32 :: Marker
_MAP_32 = MkMarker 0xda

_MAP_STREAM :: Marker
_MAP_STREAM = MkMarker 0xdb

_STRUCT_8 :: Marker
_STRUCT_8 = MkMarker 0xdc

_STRUCT_16 :: Marker
_STRUCT_16 = MkMarker 0xdd

_END_OF_STREAM :: Marker
_END_OF_STREAM = MkMarker 0xdf

_NEG_TINY_INT_FIRST :: Marker
_NEG_TINY_INT_FIRST = MkMarker 0xf0

_NEG_TINY_INT_LAST :: Marker
_NEG_TINY_INT_LAST = MkMarker 0xff
