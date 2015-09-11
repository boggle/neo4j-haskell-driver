module Codec.Packstream.Marker where

import           Data.Word

_POS_TINY_INT_FIRST :: Word8
_POS_TINY_INT_FIRST = 0x00

_POS_TINY_INT_LAST :: Word8
_POS_TINY_INT_LAST = 0x7f

_TINY_TEXT_FIRST :: Word8
_TINY_TEXT_FIRST = 0x80

_TINY_TEXT_LAST :: Word8
_TINY_TEXT_LAST = 0x8f

_TINY_LIST_FIRST :: Word8
_TINY_LIST_FIRST = 0x90

_TINY_LIST_LAST :: Word8
_TINY_LIST_LAST = 0x9f

_TINY_MAP_FIRST :: Word8
_TINY_MAP_FIRST = 0xa0

_TINY_MAP_LAST :: Word8
_TINY_MAP_LAST = 0xaf

_TINY_STRUCT_FIRST :: Word8
_TINY_STRUCT_FIRST = 0xb0

_TINY_STRUCT_LAST :: Word8
_TINY_STRUCT_LAST = 0xbf

_NULL :: Word8
_NULL = 0xc0

_FLOAT_64 :: Word8
_FLOAT_64 = 0xc1

_FALSE :: Word8
_FALSE = 0xc2

_TRUE :: Word8
_TRUE = 0xc3

_INT_8 :: Word8
_INT_8 = 0xc8

_INT_16 :: Word8
_INT_16 = 0xc9

_INT_32 :: Word8
_INT_32 = 0xca

_INT_64 :: Word8
_INT_64 = 0xcb

_TEXT_8 :: Word8
_TEXT_8 = 0xd0

_TEXT_16 :: Word8
_TEXT_16 = 0xd1

_TEXT_32 :: Word8
_TEXT_32 = 0xd2

_END_OF_TEXT :: Word8
_END_OF_TEXT = 0x00

_LIST_8 :: Word8
_LIST_8 = 0xd4

_LIST_16 :: Word8
_LIST_16 = 0xd5

_LIST_32 :: Word8
_LIST_32 = 0xd6

_LIST_STREAM :: Word8
_LIST_STREAM = 0xd7

_MAP_8 :: Word8
_MAP_8 = 0xd8

_MAP_16 :: Word8
_MAP_16 = 0xd9

_MAP_32 :: Word8
_MAP_32 = 0xda

_MAP_STREAM :: Word8
_MAP_STREAM = 0xdb

_STRUCT_8 :: Word8
_STRUCT_8 = 0xdc

_STRUCT_16 :: Word8
_STRUCT_16 = 0xdd

_END_OF_STREAM :: Word8
_END_OF_STREAM = 0xdf

_NEG_TINY_INT_FIRST :: Word8
_NEG_TINY_INT_FIRST = 0xf0

_NEG_TINY_INT_LAST :: Word8
_NEG_TINY_INT_LAST = 0xff
