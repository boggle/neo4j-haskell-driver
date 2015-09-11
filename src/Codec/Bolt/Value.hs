module Codec.Packstream.Encode(
)

where

import           Data.Codec.PackStream.Encode

type Encoder t = t -> PackStream
