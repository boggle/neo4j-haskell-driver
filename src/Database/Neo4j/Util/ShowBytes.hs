module Database.Neo4j.Util.ShowBytes (
  showBytes
) where

import           Data.Binary
import qualified Data.Binary.Put      as P
import qualified Data.ByteString.Lazy as BSL
import           Data.Char            (toUpper)
import           Numeric              (showHex)

showBytes :: Binary a => a -> String
showBytes bin = unwords $ map hexWord8 (BSL.unpack bytes)
  where
    bytes = P.runPut (put bin)
    hexWord8 w = pad (map toUpper (showHex w ""))
    pad chars = replicate (2 - length chars) '0' ++ chars
