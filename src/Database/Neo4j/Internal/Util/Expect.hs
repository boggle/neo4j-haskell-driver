module Database.Neo4j.Internal.Util.Expect(
  expect
) where

import           Control.Monad   (guard)
import           Data.Binary     (Binary, get)
import           Data.Binary.Get (Get)

{-# INLINE expect #-}
expect :: (Binary a, Eq a) => a -> Get ()
expect expected = get >>= \actual -> guard (expected == actual)
