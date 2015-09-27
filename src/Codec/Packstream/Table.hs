module Codec.Packstream.Table(
  Table,
  mkCase,
  compile,
  compileWithDefault,
  compileM,
  compileWithDefaultM
) where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

-- TODO: Switch to using an array based lookup table

newtype Table m a b = MkTable { unTable :: M.Map a (m b) } deriving (Show, Eq, Ord)

instance (Ord a) => Monoid (Table m a b) where
  mempty = MkTable M.empty
  (MkTable leftMap) `mappend` (MkTable rightMap) = MkTable $ leftMap `mappend` rightMap

instance (Ord a, Functor m) => Functor (Table m a) where
  fmap f (MkTable m) = MkTable $ M.map (fmap f) m

instance (Ord a, Applicative m) => Applicative (Table m a) where
  pure _ = MkTable M.empty
  (MkTable fMap) <*> (MkTable vMap) = MkTable $ M.intersectionWith (<*>) fMap vMap

mkCase :: a -> m b -> Table m a b
mkCase k v = MkTable $ M.singleton k v

compile :: (Ord a, Monad m, Monoid (m b)) => Table m a b -> a -> m b
compile t = compileWithDefault t mempty

compileWithDefault :: (Ord a, Monad m) => Table m a b -> m b -> a -> m b
compileWithDefault t els k = fromMaybe els $ M.lookup k $ unTable t

compileM :: (Ord a, Monad m, Monoid (m b)) => Table m a b -> m a -> m b
compileM t = compileWithDefaultM t mempty

compileWithDefaultM :: (Ord a, Monad m) => Table m a b -> m b -> m a -> m b
compileWithDefaultM t els keyM = keyM >>= \k -> fromMaybe els $ M.lookup k $ unTable t
