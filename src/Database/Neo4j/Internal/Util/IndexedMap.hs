module Database.Neo4j.Internal.Util.IndexedMap(
  IndexedMap,
  empty,
  singleton,
  extract,
  values,
  index,
  (!),
  lookup,
  insert
) where

import qualified Data.Map    as M
import qualified Data.Vector as V
import           Prelude     hiding (lookup)

data IndexedMap k v = MkIndexedMap (V.Vector v) (M.Map k Int) deriving (Show, Eq, Ord)

empty :: Ord k => IndexedMap k v
empty = MkIndexedMap V.empty M.empty

singleton :: Ord k => k -> v -> IndexedMap k v
singleton k v = MkIndexedMap (V.singleton v) (M.singleton k 0)

extract :: Ord k => (v -> k) -> V.Vector v -> IndexedMap k v
extract f vec = MkIndexedMap vec indexMap
  where
    indexMap = V.ifoldl indexValue M.empty vec
    indexValue m i v = M.insert (f v) i m

values :: IndexedMap k v -> V.Vector v
values (MkIndexedMap v _) = v

index :: Ord k => k -> IndexedMap k v -> Maybe Int
index k (MkIndexedMap _ m) = M.lookup k m

(!) :: IndexedMap k v -> Int -> v
(!) (MkIndexedMap vec _) idx = vec V.! idx

lookup :: Ord k => k -> IndexedMap k v -> Maybe v
lookup k (MkIndexedMap vec m) = M.lookup k m >>= (vec V.!?)

insert :: (Ord k, Eq v) => k -> v -> IndexedMap k v -> Maybe (IndexedMap k v)
insert k v im@(MkIndexedMap vec m) =
  case lookup k im of
    Just cur -> if v == cur then Just im else Nothing
    Nothing -> Just $ MkIndexedMap (V.snoc vec v) (M.insert k (V.length vec) m)
