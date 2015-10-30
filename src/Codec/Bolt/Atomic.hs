module Codec.Bolt.Atomic(
  Atomic(..),
  Map,
  LazyMap(..)
)

where

import           Codec.Packstream.Atom
import           Control.Applicative
import           Control.Monad
import           Data.Int
import           Data.Text             (Text)
import qualified Data.Vector           as V
import qualified Data.Map              as M
import qualified Data.Map.Lazy         as LM

type Map v = M.Map Text v

newtype LazyMap v = LazyMap { lazyMap :: LM.Map Text v }

deriving instance (Eq a) => Eq (LazyMap a)
deriving instance (Ord a) => Ord (LazyMap a)
deriving instance (Show a) => Show (LazyMap a)

instance Monoid (LazyMap a) where
  mempty = LazyMap mempty
  (LazyMap l) `mappend` (LazyMap r) = LazyMap $ l `mappend` r

-- LAW: construct $ atomize v = Just v
class Atomic a where
  atomize :: a -> Atom
  construct :: Atom -> Maybe a

instance Atomic Atom where
  atomize = id
  construct = Just

instance Atomic () where
  atomize _ = ANull
  construct _ = Just ()

instance Atomic a => Atomic (Maybe a) where
  atomize (Just v ) = atomize v
  atomize _ = ANull
  construct ANull = Nothing
  construct atom = Just $ construct atom

instance (Atomic a, Atomic b) => Atomic (Either a b) where
  atomize = either atomize atomize
  construct v = Left <$> construct v <|> Right <$> construct v

instance Atomic Bool where
  atomize = ABool
  construct (ABool b) = Just b
  construct _ = Nothing

instance Atomic Text where
  atomize = AText
  construct (AText t) = Just t
  construct _ = Nothing

instance Atomic Double where
  atomize = ADouble
  construct (ADouble d) = Just d
  construct _ = Nothing

instance Atomic Int64 where
  atomize v = case v of
    _ | v == fromIntegral v8  -> AInt8 v8
    _ | v == fromIntegral v16 -> AInt16 v16
    _ | v == fromIntegral v32 -> AInt32 v32
    _                         -> AInt64 v
    where
      v8 = fromIntegral v :: Int8
      v16 = fromIntegral v :: Int16
      v32 = fromIntegral v :: Int32
  construct (AInt8 i) = Just $ fromIntegral i
  construct (AInt16 i) = Just $ fromIntegral i
  construct (AInt32 i) = Just $ fromIntegral i
  construct (AInt64 i) = Just $ fromIntegral i
  construct _ = Nothing

instance Atomic Int32 where
  atomize v = case v of
    _ | v == fromIntegral v8  -> AInt8 v8
    _ | v == fromIntegral v16 -> AInt16 v16
    _                         -> AInt32 v
    where
      v8 = fromIntegral v :: Int8
      v16 = fromIntegral v :: Int16
  construct (AInt8 i) = Just $ fromIntegral i
  construct (AInt16 i) = Just $ fromIntegral i
  construct (AInt32 i) = Just $ fromIntegral i
  construct (AInt64 i) =
    let i32 = fromIntegral i :: Int32
    in if fromIntegral i32 == i then Just i32 else Nothing
  construct _ = Nothing

instance Atomic Int16 where
  atomize v = case v of
    _ | v == fromIntegral v8  -> AInt8 v8
    _                         -> AInt16 v
    where
      v8 = fromIntegral v :: Int8
  construct (AInt8 i) = Just $ fromIntegral i
  construct (AInt16 i) = Just $ fromIntegral i
  construct (AInt32 i) =
    let i16 = fromIntegral i :: Int16
    in if fromIntegral i16 == i then Just i16 else Nothing
  construct (AInt64 i) =
    let i16 = fromIntegral i :: Int16
    in if fromIntegral i16 == i then Just i16 else Nothing
  construct _ = Nothing

instance Atomic Int8 where
  atomize = AInt8
  construct (AInt8 i) = Just $ fromIntegral i
  construct (AInt16 i) =
    let i8 = fromIntegral i :: Int8
    in if fromIntegral i8 == i then Just i8 else Nothing
  construct (AInt32 i) =
    let i8 = fromIntegral i :: Int8
    in if fromIntegral i8 == i then Just i8 else Nothing
  construct (AInt64 i) =
    let i8 = fromIntegral i :: Int8
    in if fromIntegral i8 == i then Just i8 else Nothing
  construct _ = Nothing

instance Atomic Int where
  atomize i = atomize i64 where i64 = fromIntegral i :: Int64
  construct (AInt8 v) = Just $ fromIntegral v
  construct (AInt16 v) = Just $ fromIntegral v
  construct (AInt32 v) =
    let i = fromIntegral v :: Int
    in if fromIntegral i == v then Just i else Nothing
  construct (AInt64 v) =
    let i = fromIntegral v :: Int
    in if fromIntegral i == v then Just i else Nothing
  construct _ = Nothing

instance Atomic a => Atomic (V.Vector a) where
  atomize vs = AVector $ V.map atomize vs
  construct (AList vs) = mapM construct $ V.fromList vs
  construct (AVector vs) = mapM construct vs
  construct _ = Nothing

instance Atomic a => Atomic [a] where
  atomize xs = AList $ map atomize xs
  construct (AList xs) = mapM construct xs
  construct (AVector xs) = mapM construct $ V.toList xs
  construct _ = Nothing

instance Atomic v => Atomic (Map v) where
  atomize props = AMap $ V.fromList $ map atomizeMapEntry $ M.toList props
  construct (AMap vs) = V.foldM insertAtomizedMapEntry M.empty vs
  construct (AStreamedMap vs) = foldM insertAtomizedMapEntry M.empty vs
  construct _ = Nothing

instance Atomic v => Atomic (LazyMap v) where
  atomize m = AStreamedMap $ map atomizeMapEntry $ LM.toList $ lazyMap m
  construct (AMap vs) = LazyMap <$> V.foldM lazyInsertAtomizedMapEntry LM.empty vs
  construct (AStreamedMap vs) = LazyMap <$> foldM lazyInsertAtomizedMapEntry LM.empty vs
  construct _ = Nothing

atomizeMapEntry :: Atomic v => (Text, v) -> (Text, Atom)
atomizeMapEntry (k, v) = (k, atomize v)

constructMapEntry :: Atomic v => (Text, Atom) -> Maybe (Text, v)
constructMapEntry (k, atom) = (,) k <$> construct atom

insertAtomizedMapEntry :: Atomic v => Map v -> (Text, Atom) -> Maybe (Map v)
insertAtomizedMapEntry m e = insertEntry <$> constructMapEntry e where insertEntry (k, v) = M.insert k v m

lazyInsertAtomizedMapEntry :: Atomic v => Map v -> (Text, Atom) -> Maybe (Map v)
lazyInsertAtomizedMapEntry m e = insertEntry <$> constructMapEntry e where insertEntry (k, v) = LM.insert k v m
