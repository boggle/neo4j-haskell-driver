{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeFamilies              #-}
module Codec.Bolt.PTS(
  Value(..),
  packValue,
  unpackValue,
  Codec,
  DynValue,
  Id,
  unId,
  Node,
  nodeId,
  nodeLabels,
  nodeProperties,
  Relationship,
  relationshipId,
  relationshipType,
  relationshipStartNodeId,
  relationshipEndNodeId,
  relationshipProperties,
  Entity,
  entityId,
  entityProperties,
  Direction(..),
  Step,
  stepNode,
  stepRelationship,
  stepDirection,
  Segment,
  segmentDirection,
  segmentStartNode,
  segmentEndNode,
  segmentRelationshipId,
  segmentRelationshipType,
  segmentRelationshipProperties,
  segmentRelationship,
  (<-|),
  (|--),
  (--|),
  (|->),
  Path,
  singleNodePath,
  (<@>),
  (<+>),
  pathLength,
  pathNodes,
  pathRelationships,
  pathSegment,
  pathSegments,
  pathSegments_,
  Directed(..),
  Invertible(..),
  LazyMap,
  unLazyMap,
  EncodedValue,
  toValue,
  fromValue,
  encodeValue,
  decodeValue
)

where

import qualified Codec.Bolt.Internal.IndexedMap as IM
import           Codec.Packstream.Atom
import           Codec.Packstream.Signature
import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Int
import qualified Data.Map.Lazy                  as LM
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromJust)
import qualified Data.Text                      as T
import qualified Data.Vector                    as V

newtype EncodedValue = Encode { unEncode :: Atom }

instance Binary EncodedValue where
  put = put . unEncode
  get = liftA Encode get

data Value  = NULL
            | BOOL                        !Bool
            | FLOAT        {-# UNPACK #-} !Double
            | INTEGER      {-# UNPACK #-} !Int64
            | TEXT         {-# UNPACK #-} !T.Text
            | LIST         {-# UNPACK #-} !(V.Vector DynValue)
            | MAP                         !(M.Map T.Text DynValue)
            | NODE         {-# UNPACK #-} !Node
            | RELATIONSHIP {-# UNPACK #-} !Relationship
            | PATH         {-# UNPACK #-} !Path
            deriving (Show, Eq, Ord)

data DynValue = forall a. (Codec a) => DynValue a

instance Show DynValue where
  show (DynValue v) = show v

instance Eq DynValue where
  (==) l r = toValue l == toValue r

instance Ord DynValue where
  compare l r = compare (toValue l) (toValue r)

class (Show a, Eq a) => Codec a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a
  {-# INLINEABLE toDynValue #-}
  toDynValue :: a -> DynValue
  toDynValue = DynValue
  {-# INLINEABLE fromDynValue #-}
  fromDynValue :: DynValue -> Maybe a
  fromDynValue = decodeValue . encodeValue
  {-# INLINEABLE encodeValue #-}
  encodeValue :: a -> EncodedValue
  encodeValue = encodeValue . toValue
  {-# INLINEABLE decodeValue #-}
  decodeValue :: EncodedValue -> Maybe a
  decodeValue bin = decodeValue bin >>= fromValue

newtype Id = Id { unId :: Int64 } deriving (Eq, Show, Ord)

data Node = MkNode {
    nodeId         :: Id,
    nodeLabels     :: V.Vector T.Text,
    nodeProperties :: M.Map T.Text DynValue
  } deriving (Eq, Show, Ord)

data Relationship = MkRelationship {
    relationshipId          :: Id,
    relationshipType        :: T.Text,
    relationshipStartNodeId :: Id,
    relationshipEndNodeId   :: Id,
    relationshipProperties  :: M.Map T.Text DynValue
} deriving (Eq, Show, Ord)

class Entity a where
  entityId :: a -> Id
  entityProperties :: a -> M.Map T.Text DynValue

instance Entity Node where
  entityId = nodeId
  entityProperties = nodeProperties

instance Entity Relationship where
  entityId = relationshipId
  entityProperties = relationshipProperties

data Direction = INCOMING | OUTGOING deriving (Eq, Show, Ord)

data Step = MkStep {
  stepNode         :: Node,
  stepRelationship :: Relationship,
  stepDirection    :: Direction
} deriving (Eq, Show, Ord)

data Segment = MkSegment {
    segmentDirection              :: Maybe Direction,
    segmentRelationshipId         :: Id,
    segmentRelationshipType       :: T.Text,
    segmentStartNode              :: Node,
    segmentEndNode                :: Node,
    segmentRelationshipProperties :: M.Map T.Text DynValue
} deriving (Eq, Show, Ord)

(--|) :: Node -> Relationship -> Maybe Step
(--|) startNode rel =
  if nodeId startNode == relationshipStartNodeId rel
  then Just MkStep { stepNode = startNode, stepRelationship = rel, stepDirection = OUTGOING }
  else Nothing

(|->) :: Maybe Step -> Node -> Maybe Segment
(|->) (Just step) endNode | stepDirection step == OUTGOING =
    if nodeId endNode == relationshipEndNodeId rel
    then Just MkSegment {
      segmentDirection = if nodeId startNode == nodeId endNode then Nothing else Just OUTGOING,
      segmentRelationshipId = relationshipId rel,
      segmentRelationshipType = relationshipType rel,
      segmentStartNode = startNode,
      segmentEndNode = endNode,
      segmentRelationshipProperties = relationshipProperties rel
    }
    else Nothing
    where
      startNode = stepNode step
      rel = stepRelationship step
(|->) _ _ = Nothing

(<-|) :: Node -> Relationship -> Maybe Step
(<-|) endNode rel =
  if nodeId endNode == relationshipEndNodeId rel
  then Just MkStep { stepNode = endNode, stepRelationship = rel, stepDirection = INCOMING }
  else Nothing

(|--) :: Maybe Step -> Node -> Maybe Segment
(|--) (Just step) startNode | stepDirection step == INCOMING =
    if nodeId startNode == relationshipStartNodeId rel
    then Just MkSegment {
      segmentDirection = if nodeId startNode == nodeId endNode then Nothing else Just INCOMING,
      segmentRelationshipId = relationshipId rel,
      segmentRelationshipType = relationshipType rel,
      segmentStartNode = startNode,
      segmentEndNode = endNode,
      segmentRelationshipProperties = relationshipProperties rel
    }
    else Nothing
    where
      endNode = stepNode step
      rel = stepRelationship step
(|--) _ _ = Nothing

segmentRelationship :: Segment -> Relationship
segmentRelationship segment =
  if segmentDirection segment == Just INCOMING
  then MkRelationship {
      relationshipId = segmentRelationshipId segment,
      relationshipType = segmentRelationshipType segment,
      relationshipStartNodeId = nodeId $ segmentEndNode segment,
      relationshipEndNodeId = nodeId $ segmentStartNode segment,
      relationshipProperties = segmentRelationshipProperties segment
    }
  else MkRelationship {
      relationshipId = segmentRelationshipId segment,
      relationshipType = segmentRelationshipType segment,
      relationshipStartNodeId = nodeId $ segmentStartNode segment,
      relationshipEndNodeId = nodeId $ segmentEndNode segment,
      relationshipProperties = segmentRelationshipProperties segment
    }

data Path = MkPath {
  pathNodesMap         :: IM.IndexedMap Id Node,
  pathRelationshipsMap :: IM.IndexedMap Id Relationship,
  pathElements         :: V.Vector Int
} deriving (Show, Eq, Ord)

singleNodePath :: Node -> Path
singleNodePath startNode = MkPath {
  pathNodesMap = IM.singleton (nodeId startNode) startNode,
  pathRelationshipsMap = IM.empty,
  pathElements = V.empty
}

(<@>) :: Node -> Maybe Path
(<@>) = Just . singleNodePath

(<+>) :: Maybe Path -> Maybe Segment -> Maybe Path
(<+>) (Just path) (Just segment) =
  if end path == startNode
  then do
    newPathNodesMap <- IM.insert startNodeId startNode (pathNodesMap path) >>= IM.insert endNodeId endNode
    newPathRelsMap <- IM.insert relId rel $ pathRelationshipsMap path
    relIndex <- IM.index relId newPathRelsMap
    endNodeIndex <- IM.index endNodeId newPathNodesMap
    return $
      let relElt = if segmentDirection segment == Just INCOMING then - relIndex else relIndex
      in MkPath {
        pathNodesMap = newPathNodesMap,
        pathRelationshipsMap = newPathRelsMap,
        pathElements = V.snoc (V.snoc (pathElements path) relElt) endNodeIndex
      }
  else Nothing
  where
    startNodeId = nodeId startNode
    startNode = start segment
    endNodeId = nodeId endNode
    endNode = end segment
    relId = segmentRelationshipId segment
    rel = segmentRelationship segment
(<+>) _ _ = Nothing

pathLength :: Path -> Int
pathLength path = div (V.length $ pathElements path) 2

pathNodes :: Path -> V.Vector Node
pathNodes path = V.map nodeAtIndex $ V.ifilter nodeIndices $ pathElements path
  where
    nodeAtIndex idx = pathNodesMap path IM.! idx
    nodeIndices idx _ = mod idx 2 == 1

pathRelationships :: Path -> V.Vector Relationship
pathRelationships path = V.map relAtIndex $ V.ifilter relIndices $ pathElements path
  where
    relAtIndex idx = pathRelationshipsMap path IM.! idx
    relIndices idx _ = mod idx 2 == 0

pathSegment :: Int -> Path -> Maybe Segment
pathSegment i path =
  if i < 0 || i >= len
  then Nothing
  else Just MkSegment {
    segmentDirection =
      if startNodeId == endNodeId then Nothing
      else Just (if startNodeId == relationshipStartNodeId rel then OUTGOING else INCOMING),
    segmentRelationshipId = relationshipId rel,
    segmentRelationshipType = relationshipType rel,
    segmentStartNode = startNode,
    segmentEndNode = endNode,
    segmentRelationshipProperties = relationshipProperties rel
  }
  where
    len = pathLength path
    idx = i * 2
    elts = pathElements path
    nodes = pathNodesMap path
    startNode = nodes IM.! (if i == 0 then 0 else elts V.! (idx - 1))
    startNodeId = nodeId startNode
    endNode = nodes IM.! (elts V.! (idx + 1))
    endNodeId = nodeId endNode
    rels = pathRelationshipsMap path
    rel = rels IM.! (elts V.! idx)

pathSegments :: Path -> [Segment]
pathSegments path = map fromJust $ pathSegments_ path

pathSegments_ :: Path -> [Maybe Segment]
pathSegments_ path =
  if lst == 0
    then []
    else map segment [0..(lst - 1)]
  where
    lst = pathLength path
    segment i = pathSegment i path

(<++>) :: Maybe Path -> Maybe Path -> Maybe Path
(<++>) Nothing Nothing = Nothing
(<++>) optPath Nothing = optPath
(<++>) Nothing optPath = optPath
(<++>) optPath (Just path) = foldl (<+>) optPath $ pathSegments_ path

instance Monoid (Maybe Path) where
  mempty = Nothing
  mappend = (<++>)

class Directed a where
  type Anchor a
  start :: a -> Anchor a
  end :: a -> Anchor a

instance Directed Relationship where
  type Anchor Relationship = Id
  start = relationshipStartNodeId
  end = relationshipEndNodeId

instance Directed Segment where
  type Anchor Segment = Node
  start = segmentStartNode
  end = segmentEndNode

instance Directed Path where
  type Anchor Path = Node
  start path = pathNodesMap path IM.! 0
  end path = pathNodesMap path IM.! (if V.null elts then 0 else V.last elts)
             where elts = pathElements path

class Invertible a where
  inverse :: a -> a

instance Invertible a => Invertible (Maybe a) where
  inverse = fmap inverse

instance Invertible Direction where
  inverse INCOMING = OUTGOING
  inverse OUTGOING = INCOMING

instance Invertible Segment where
  inverse segment = segment {
    segmentStartNode = segmentEndNode segment,
    segmentEndNode = segmentStartNode segment,
    segmentDirection = inverse $ segmentDirection segment
  }

-- TODO instance invertible Path

newtype LazyMap a = LazyMap { unLazyMap :: LM.Map T.Text a } deriving (Eq, Show)

instance Monoid a => Monoid (LazyMap a) where
  mempty = LazyMap mempty
  (LazyMap l) `mappend` (LazyMap r) = LazyMap $ l `mappend` r

packValue :: Value -> Atom
packValue NULL        = ANull
packValue (BOOL b)    = ABool b
packValue (FLOAT v)   = ADouble v
packValue (INTEGER v) = case v of
        _ | v == fromIntegral v8  -> AInt8 v8
        _ | v == fromIntegral v16 -> AInt16 v16
        _ | v == fromIntegral v32 -> AInt32 v32
        _                         -> AInt64 v
        where
          v8 = fromIntegral v :: Int8
          v16 = fromIntegral v :: Int16
          v32 = fromIntegral v :: Int32
packValue (TEXT txt) = AText txt
packValue (LIST vs)  = AVector $ V.map pack vs
packValue (MAP m)    = AMap $ M.foldMapWithKey packFlatDynEntryM m
packValue (NODE n)   = pack n
packValue (RELATIONSHIP rel) = pack rel
packValue (PATH path) = pack path

pack :: Codec a => a -> Atom
pack = unEncode . encodeValue

packEntry :: Codec v => (k, v) -> (k, Atom)
packEntry (k, v) = (k, pack v)

packFlatDynEntryM :: Monad m => a -> DynValue -> m (a, Atom)
packFlatDynEntryM k v = return (k, pack v)

unpackValue :: Atom -> Value
unpackValue ANull             = NULL
unpackValue (ABool b)         = BOOL b
unpackValue (ADouble v)       = FLOAT v
unpackValue (AInt8 v)         = INTEGER $ fromIntegral v
unpackValue (AInt16 v)        = INTEGER $ fromIntegral v
unpackValue (AInt32 v)        = INTEGER $ fromIntegral v
unpackValue (AInt64 v)        = INTEGER v
unpackValue (AText txt)       = TEXT txt
unpackValue (AVector vs)      = LIST $ V.map unpackDynValue vs
unpackValue (AList vs)        = LIST $ V.fromList $ map unpackDynValue vs
unpackValue (AMap vs)         = MAP $ V.foldl insertUnpackedDynEntry M.empty vs
unpackValue (AStreamedMap vs) = MAP $ foldl insertUnpackedDynEntry M.empty vs
unpackValue (struct @ (AStructure sig _)) | sig == _NODE = fromJust $ NODE <$> unpack struct
unpackValue (struct @ (AStructure sig _)) | sig == _RELATIONSHIP = fromJust $ RELATIONSHIP <$> unpack struct
unpackValue (struct @ (AStructure sig _)) | sig == _PATH = fromJust $ PATH <$> unpack struct
unpackValue _ = error "Structure with unsupported signature cannot be unpacked to a PTS Value"

unpack :: Codec a => Atom -> Maybe a
unpack = decodeValue . Encode

unpackDynValue :: Atom -> DynValue
unpackDynValue = DynValue . unpackValue

insertUnpackedDynEntry :: M.Map T.Text DynValue -> (T.Text, Atom) -> M.Map T.Text DynValue
insertUnpackedDynEntry m (k, v) = M.insert k (unpackDynValue v) m

insertUnpackedEntry :: (Ord k, Codec v) => M.Map k v -> (k, Atom) -> Maybe (M.Map k v)
insertUnpackedEntry m (k, a) =  (\v -> M.insert k v m) <$> unpack a

lazyInsertUnpackedEntry :: (Ord k, Codec v) => LM.Map k v -> (k, Atom) -> Maybe (LM.Map k v)
lazyInsertUnpackedEntry m (k, a) =  (\v -> LM.insert k v m) <$> unpack a

instance Codec Value where
  toValue = id
  fromValue = Just
  encodeValue = Encode . packValue
  decodeValue = Just . unpackValue . unEncode

instance Codec DynValue where
  toValue (DynValue x) = toValue x
  fromValue = Just . DynValue
  toDynValue = id
  fromDynValue = Just

instance Codec () where
  toValue _ = NULL
  fromValue _ = Just ()
  encodeValue _ = Encode ANull
  decodeValue _ = Just ()

instance Codec a => Codec (Maybe a) where
  toValue (Just v) = toValue v
  toValue Nothing = NULL
  fromValue NULL = Nothing
  fromValue v = Just $ fromValue v
  encodeValue (Just v) = encodeValue v
  encodeValue _ = Encode ANull
  decodeValue (Encode ANull) = Nothing
  decodeValue encoded = Just $ decodeValue encoded

instance (Codec a, Codec b) => Codec (Either a b) where
  toValue = either toValue toValue
  fromValue v = Left <$> fromValue v <|> Right <$> fromValue v

instance Codec Id where
  toValue v = INTEGER $ unId v
  fromValue (INTEGER i) = Just $ Id i
  fromValue _ = Nothing

instance Codec Bool where
  toValue = BOOL
  fromValue (BOOL b) = Just b
  fromValue _ = Nothing
  encodeValue b = Encode $ ABool b
  decodeValue (Encode (ABool b)) = Just b
  decodeValue _ = Nothing

instance Codec Double where
  toValue = FLOAT
  fromValue (FLOAT v) = Just v
  fromValue _ = Nothing
  encodeValue v = Encode $ ADouble v
  decodeValue (Encode (ADouble v)) = Just v
  decodeValue _ = Nothing

instance Codec Int8 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int8
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec Int16 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int16
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec Int32 where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int32
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec Int64 where
  toValue = INTEGER
  fromValue (INTEGER v) = Just v
  fromValue _ = Nothing

instance Codec T.Text where
  toValue = TEXT
  fromValue (TEXT txt) = Just txt
  fromValue _ = Nothing
  encodeValue txt = Encode $ AText txt
  decodeValue (Encode (AText txt)) = Just txt
  decodeValue _ = Nothing

instance Codec Int where
  toValue = INTEGER . fromIntegral
  fromValue (INTEGER v) =
    let w = fromIntegral v :: Int
    in if v == fromIntegral w then Just w else Nothing
  fromValue _ = Nothing

instance Codec a => Codec (V.Vector a) where
  toValue vs = LIST $ V.map toDynValue vs
  fromValue (LIST vs) = V.mapM fromDynValue vs
  fromValue _ = Nothing
  encodeValue vs = Encode $ AVector $ V.map pack vs
  decodeValue (Encode (AList vs)) = mapM unpack $ V.fromList vs
  decodeValue (Encode (AVector vs)) = mapM unpack vs
  decodeValue _ = Nothing

instance Codec a => Codec [a] where
  toValue vs = LIST $ V.fromList $ map toDynValue vs
  fromValue (LIST vs) = mapM fromDynValue $ V.toList vs
  fromValue _ = Nothing
  encodeValue vs = Encode $ AList $ map pack vs
  decodeValue (Encode (AList vs)) = mapM unpack vs
  decodeValue (Encode (AVector vs)) = mapM unpack $ V.toList vs
  decodeValue _ = Nothing

instance Codec a => Codec (M.Map T.Text a) where
  toValue m = MAP $ M.map toDynValue m
  fromValue (MAP m) = M.foldMapWithKey foldEntry m
    where foldEntry k v = M.singleton k <$> fromDynValue v
  fromValue _ = Nothing
  encodeValue m = Encode $ AMap $ V.fromList $ map packEntry $ M.toList m
  decodeValue (Encode (AMap vs)) = V.foldM insertUnpackedEntry M.empty vs
  decodeValue (Encode (AStreamedMap vs)) = foldM insertUnpackedEntry M.empty vs
  decodeValue _ = Nothing

instance Codec a => Codec (LazyMap (M.Map T.Text a)) where
  toValue (LazyMap m) = MAP $ LM.map toDynValue m
  fromValue (MAP m) = M.foldMapWithKey foldEntry m
    where foldEntry k v = LazyMap . LM.singleton k <$> fromDynValue v
  fromValue _ = Nothing
  encodeValue (LazyMap m) = Encode $ AStreamedMap $ map packEntry $ LM.toList m
  decodeValue (Encode (AMap vs)) = LazyMap <$> V.foldM lazyInsertUnpackedEntry LM.empty vs
  decodeValue (Encode (AStreamedMap vs)) = LazyMap <$> foldM lazyInsertUnpackedEntry LM.empty vs
  decodeValue _ = Nothing

instance Codec Node where
  toValue = NODE
  fromValue (NODE n) = Just n
  fromValue _ = Nothing
  encodeValue n = Encode $
                  AStructure _NODE $ V.cons (pack $ nodeId n)
                                   $ V.cons (pack $ nodeLabels n)
                                   $ V.singleton (pack $ nodeProperties n)
  decodeValue (Encode (AStructure sig args)) | sig == _NODE =
    do
      nId <- args V.!? 0 >>= unpack
      nLabels <- args V.!? 1 >>= unpack
      nProperties <- args V.!? 2 >>= unpack
      return MkNode { nodeId = nId, nodeLabels = nLabels, nodeProperties = nProperties }
  decodeValue _ = Nothing

instance Codec Relationship where
  toValue = RELATIONSHIP
  fromValue (RELATIONSHIP rel) = Just rel
  fromValue _ = Nothing
  encodeValue rel = Encode $
                    AStructure _RELATIONSHIP $ V.cons (pack $ relationshipId rel)
                                             $ V.cons (pack $ relationshipStartNodeId rel)
                                             $ V.cons (pack $ relationshipEndNodeId rel)
                                             $ V.cons (pack $ relationshipType rel)
                                             $ V.singleton (pack $ relationshipProperties rel)
  decodeValue (Encode (AStructure sig args)) | sig == _RELATIONSHIP =
    do
      relId <- args V.!? 0 >>= unpack
      relStartNodeId <- args V.!? 1 >>= unpack
      relEndNodeId <- args V.!? 2 >>= unpack
      relType <- args V.!? 3 >>= unpack
      relProperties <- args V.!? 4 >>= unpack
      return MkRelationship {
        relationshipId = relId,
        relationshipStartNodeId = relStartNodeId,
        relationshipEndNodeId = relEndNodeId,
        relationshipType = relType,
        relationshipProperties = relProperties
      }
  decodeValue _ = Nothing

instance Codec Path where
  toValue = PATH
  fromValue (PATH path) = Just path
  fromValue _ = Nothing
  encodeValue path = Encode $
    AStructure _PATH $ V.cons (pack $ IM.values $ pathNodesMap path)
                     $ V.cons (AVector $ V.map packAsUnboundRelationship $ IM.values $ pathRelationshipsMap path)
                     $ V.singleton (pack $ pathElements path)
  -- decodeValue (Encode (AStructure sig args)) | sig == _PATH =
  --   do
  --     pathNodes <- args V.!? 0 >>= unpack
  --     unboundRels <- args V.!? 1 >>= unpack
  --     elts <- args V.!? 2 >>= unpack
  --     unpackPath pathNodes unboundRels elts
  -- decodeValue _ = Nothing

data UnboundRelationship = UnboundRelationship {
  unboundRelationshipId         :: Id,
  unboundRelationshipType       :: T.Text,
  unboundRelationshipProperties :: M.Map T.Text DynValue
}

-- instance Codec UnboundRelationship where
--   toValue =

packAsUnboundRelationship :: Relationship -> Atom
packAsUnboundRelationship rel =
 AStructure _UNBOUND_RELATIONSHIP
   $ V.cons (pack $ relationshipId rel)
   $ V.cons (pack $ relationshipType rel)
   $ V.singleton (pack $ relationshipProperties rel)

-- unpackPath :: V.Vector Node -> V.Vector Atom -> V.Vector Int -> Maybe Path
-- unpackPath nodes unboundRels elts = undefined
