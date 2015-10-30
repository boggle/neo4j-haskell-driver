module Database.Neo4j.Internal.PGM (
  Id(..),
  Label(..),
  RelationshipType(..),
  Properties,
  Node(..),
  Relationship(..),
  Entity(..),
  Direction(..),
  Segment(..),
  segmentRelationship,
  Step,
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
  (<++>),
  Directed(..),
  Invertible(..)
)

where

import qualified Database.Neo4j.Internal.Util.IndexedMap      as IM
import           Database.Neo4j.Internal.ValueCodec
import           Database.Neo4j.Internal.Packstream.Atomic
import           Database.Neo4j.Internal.Packstream.Atom
import           Database.Neo4j.Internal.Packstream.Signature
import           Data.Int
import           Data.Maybe
import qualified Data.Text                                    as T
import qualified Data.Vector                                  as V

newtype Id = Id { getId :: Int64 } deriving (Eq, Show, Ord)

instance Atomic Id where
  atomize = AInt64 . getId
  construct x = Id <$> construct x

instance ValueCodec Id where

type Properties v = Map v

newtype Label = Label { labelName :: T.Text } deriving (Eq, Show, Ord)

instance Atomic Label where
  atomize = AText . labelName
  construct (AText txt) = Just $ Label txt
  construct _ = Nothing

newtype RelationshipType = RelationshipType { relationshipTypeName :: T.Text } deriving (Eq, Show, Ord)

instance Atomic RelationshipType where
  atomize = AText . relationshipTypeName
  construct (AText txt) = Just $ RelationshipType txt
  construct _ = Nothing

data Node v = Node {
  nodeId         :: Id,
  -- TODO
  nodeLabels     :: V.Vector T.Text,
  nodeProperties :: Properties v
}

deriving instance Eq v => Eq (Node v)
deriving instance Ord v => Ord (Node v)
deriving instance Show v => Show (Node v)

instance (Atomic v) => Atomic (Node v) where
  atomize n = AStructure _NODE $ V.cons (atomize $ nodeId n)
                               $ V.cons (atomize $ nodeLabels n)
                               $ V.singleton (atomize $ nodeProperties n)
  construct (AStructure sig args) | sig == _NODE =
    do
      nId <- args V.!? 0 >>= construct
      nLabels <- args V.!? 1 >>= construct
      nProperties <- args V.!? 2 >>= construct
      return Node { nodeId = nId, nodeLabels = nLabels, nodeProperties = nProperties }
  construct _ = Nothing

instance ValueCodec v => ValueCodec (Node v) where

data Relationship v = Relationship {
    relationshipId          :: Id,
    relationshipType        :: RelationshipType,
    relationshipStartNodeId :: Id,
    relationshipEndNodeId   :: Id,
    relationshipProperties  :: Properties v
}

deriving instance Eq v => Eq (Relationship v)
deriving instance Ord v => Ord (Relationship v)
deriving instance Show v => Show (Relationship v)

instance Atomic v => Atomic (Relationship v) where
  atomize rel = AStructure _RELATIONSHIP $ V.cons (atomize $ relationshipId rel)
                                         $ V.cons (atomize $ relationshipStartNodeId rel)
                                         $ V.cons (atomize $ relationshipEndNodeId rel)
                                         $ V.cons (atomize $ relationshipTypeName $ relationshipType rel)
                                         $ V.singleton (atomize $ relationshipProperties rel)
  construct (AStructure sig args) | sig == _RELATIONSHIP =
    do
      relId <- args V.!? 0 >>= construct
      relStartNodeId <- args V.!? 1 >>= construct
      relEndNodeId <- args V.!? 2 >>= construct
      relTypeName <- args V.!? 3 >>= construct
      relProperties <- args V.!? 4 >>= construct
      return Relationship {
        relationshipId = relId,
        relationshipStartNodeId = relStartNodeId,
        relationshipEndNodeId = relEndNodeId,
        relationshipType = RelationshipType relTypeName,
        relationshipProperties = relProperties
      }
  construct _ = Nothing

instance ValueCodec v => ValueCodec (Relationship v) where

class Entity a where
  type PropertyValue a
  entityId :: a -> Id
  entityProperties :: a -> Properties (PropertyValue a)

instance Entity (Node v) where
  type PropertyValue (Node v) = v
  entityId = nodeId
  entityProperties = nodeProperties

instance Entity (Relationship v) where
  type PropertyValue (Relationship v) = v
  entityId = relationshipId
  entityProperties = relationshipProperties

data Direction = INCOMING | OUTGOING deriving (Eq, Show, Ord)

data Segment v = Segment {
    segmentDirection              :: Maybe Direction,
    segmentRelationshipId         :: Id,
    segmentRelationshipType       :: RelationshipType,
    segmentStartNode              :: Node v,
    segmentEndNode                :: Node v,
    segmentRelationshipProperties :: Properties v
}

deriving instance Eq v => Eq (Segment v)
deriving instance Ord v => Ord (Segment v)
deriving instance Show v => Show (Segment v)

segmentRelationship :: Segment v -> Relationship v
segmentRelationship segment =
  if segmentDirection segment == Just INCOMING
  then Relationship {
      relationshipId = segmentRelationshipId segment,
      relationshipType = segmentRelationshipType segment,
      relationshipStartNodeId = nodeId $ segmentEndNode segment,
      relationshipEndNodeId = nodeId $ segmentStartNode segment,
      relationshipProperties = segmentRelationshipProperties segment
    }
  else Relationship {
      relationshipId = segmentRelationshipId segment,
      relationshipType = segmentRelationshipType segment,
      relationshipStartNodeId = nodeId $ segmentStartNode segment,
      relationshipEndNodeId = nodeId $ segmentEndNode segment,
      relationshipProperties = segmentRelationshipProperties segment
    }

data Step v = MkStep {
  stepNode         :: Node v,
  stepRelationship :: Relationship v,
  stepDirection    :: Direction
}

(--|) :: (Eq v) => Node v -> Relationship v -> Maybe (Step v)
(--|) startNode rel =
  if nodeId startNode == relationshipStartNodeId rel
  then Just MkStep { stepNode = startNode, stepRelationship = rel, stepDirection = OUTGOING }
  else Nothing

(|->) :: (Eq v) => Maybe (Step v) -> Node v -> Maybe (Segment v)
(|->) (Just step) endNode | stepDirection step == OUTGOING =
    if nodeId endNode == relationshipEndNodeId rel
    then Just Segment {
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

(<-|) :: (Eq v) => Node v -> Relationship v -> Maybe (Step v)
(<-|) endNode rel =
  if nodeId endNode == relationshipEndNodeId rel
  then Just MkStep { stepNode = endNode, stepRelationship = rel, stepDirection = INCOMING }
  else Nothing

(|--) :: (Eq v) => Maybe (Step v) -> Node v -> Maybe (Segment v)
(|--) (Just step) startNode | stepDirection step == INCOMING =
    if nodeId startNode == relationshipStartNodeId rel
    then Just Segment {
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

data Path v = MkPath {
  pathNodesMap         :: IM.IndexedMap Id (Node v),
  pathRelationshipsMap :: IM.IndexedMap Id (Relationship v),
  pathElements         :: V.Vector Int
}

deriving instance Eq v => Eq (Path v)
deriving instance Ord v => Ord (Path v)
deriving instance Show v => Show (Path v)

instance Atomic v => Atomic (Path v) where
  atomize MkPath {
    pathNodesMap = nodesMap,
    pathRelationshipsMap = relsMap,
    pathElements = elts
  } = AStructure _PATH
      $ V.cons (atomize $ IM.values nodesMap)
      $ V.cons (AVector $ V.map atomizeUnboundRelationship $ IM.values relsMap)
      $ V.singleton (atomize elts)
      where
        atomizeUnboundRelationship rel = atomize UnboundRelationship {
          unboundRelationshipId = relationshipId rel,
          unboundRelationshipType = relationshipType rel,
          unboundRelationshipProperties = relationshipProperties rel
        }
  construct (AStructure sig args) | sig == _PATH = do
    nodes <- construct $ args V.! 0 :: Maybe (V.Vector (Node v))
    unboundRels <- construct $ args V.! 1 :: Maybe (V.Vector (UnboundRelationship v))
    elts <- construct $ args V.! 2 :: Maybe (V.Vector Int)
    -- TODO construct path
    return MkPath {
      pathNodesMap = undefined,
      pathRelationshipsMap = undefined,
      pathElements = elts
    }
  construct _ = Nothing

instance ValueCodec v => ValueCodec (Path v) where

data UnboundRelationship v = UnboundRelationship {
  unboundRelationshipId         :: Id,
  unboundRelationshipType       :: RelationshipType,
  unboundRelationshipProperties :: Properties v
}

instance Atomic v => Atomic (UnboundRelationship v) where
  atomize UnboundRelationship {
    unboundRelationshipId = relId,
    unboundRelationshipType = relType,
    unboundRelationshipProperties = relProps
  } =  AStructure _UNBOUND_RELATIONSHIP
       $ V.cons (atomize relId)
       $ V.cons (atomize relType)
       $ V.singleton (atomize relProps)
  construct (AStructure sig elts) | sig == _UNBOUND_RELATIONSHIP = do
    relId <- construct $ elts V.! 0
    relType <- construct $ elts V.! 1
    relProps <- construct $ elts V.! 2
    return UnboundRelationship {
      unboundRelationshipId = relId,
      unboundRelationshipType = relType,
      unboundRelationshipProperties = relProps
    }
  construct _ = Nothing

singleNodePath :: Node v -> Path v
singleNodePath startNode = MkPath {
  pathNodesMap = IM.singleton (nodeId startNode) startNode,
  pathRelationshipsMap = IM.empty,
  pathElements = V.empty
}

(<@>) :: Node v -> Maybe (Path v)
(<@>) = Just . singleNodePath

(<+>) :: (Eq v) => Maybe (Path v) -> Maybe (Segment v) -> Maybe (Path v)
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

pathLength :: Path v -> Int
pathLength path = div (V.length $ pathElements path) 2

pathNodes :: Path v -> V.Vector (Node v)
pathNodes path = V.map nodeAtIndex $ V.ifilter nodeIndices $ pathElements path
  where
    nodeAtIndex idx = pathNodesMap path IM.! idx
    nodeIndices idx _ = mod idx 2 == 1

pathRelationships :: Path v -> V.Vector (Relationship v)
pathRelationships path = V.map relAtIndex $ V.ifilter relIndices $ pathElements path
  where
    relAtIndex idx = pathRelationshipsMap path IM.! idx
    relIndices idx _ = mod idx 2 == 0

pathSegment :: (Eq v) => Int -> Path v -> Maybe (Segment v)
pathSegment i path =
  if i < 0 || i >= len
  then Nothing
  else Just Segment {
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

pathSegments :: (Eq v) => Path v -> [Segment v]
pathSegments path = map fromJust $ pathSegments_ path

pathSegments_ :: (Eq v) => Path v -> [Maybe (Segment v)]
pathSegments_ path =
  if lst == 0
    then []
    else map segment [0..(lst - 1)]
  where
    lst = pathLength path
    segment i = pathSegment i path

(<++>) :: (Eq v) => Maybe (Path v) -> Maybe (Path v) -> Maybe (Path v)
(<++>) Nothing Nothing = Nothing
(<++>) optPath Nothing = optPath
(<++>) Nothing optPath = optPath
(<++>) optPath (Just path) = foldl (<+>) optPath $ pathSegments_ path

instance (Eq v) => Monoid (Maybe (Path v)) where
  mempty = Nothing
  mappend = (<++>)

class Directed a where
  type Anchor a
  start :: a -> Anchor a
  end :: a -> Anchor a

instance Directed (Relationship v) where
  type Anchor (Relationship v) = Id
  start = relationshipStartNodeId
  end = relationshipEndNodeId

instance Directed (Segment v) where
  type Anchor (Segment v) = Node v
  start = segmentStartNode
  end = segmentEndNode

instance Directed (Path v) where
  type Anchor (Path v) = Node v
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

instance Invertible (Segment v) where
  inverse segment = segment {
    segmentStartNode = segmentEndNode segment,
    segmentEndNode = segmentStartNode segment,
    segmentDirection = inverse $ segmentDirection segment
  }

-- TODO instance invertible Path
