{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
module Codec.Bolt.PTS(
  Value(..),
  packValue,
  unpackValue,
  Codec,
  DynValue,
  Id,
  unId,
  NodeView(..),
  RelationshipView(..),
  StepView,
  Direction(..),
  (-->),
  (<--),
  stepRelationship,
  stepRelationshipId,
  stepDirection,
  stepOriginId,
  stepDestination,
  stepDestinationId,
  PathView,
  (<@>),
  (<+>),
  pathStartNode,
  pathStartNodeId,
  pathEndNode,
  pathEndNodeId,
  pathSteps,
  pathNodes,
  pathNodeIds,
  pathRels,
  pathRelIds,
  LazyMap,
  unLazyMap,
  EncodedValue,
  toValue,
  fromValue,
  encodeValue,
  decodeValue
)

where

import           Codec.Packstream.Atom
import           Codec.Packstream.Signature
import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Int
import qualified Data.Map.Lazy              as LM
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import qualified Data.Vector                as V

data Value  = NULL
            | BOOL                        !Bool
            | FLOAT        {-# UNPACK #-} !Double
            | INTEGER      {-# UNPACK #-} !Int64
            | TEXT         {-# UNPACK #-} !T.Text
            | LIST         {-# UNPACK #-} !(V.Vector DynValue)
            | MAP                         !(M.Map T.Text DynValue)
            | NODE         {-# UNPACK #-} !NodeView
            | RELATIONSHIP {-# UNPACK #-} !RelationshipView
            | PATH         {-# UNPACK #-} !PathView
            deriving (Show, Eq)

newtype EncodedValue = Encode { unEncode :: Atom }

instance Binary EncodedValue where
  put = put . unEncode
  get = liftA Encode get

data DynValue = forall a. (Codec a) => DynValue {-# UNPACK #-} !a

instance Show DynValue where
  show (DynValue v) = show v

instance Eq DynValue where
  (==) l r = toValue l == toValue r

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

newtype Id = Id { unId :: Int64 } deriving (Eq, Show)

data NodeView = MkNodeView {
    nodeId         :: Id,
    nodeLabels     :: V.Vector T.Text,
    nodeProperties :: M.Map T.Text DynValue
  } deriving (Eq, Show)

data RelationshipView = MkRelationshipView {
    relationshipId          :: Id,
    relationshipType        :: T.Text,
    relationshipStartNodeId :: Id,
    relationshipEndNodeId   :: Id,
    relationshipProperties  :: M.Map T.Text DynValue
  } deriving (Eq, Show)

data Direction = INCOMING | OUTGOING deriving (Eq, Show)

data StepView = MkStepView RelationshipView Direction NodeView deriving (Eq, Show)

(-->) :: RelationshipView -> NodeView-> Maybe StepView
(-->) rel node | relationshipEndNodeId rel == nodeId node = Just $ MkStepView rel OUTGOING node
(-->) _ _ = Nothing

(<--) :: NodeView-> RelationshipView -> Maybe StepView
(<--) node rel | relationshipStartNodeId rel == nodeId node = Just $ MkStepView rel INCOMING node
(<--) _ _ = Nothing

stepRelationship :: StepView -> RelationshipView
stepRelationship (MkStepView rel _ _) = rel

stepRelationshipId :: StepView -> Id
stepRelationshipId = relationshipId . stepRelationship

stepDirection :: StepView -> Direction
stepDirection (MkStepView _ dir _) = dir

stepOriginId :: StepView -> Id
stepOriginId (MkStepView rel OUTGOING _) = relationshipStartNodeId rel
stepOriginId (MkStepView rel INCOMING _) = relationshipEndNodeId rel

stepDestination :: StepView -> NodeView
stepDestination (MkStepView _ _ node) = node

stepDestinationId :: StepView -> Id
stepDestinationId = nodeId . stepDestination

data PathView = MkPathView NodeView (V.Vector StepView) deriving (Eq, Show)

singleNodePath :: NodeView -> PathView
singleNodePath start = MkPathView start V.empty

(<@>) :: NodeView -> Maybe PathView
(<@>) = Just . singleNodePath

(<+>) :: Maybe PathView -> Maybe StepView -> Maybe PathView
(<+>) (Just path @ (MkPathView start steps)) (Just nextStep) =
  if pathEndNodeId path == stepOriginId nextStep
    then Just $ MkPathView start $ V.snoc steps nextStep
    else Nothing
(<+>) _ _ = Nothing

instance Monoid (Maybe PathView) where
  mempty = Nothing
  Just (MkPathView lStart lSteps) `mappend` Just (MkPathView rStart rSteps) =
    if lEnd == rStart
      then Just (MkPathView lStart (V.concat [lSteps, rSteps]))
      else Nothing
      where lEnd = stepDestination $ V.last lSteps
  _ `mappend` _ = Nothing

pathStartNode :: PathView -> NodeView
pathStartNode (MkPathView startNode _) = startNode

pathStartNodeId :: PathView -> Id
pathStartNodeId = nodeId . pathStartNode

pathSteps :: PathView -> V.Vector StepView
pathSteps (MkPathView _ steps) = steps

pathEndNode :: PathView -> NodeView
pathEndNode (MkPathView start steps) =
  if V.null steps
    then start
    else stepDestination $ V.last steps

pathEndNodeId :: PathView -> Id
pathEndNodeId = nodeId . pathEndNode

pathNodes :: PathView -> V.Vector NodeView
pathNodes (MkPathView start steps) = V.cons start $ V.map stepDestination steps

pathNodeIds :: PathView -> V.Vector Id
pathNodeIds (MkPathView start steps) = V.cons (nodeId start) $ V.map stepDestinationId steps

pathRels :: PathView -> V.Vector RelationshipView
pathRels (MkPathView _ steps) = V.map stepRelationship steps

pathRelIds :: PathView -> V.Vector Id
pathRelIds (MkPathView _ steps) = V.map stepRelationshipId steps

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

pack :: Codec a  => a -> Atom
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

instance Codec NodeView where
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
      return MkNodeView { nodeId = nId, nodeLabels = nLabels, nodeProperties = nProperties }
  decodeValue _ = Nothing

instance Codec RelationshipView where
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
      return MkRelationshipView {
        relationshipId = relId,
        relationshipStartNodeId = relStartNodeId,
        relationshipEndNodeId = relEndNodeId,
        relationshipType = relType,
        relationshipProperties = relProperties
      }
  decodeValue _ = Nothing
