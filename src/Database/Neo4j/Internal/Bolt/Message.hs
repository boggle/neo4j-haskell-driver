module Database.Neo4j.Internal.Bolt.Message (
  Message(..)
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Loops                          (whileJust)
import           Data.Binary                                  (Binary, decode,
                                                               encode, get, put)
import qualified Data.Binary.Get                              as G
import qualified Data.Binary.Put                              as P
import qualified Data.ByteString                              as B
import qualified Data.ByteString.Lazy                         as BL
import qualified Data.Text                                    as T
import qualified Data.Vector                                  as V
import           Database.Neo4j.Internal.Packstream.Atom
import           Database.Neo4j.Internal.Packstream.Atomic    hiding (Map)
import           Database.Neo4j.Internal.Packstream.Signature
import           Database.Neo4j.Value

data Message = INIT { clientName :: T.Text }
             | RUN { statement :: T.Text, parameters :: Map }
             | PULL_ALL
             | DISCARD_ALL
             | RECORD { fields :: V.Vector AnyValue }
             | SUCCESS { metadata :: Map }
             | FAILURE { metadata :: Map }
             | IGNORED { metadata :: Map }
             | ACK_FAILURE

instance Atomic Message where
  atomize RECORD { fields = theFields } = arg1 _RECORD_MESSAGE theFields
  atomize RUN { statement = theStatement, parameters = theParameter } = arg2 _RUN_MESSAGE theStatement theParameter
  atomize SUCCESS { metadata = theMetadata }= arg1 _SUCCESS_MESSAGE theMetadata
  atomize PULL_ALL = arg0 _PULL_ALL_MESSAGE
  atomize DISCARD_ALL = arg0 _DISCARD_ALL_MESSAGE
  atomize IGNORED { metadata = theMetadata }= arg1 _IGNORED_MESSAGE theMetadata
  atomize FAILURE { metadata = theMetadata }= arg1 _FAILURE_MESSAGE theMetadata
  atomize ACK_FAILURE = arg0 _ACK_FAILURE_MESSAGE
  atomize INIT { clientName = theClientName } = arg1 _INIT_MESSAGE theClientName
  construct (AStructure sig args) | sig == _RECORD_MESSAGE && V.length args == 1 = do
    theFields <- construct $ args V.! 0 :: Maybe (V.Vector AnyValue )
    return RECORD { fields = theFields }
  construct (AStructure sig args) | sig == _RUN_MESSAGE && V.length args == 2 = do
    theStatement <- construct $ args V.! 0 :: Maybe T.Text
    theParameters <- construct $ args V.! 1 :: Maybe Map
    return RUN { statement = theStatement, parameters = theParameters }
  construct (AStructure sig args) | sig == _SUCCESS_MESSAGE && V.length args == 1 = do
    theMetadata <- construct $ args V.! 0 :: Maybe Map
    return SUCCESS { metadata = theMetadata }
  construct (AStructure sig args) | sig == _PULL_ALL_MESSAGE && V.null args = Just PULL_ALL
  construct (AStructure sig args) | sig == _DISCARD_ALL_MESSAGE && V.null args = Just DISCARD_ALL
  construct (AStructure sig args) | sig == _IGNORED_MESSAGE && V.length args == 1 = do
    theMetadata <- construct $ args V.! 0 :: Maybe Map
    return IGNORED { metadata = theMetadata }
  construct (AStructure sig args) | sig == _FAILURE_MESSAGE && V.length args == 1 = do
    theMetadata <- construct $ args V.! 0 :: Maybe Map
    return FAILURE { metadata = theMetadata }
  construct (AStructure sig args) | sig == _ACK_FAILURE_MESSAGE && V.null args = Just ACK_FAILURE
  construct (AStructure sig args) | sig == _INIT_MESSAGE && V.length args == 1 = do
    theClientName <- construct $ args V.! 0 :: Maybe T.Text
    return INIT { clientName = theClientName }
  construct _ = Nothing

arg0 :: Signature -> Atom
arg0 sig = AStructure sig V.empty

arg1 :: Atomic a => Signature -> a -> Atom
arg1 sig a = AStructure sig $ V.singleton $ atomize a

arg2 :: (Atomic a, Atomic b) => Signature -> a -> b -> Atom
arg2 sig a b = AStructure sig $ V.cons (atomize a) $ V.singleton $ atomize b

instance Binary Message where
  put msg = forM_ msgChunks putChunk >> putEnd
    where
      msgChunks = BL.toChunks $ encode $ atomize msg
      putChunk chunk = do
        P.putWord16be $ fromIntegral $ B.length chunk
        put chunk
      putEnd = P.putWord16be 0
  get = do
      chunks <- whileJust getSize G.getByteString
      maybe empty return $ construct $ decode $ BL.fromChunks chunks
    where
      getSize = do
        size <- fromIntegral <$> G.getWord16be
        return $ if size == 0 then Nothing else Just size
