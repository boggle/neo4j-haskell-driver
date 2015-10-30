module Database.Neo4j.Internal.Bolt.Message (
  Message(..)
) where

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
  atomize FAILURE { metadata = theMetadata }= arg1 _IGNORED_MESSAGE theMetadata
  atomize ACK_FAILURE = arg0 _ACK_FAILURE_MESSAGE
  atomize INIT { clientName = theClientName } = arg1 _INIT_MESSAGE theClientName
  construct = undefined

arg0 :: Signature -> Atom
arg0 sig = AStructure sig V.empty

arg1 :: Atomic a => Signature -> a -> Atom
arg1 sig a = AStructure sig $ V.singleton $ atomize a

arg2 :: (Atomic a, Atomic b) => Signature -> a -> b -> Atom
arg2 sig a b = AStructure sig $ V.cons (atomize a) $ V.singleton $ atomize b
