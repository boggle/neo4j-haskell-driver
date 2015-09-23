{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Codec.Packstream.CodingTest (
  unitTests
) where

import           Control.Monad
import           Data.Binary                      (get, put)
import qualified Data.Binary.Get                  as G
import qualified Data.Binary.Put                  as P
import qualified Data.Vector                      as V

import           Test.SmallCheck.Series
import           Test.SmallCheck.Series.Instances ()
import           Test.Tasty
import qualified Test.Tasty.SmallCheck            as SC

import qualified Codec.Packstream.Coding          as PSC
import qualified Codec.Packstream.Marker          as PSM
import qualified Codec.Packstream.Signature       as PSS

newtype Marker = MkMarker { marker :: PSM.Marker } deriving (Eq, Show)

instance Monad m => Serial m Marker where
  series = generate $ \d -> map MkMarker $ take d PSM.allMarkers

newtype Signature = MkSignature { signature :: PSS.Signature } deriving (Eq, Show)

instance Monad m => Serial m Signature where
  series = liftM (MkSignature . PSS.signature) series

newtype Vec a = MkVec { vector :: V.Vector a } deriving (Eq, Show)

instance Serial m [a] => Serial m (Vec a) where
  series = fmap (MkVec . V.fromList) series

unitTests :: TestTree
unitTests =
  testGroup "Codec.Packstream.Coding" [
    SC.testProperty "marker coding" $ SC.changeDepth (const 255) $ propVerifyCoding putMarker getMarker,
    SC.testProperty "signature coding" $ SC.changeDepth (const 255) $ propVerifyCoding putSignature getSignature,
    SC.testProperty "null coding" $ propVerifyCoding (const PSC.putNull) PSC.getNull,
    SC.testProperty "bool coding" $ propVerifyCoding PSC.putBool PSC.getBool,
    SC.testProperty "float64 coding" $ propVerifyCoding PSC.putFloat64 PSC.getFloat64,
    SC.testProperty "text coding" $ propVerifyCoding PSC.putText PSC.getText,
    testGroup "int coding" [
      SC.testProperty "tinyInt coding" $ propVerifyMaybeCoding PSC.putTinyInt PSC.getTinyInt,
      SC.testProperty "int8 coding" $ SC.changeDepth (const 255) $ propVerifyCoding PSC.putInt8 PSC.getInt8,
      SC.testProperty "int16 coding" $ propVerifyCoding PSC.putInt16 PSC.getInt16,
      SC.testProperty "int32 coding" $ propVerifyCoding PSC.putInt32 PSC.getInt32,
      SC.testProperty "int64 coding" $ propVerifyCoding PSC.putInt64 PSC.getInt64
    ],
    testGroup "container coding" [
      SC.testProperty "bool vector coding" $ propVerifyCoding (putVec PSC.putBool) (getVec PSC.getBool),
      SC.testProperty "int8 list coding" $ propVerifyCoding (putList PSC.putInt8) (getList PSC.getInt8)
    ]
  ]
  where
    putList putElt lst = PSC.streamList $ map putElt lst
    getList = PSC.unStreamList
    putVec putElt vec = PSC.putVector $ V.map putElt $ vector vec
    getVec getElt = liftM MkVec $ PSC.getVector getElt
    putMarker = put . marker
    getMarker = liftM MkMarker get
    putSignature = put . signature
    getSignature = liftM MkSignature get
    propVerifyMaybeCoding maybePut getter inputValue =
      case maybePut inputValue of
        Just putter -> inputValue == G.runGet getter (P.runPut putter)
        Nothing     -> True
    propVerifyCoding putter getter inputValue = inputValue == decodedValue
      where
        encodedBytes = P.runPut (putter inputValue)
        decodedValue = G.runGet getter encodedBytes
