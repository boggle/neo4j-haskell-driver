{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Codec.Packstream.Test (
  unitTests
) where

import           Control.Monad
import qualified Data.Binary.Get                  as G
import qualified Data.Binary.Put                  as P
import qualified Data.Vector                      as V

import           Test.SmallCheck.Series
import           Test.SmallCheck.Series.Instances ()
import           Test.Tasty
import qualified Test.Tasty.SmallCheck            as SC

import qualified Codec.Packstream.Marker          as PSM
import qualified Codec.Packstream.Signature       as PSS
import qualified Codec.Packstream.Value           as PSV

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
  testGroup "Codec.Packstream" [
    SC.testProperty "marker coding" $ SC.changeDepth (const 255) $ propVerifyCoding putMarker getMarker,
    SC.testProperty "signature coding" $ SC.changeDepth (const 255) $ propVerifyCoding putSignature getSignature,
    SC.testProperty "null coding" $ propVerifyCoding (const PSV.putNull) PSV.getNull,
    SC.testProperty "bool coding" $ propVerifyCoding PSV.putBool PSV.getBool,
    SC.testProperty "float64 coding" $ propVerifyCoding PSV.putFloat64 PSV.getFloat64,
    testGroup "int coding" [
      SC.testProperty "tinyInt coding" $ propVerifyMaybeCoding PSV.putTinyInt PSV.getTinyInt,
      SC.testProperty "int8 coding" $ SC.changeDepth (const 255) $ propVerifyCoding PSV.putInt8 PSV.getInt8,
      SC.testProperty "int16 coding" $ propVerifyCoding PSV.putInt16 PSV.getInt16,
      SC.testProperty "int32 coding" $ propVerifyCoding PSV.putInt32 PSV.getInt32,
      SC.testProperty "int64 coding" $ propVerifyCoding PSV.putInt64 PSV.getInt64
    ],
    testGroup "container coding" [
      SC.testProperty "bool vector coding" $ propVerifyCoding (putEltVec PSV.putBool) (getEltVec PSV.getBool),
      SC.testProperty "int8 list coding" $ propVerifyCoding (putList PSV.putInt8) (getList PSV.getInt8)
    ]
  ]
  where
    putList putElt lst = PSV.streamList $ map putElt lst
    getList = PSV.unStreamList
    putEltVec putElt vec = PSV.putVector $ V.map putElt $ vector vec
    getEltVec getElt = liftM MkVec $ PSV.getVector getElt
    putMarker = PSM.putMarker . marker
    getMarker = liftM MkMarker PSM.getMarker
    putSignature = PSS.putSignature . signature
    getSignature = liftM MkSignature PSS.getSignature
    propVerifyMaybeCoding maybePut get inputValue =
      case maybePut inputValue of
        Just put -> inputValue == G.runGet get (P.runPut put)
        Nothing  -> True
    propVerifyCoding put get inputValue = inputValue == decodedValue
      where
        encodedBytes = P.runPut (put inputValue)
        decodedValue = G.runGet get encodedBytes
