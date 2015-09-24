{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Test.SCInstances where

import           Control.Monad
import qualified Data.Vector                      as V

import           Test.SmallCheck.Series
import           Test.SmallCheck.Series.Instances ()

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
