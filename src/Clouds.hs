{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Clouds where

import Control.Wire
import Prelude hiding ((.), id)

data Cloud = Cloud (ObjectState (Double, Double)) Int

cloudGen =  proc wat -> do
  y <- noiseRM -< (500 :: Double, 600 :: Double)
  w <- noiseRM -< (30 :: Int, 85 :: Int)
  returnA -< Cloud (ObjectState (y, 0 - fromIntegral w) (5,0)) w

cloudWire (Cloud state width) = fmap (\s -> Cloud s width) $ object_ (const id) state

cloudMaker = clouds . event (periodically 5 . cloudGen)

-- TODO: this should use mkGen to create clouds
cloudSystem = proc xxx -> do
  xs <- cloudMaker -< xxx
  moved <- ?a -< xs
  returnA -< moved

clouds :: Wire e m (Maybe a) [a]
clouds = accum (\v c -> maybe v (:v) c) []

