{-# LANGUAGE GADTs #-}
module Wires where

import Control.Wire
import Data.VectorSpace hiding (Sum)
import Data.Monoid
import Prelude hiding ((.))

interpolateFromTo :: (Monad m, VectorSpace c, Monoid e, Scalar c ~ Double) => c -> c -> Scalar c -> Wire e m b c
interpolateFromTo start finish duration = let step dt old = old ^+^ (finish ^-^ start) ^* (dt/duration)
                                           in iterateWT step start . for duration 

foreverW :: Monad m => Wire e m a b -> Wire e m a b
foreverW a = a `andThen` foreverW a

sampleFPS :: (Monad m, Monoid e, Integral b) => Time -> Wire e m a (Maybe b)
sampleFPS secs = event (periodically secs . fmap (round . (1/)) dtime)

if' :: (Monad m, Monoid e) => (a -> Bool) -> Wire e m a b -> Wire e m a b -> Wire e m a b 
if' p a b = a . when p <|> b


