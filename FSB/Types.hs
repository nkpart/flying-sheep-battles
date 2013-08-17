module FSB.Types where

import Prelude hiding ((.), id, mapM_, elem)
import Graphics
import Control.Wire

type T3 a = (a,a,a)

data Sky = Sky RGB [(Int, Int)]

type RGB = T3 Double

data GameState = GameState (Ship, Ship, (Maybe Bool))

data Ship = Ship { 
          _shipObject :: ObjectState (Double, Double) ,
          -- _shipPosition :: (Double, Double), 
          _shipThrust :: (Double, Double), 
          -- _shipVelocity :: (Double, Double),
          _shipBoosting :: Bool
        } deriving (Eq, Show)

_shipPosition = objPosition . _shipObject
_shipVelocity = objVelocity . _shipObject

data Thing = Thing { _thingRect :: BLRect Double, _thingColor :: RGB }

data World = World { _worldBox :: BLRect Double, _worldScenery :: [Thing] }

data ThrustControl = ThrustUp
                   | ThrustDown
                   | ThrustLeft
                   | ThrustRight
                   | ThrustBoost
                   deriving (Eq, Show)

type Controls a = a -> Maybe ThrustControl
