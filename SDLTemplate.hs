{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
module SDLTemplate where

import Data.Maybe (mapMaybe)
import Data.Fixed
import qualified Control.Monad as M (forever)
import qualified Config as C
import Graphics
import Wires
import Control.Lens hiding (within, perform)
import Data.VectorSpace hiding (Sum, getSum)
import Data.Monoid 
import Data.Foldable (forM_, asum, mapM_, foldMap)
import Debug.Trace (trace)
import Prelude hiding ((.), id, mapM_)
import Control.Wire hiding (empty)
import qualified Data.Set as Set (insert, null, filter, toList, fromList)
import Data.Set (Set)
import Data.List as List (deleteBy)
import Data.Function (on)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI

data Ship = Ship { _shipPosition :: (Double, Double), _shipThrust :: (Double, Double), _shipVelocity :: (Double, Double) } deriving (Eq, Show)

data Thing = Thing { _thingRect :: BLRect Double, _thingColor :: C.T3 Double }
data World = World { _worldBox :: BLRect Double, _worldScenery :: [Thing] }

data ThrustControl = ThrustUp
                   | ThrustDown
                   | ThrustLeft
                   | ThrustRight
                   | ThrustBoost
                   deriving (Eq, Show)

type Controls = Set SDL.Keysym -> [ThrustControl]

player1 :: Controls
player1 = mapMaybe (f . SDL.symKey) . Set.toList
        where f SDL.SDLK_a = Just ThrustLeft
              f SDL.SDLK_d = Just ThrustRight
              f SDL.SDLK_s = Just ThrustDown
              f SDL.SDLK_w = Just ThrustUp
              f SDL.SDLK_LSHIFT = Just ThrustBoost
              f _ = Nothing
    
player2 :: Controls
player2 = mapMaybe (f . SDL.symKey) . Set.toList
        where f SDL.SDLK_LEFT = Just ThrustLeft
              f SDL.SDLK_RIGHT = Just ThrustRight
              f SDL.SDLK_DOWN = Just ThrustDown
              f SDL.SDLK_UP = Just ThrustUp
              f SDL.SDLK_RSHIFT = Just ThrustBoost
              f _ = Nothing
    
-- Controls SDL.SDLK_LEFT SDL.SDLK_RIGHT SDL.SDLK_UP SDL.SDLK_DOWN SDL.SDLK_RSHIFT

gravityMagnitude = 240
gravity L = (-1, 0) ^* gravityMagnitude
gravity R = (1, 0) ^* gravityMagnitude
gravity T = (0, 1) ^* gravityMagnitude
gravity B = (0, -1) ^* gravityMagnitude

data Game = Game { shipSize :: (Int, Int) , screenWidth :: Int , screenHeight :: Int }

world = World (BLRect 0 10 (realToFrac C.width) (realToFrac $ C.height - 10)) [
                                                     Thing (BLRect 190 0 30 350) C.goldenRod
                                                   ]
-- init: 0 12 24
--  -12: -12 0 12
--  abs: 12 0 12
fractionToNight hour = abs (hour - 12) / 12

skyColor = (\dt -> C.dayColor + (C.nightColor - C.dayColor) ^* fractionToNight dt)

cloudRects w d  = [
                 BLRect d 0 (w - 2 * d) (d `div` 2),
                 BLRect 0 (d `div` 2) w d,
                 BLRect d (d + d `div` 2) (w - 2 * d) (d `div` 2)
                 ]

data Cloud = Cloud (ObjectState (Double, Double)) Int

cloudGen =  proc wat -> do
  y <- noiseRM -< (500 :: Double, 600 :: Double)
  w <- noiseRM -< (30 :: Int, 85 :: Int)
  returnA -< Cloud (ObjectState (y, 0 - fromIntegral w) (5,0)) w

cloudWire (Cloud state width) = fmap (\s -> Cloud s width) $ object_ (const id) state

cloudMaker = clouds . (event $ periodically 5 . cloudGen)

cloudSystem = proc xxx -> do
  xs <- cloudMaker -< xxx
  moved <- ?a -< xs
  returnA -< moved

clouds :: Wire e m (Maybe a) [a]
clouds = accum (\v c -> maybe v (:v) c) []

timeCycle = fmap ((`mod'` 24) . (+17)) (timeFrom 0)

starsW :: (Monoid e, Monad m, MonadRandom m) => (Int, Int) -> Wire e m a [(Int, Int)]
starsW (w, h) = timeCycle >>> (id &&& randomPositions) >>> accum changeStars mempty
  where changeStars oldStars (h, newStar) | h > 17.5 = newStar:oldStars
                                                | h < 6 = if null oldStars then oldStars else tail oldStars
                                                | otherwise = []

randomPositions :: MonadRandom m => Wire e m a (Int, Int)
randomPositions = proc wat -> do
  v <- noiseRM -< (0,400)
  z <- noiseRM -< (0, 600)
  returnA -< (v, z)

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode C.width C.height 32 [SDL.SWSurface]
  sheep <- SDLI.load "sheep.png"
  let shipThing = runShip (C.shipW, C.shipH) world 
  M.forever $
    go sheep screen clockSession ((keysDownW >>> shipThing) &&& ((sampleFPS 1 >>^ (mapM_ print)) >>> perform) &&& (timeCycle >>^ skyColor) &&& starsW (C.width, C.height)) Nothing
 where
  go image screen s w holding = do
    let draw = paintRect screen C.height
    (state@(((ship1, boosted1), (ship2, boosted2), victory), (_, (sky, stars))), w', s') <-
      case holding of
        Just (_, s) -> return s
        Nothing -> stepSession_ w s ()

    -- The backdrop
    paintScreen screen sky
    forM_ stars $ \(x,y) -> draw (lerp sky (255,255,255) ((fromIntegral y / fromIntegral C.height) ^ 2)) $ BLRect x y 2 2
    forM_ (map (moveRelativeTo (10, 500)) $ cloudRects 100 20) $ draw (255, 255, 255)
    forM_ (_worldScenery world) $ \(Thing r c) -> draw c $ fmap round r
    -- ground
    draw (209, 223, 188) $ BLRect 0 0 C.width 10

    let handleShip ship boosted victory = do
        wiggle <- if _shipThrust ship /= (0,0)
                    then randomRIO (-2, 2)
                    else return 0
        let wiggledSize = over both (+ wiggle * 2) (C.shipW, C.shipH)

        -- Draw Ship
        let (Ship {..}) = ship
        let rect@(BLRect leftEdge bottomEdge _ _)  = rectForCenter _shipPosition wiggledSize
        -- draw (0, 50, 200) rect
        let x = Just $ toSDLRect C.height rect
        SDL.blitSurface image Nothing screen x
        case victory of 
          Just False -> draw (255, 0, 0) $ BLRect leftEdge bottomEdge C.shipW 10
          _ -> return ()

        -- Draw Thrusters
        let thrustColor = if not boosted then (200,50,0) else (255, 100, 0)
        forM_ (thrusters wiggledSize _shipThrust boosted) (draw thrustColor . moveRelativeTo (leftEdge, bottomEdge))

    handleShip ship1 boosted1 $ victory
    handleShip ship2 boosted2 $ fmap not victory

    SDL.flip screen

    case victory of
      Nothing -> go image screen s' w' Nothing
      Just _ -> case holding of
                  Nothing -> go image screen s' w' $ Just (150, (state, w', s'))
                  Just (v, s) -> if (v-1) == 0 
                                   then return ()
                                   else go image screen s' w' $ Just (v-1, s)

rectForCenter pos (sizeX', sizeY') = let (left, bottom) =  over both round pos - over both (`div` 2) (sizeX', sizeY')
                                      in BLRect left bottom sizeX' sizeY'

thrusters (sizeX', sizeY') (tx, ty) boosted = map snd . filter fst $ [
         (tx > 0 , BLRect (-sizeO) ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (tx < 0 , BLRect sizeX' ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (ty > 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) (-sizeO) size sizeO)
       , (ty < 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) sizeY' size sizeO)
     ]
     where size = if boosted then 12 else 10
           sizeO = if boosted then 20 else 10

runShip shipSize world = proc keysDown -> do
  g <- gravityEdge -< keysDown
  (ship1, boost1) <- moveShip shipSize world (100, 200) -< (player1 keysDown, g)
  (ship2, boost2) <- moveShip shipSize world (300, 200) -< (player2 keysDown, g)
  let stuffed = rectForCenter (_shipPosition ship1) shipSize `overlapping` rectForCenter (_shipPosition ship2) shipSize
  let ended = if stuffed then Just (ship1Wins ship1 ship2) else Nothing
  returnA -< ((ship1, boost1), (ship2, boost2), ended)

ship1Wins ship1 ship2 = let collisionVector = normalized $ _shipPosition ship1 - _shipPosition ship2
                            collisionForce v = abs . magnitude . project collisionVector $ _shipVelocity v 
                        in collisionForce ship1 > collisionForce ship2

moveShip shipSize world initPos = proc (controls, g) -> do
  let thrust = acceleration controls
  let boost = elem ThrustBoost controls
  (ObjectState pos vel) <- spaceShipObject shipSize initPos -< (Accelerate ((thrust * if boost then 2.5 else 1.0) + gravity g), world)
  returnA -< (Ship pos thrust vel, boost)

acceleration = ((100 *^) . getSum . foldMap Sum . map f)
                where f ThrustUp = (0, 3)
                      f ThrustDown = (0, -3)
                      f ThrustLeft = (-1, 0)
                      f ThrustRight = (1, 0)
                      f _ = (0,0)

debug x = trace (show x) x

spaceShipObject (sizeX, sizeY) initialPosition = object_ postUpdate (ObjectState initialPosition (0,0))
  where postUpdate world (ObjectState pos vel@(vx, vy)) = 
           let shipBox' = centerAndSizeToRect pos (realToFrac sizeX, realToFrac sizeY)
               collision = boundsCheck shipBox' (_worldBox world) 
                       <|> asum (map (overlappingEdge shipBox' . _thingRect) . _worldScenery $ world )
            in ObjectState pos $ maybe vel (\e -> 0.8 *^ case e of
                                                   L -> (abs vx, vy)
                                                   R -> (-(abs vx), vy)
                                                   T -> (vx, -(abs vy))
                                                   B -> (vx, abs vy)) collision

gravityEdge :: Wire e m (Set SDL.Keysym) Edge 
gravityEdge = accum (\g keys -> if keyDown SDL.SDLK_q keys then rotate g else g) B
            where rotate g = case g of
                               B -> R
                               R -> T
                               T -> L
                               L -> B

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  ev <- SDL.pollEvent
  print ev
  case ev of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k | SDL.symKey k == SDL.SDLK_q && elem SDL.KeyModLeftMeta (SDL.symModifiers k) -> error "Done"
    SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete' k keysDown)
    SDL.Quit -> error "Done"
    _ -> parseEvents keysDown

keysDownW :: Wire e IO a (Set SDL.Keysym)
keysDownW = mkStateM mempty $ \_ (_, keys) -> do
  newKeys <- parseEvents keys
  -- let keysDown'' = keysDown' -- Set.insert (SDL.Keysym SDL.SDLK_RSHIFT [] ' ') keysDown'
  return $ (Right newKeys, newKeys)

-- Modifiers might change on key up. we don't care
delete' k = Set.fromList . deleteBy ((==) `on` SDL.symKey) k . Set.toList

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . Set.null . Set.filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym

-- TODO
-- Good collision detection using a priority queue
