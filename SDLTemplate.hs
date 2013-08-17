{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
module SDLTemplate where

import FSB.Renderer.SDL (initRenderer)
import Data.Maybe (mapMaybe)
import Data.Fixed
import qualified Config as C
import Graphics
import Wires
import Control.Lens hiding (within, perform)
import Data.VectorSpace hiding (Sum, getSum)
import Data.Monoid 
import Data.Foldable (asum, mapM_, foldMap, elem)
import Debug.Trace (trace)
import Prelude hiding ((.), id, mapM_, elem)
import Control.Wire hiding (empty)
import qualified Data.Set as Set (insert, toList, delete)
import Data.Set (Set)
import FSB.Types
import Graphics.UI.SDL as SDL
import FSB.Renderer

player1 :: Controls SDL.SDLKey
player1 v = case v of
              SDL.SDLK_a -> Just ThrustLeft
              SDL.SDLK_d -> Just ThrustRight
              SDL.SDLK_s -> Just ThrustDown
              SDL.SDLK_w -> Just ThrustUp
              SDL.SDLK_LSHIFT -> Just ThrustBoost
              _ -> Nothing
    
player2 :: Controls SDL.SDLKey
player2 v = case v of
              SDL.SDLK_LEFT -> Just ThrustLeft
              SDL.SDLK_RIGHT -> Just ThrustRight
              SDL.SDLK_DOWN -> Just ThrustDown
              SDL.SDLK_UP -> Just ThrustUp
              SDL.SDLK_RSHIFT -> Just ThrustBoost
              _ -> Nothing

gravityMagnitude = 240
gravity L = (-1, 0) ^* gravityMagnitude
gravity R = (1, 0) ^* gravityMagnitude
gravity T = (0, 1) ^* gravityMagnitude
gravity B = (0, -1) ^* gravityMagnitude

-- init: 0 12 24
--  -12: -12 0 12
--  abs: 12 0 12
fractionToNight hour = abs (hour - 12) / 12

skyColor dt = C.dayColor + (C.nightColor - C.dayColor) ^* fractionToNight dt

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

timeCycle = fmap ((`mod'` 24) . (+17)) (timeFrom 0)

starsW :: (Monoid e, Monad m, MonadRandom m) => (Int, Int) -> Wire e m a [(Int, Int)]
starsW skySize = timeCycle >>> (id &&& randomPositions skySize) >>> accum changeStars mempty
  where changeStars oldStars (h, newStar) | h > 17.5 = newStar:oldStars
                                                | h < 6 = if null oldStars then oldStars else tail oldStars
                                                | otherwise = []

randomPositions :: MonadRandom m => (Int, Int) -> Wire e m a (Int, Int)
randomPositions (w,h) = (,) <$> (pure (0,w) >>> noiseRM) <*> (pure (0,h) >>> noiseRM)

world = World (BLRect 0 10 (realToFrac C.width) (realToFrac $ C.height - 10)) [
                                                   Thing (BLRect 190 0 30 350) C.goldenRod
                                                 ]

shipSim = GameState <$> (keysDownW >>> runShips (C.shipW, C.shipH) world)
skySim = Sky <$> (timeCycle >>^ skyColor) <*> starsW (C.width, C.height)


data Loop = TapToStart
          | Play
          | ShowVictory GameState Sky

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  renderer <- initRenderer
  let loop Play = loop =<< (play renderer clockSession $ shipSim &&& skySim
                                                               <* ((sampleFPS 1.0 >>^ mapM_ print) >>> perform))
      loop (ShowVictory gameState sky) = loop =<< (victory renderer gameState sky clockSession $ fmap (> 3) $ timeFrom 0)
  loop Play
  return ()
 where
  victory renderer state sky s w = do
    (done, w', s') <- stepSession_ w s ()
    render renderer world state sky
    if not done
      then victory renderer state sky s' w'
      else return Play

  play renderer s w = do
    (state@(GameState (_, _, checkVictory), _), w', s') <- stepSession_ w s ()
    case checkVictory of
      Nothing -> do
        render renderer world `uncurry` state
        play renderer s' w'
      Just _ -> return $ ShowVictory `uncurry` state

applyControls f = mapMaybe f . Set.toList

runShips shipSize world = proc keysDown -> do
  ship1 <- shipWire shipSize world (100, 200) -< (applyControls player1 keysDown)
  ship2 <- shipWire shipSize world (300, 200) -< (applyControls player2 keysDown)
  let stuffed = rectForCenter (_shipPosition ship1) shipSize `overlapping` rectForCenter (_shipPosition ship2) shipSize
  let ended = if stuffed then Just (ship1Wins ship1 ship2) else Nothing
  returnA -< (ship1, ship2, ended)

ship1Wins ship1 ship2 = let collisionVector = normalized $ _shipPosition ship1 - _shipPosition ship2
                            collisionForce ship = abs . magnitude . project collisionVector $ _shipVelocity ship
                        in collisionForce ship1 > collisionForce ship2

shipWire shipSize world initPos = proc (controls) -> do
  let thrust = acceleration controls
  let boost = ThrustBoost `elem` controls
  s <- spaceShipObject shipSize initPos -< (Accelerate (thrust * if boost then 2.5 else 1.0), world)
  returnA -< (Ship s thrust boost)

objectDiffForControls controls g = let thrust = acceleration controls
                                       boost = ThrustBoost `elem` controls
                                    in Accelerate ((thrust * if boost then 2.5 else 1.0) + g)

acceleration = (100 *^) . getSum . foldMap Sum . map f
                where f ThrustUp = (0, 3)
                      f ThrustDown = (0, -3)
                      f ThrustLeft = (-1, 0)
                      f ThrustRight = (1, 0)
                      f _ = (0,0)

debug x = trace (show x) x

spaceShipObject size initialPosition = object_ postUpdate (ObjectState initialPosition (0,0))
  where postUpdate world (ObjectState pos vel@(vx, vy)) = 
           let shipBox' = centerAndSizeToRect pos $ over both realToFrac size
               collision = boundsCheck shipBox' (_worldBox world) 
                       <|> asum (map (overlappingEdge shipBox' . _thingRect) . _worldScenery $ world )
            in ObjectState pos $ maybe vel (\e -> 0.8 *^ case e of
                                                   L -> (abs vx, vy)
                                                   R -> (-(abs vx), vy)
                                                   T -> (vx, -(abs vy))
                                                   B -> (vx, abs vy)) collision

gravityEdge :: Wire e m (Set SDL.SDLKey) Edge 
gravityEdge = accum (\g keys -> if SDL.SDLK_q `elem` keys then rotate g else g) B
            where rotate g = case g of
                               B -> R
                               R -> T
                               T -> L
                               L -> B

keysDownW = mkStateM mempty $ \_ (_, keys) -> do
  newKeys <- parseEvents keys
  return (Right newKeys, newKeys)

parseEvents :: Set SDL.SDLKey -> IO (Set SDL.SDLKey)
parseEvents keysDown = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k | SDL.symKey k == SDL.SDLK_q && elem SDL.KeyModLeftMeta (SDL.symModifiers k) -> error "Done"
    SDL.KeyDown k -> parseEvents (Set.insert (SDL.symKey k) keysDown)
    SDL.KeyUp k -> parseEvents (Set.delete (SDL.symKey k) keysDown)
    SDL.Quit -> error "Done"
    _ -> parseEvents keysDown

-- TODO
-- Good collision detection using a priority queue
-- vector based collisions instead of the weird overlapping edges thing?
