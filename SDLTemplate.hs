{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module SDLTemplate where


import Data.Maybe (listToMaybe)
import qualified Config as C
import Data.Function (on)
import Graphics
import Wires
import Control.Lens hiding (within)
import Data.VectorSpace hiding (Sum)
import Data.Monoid
import Data.Foldable
import Debug.Trace (trace)
import Prelude hiding ((.), id, null)
import Control.Wire hiding (empty)
import qualified Data.Set as Set (insert, null, filter, toList, fromList)
import Data.Set (Set)
import Data.List as List (deleteBy)
import qualified Graphics.UI.SDL as SDL
import Data.Fixed

data Ship = Ship { _shipPosition :: (Double, Double), _shipThrust :: (Double, Double) } deriving (Eq, Show)

data Thing = Thing { _thingRect :: (BLRect Int), _thingColor :: (C.T3 Double) }
data World = World { _worldBox :: BLRect Int, _worldScenery :: [Thing] }

isThrusting = (/= (0,0)) . _shipThrust

gravityMagnitude = 240
gravity L = (-1, 0) ^* gravityMagnitude
gravity R = (1, 0) ^* gravityMagnitude
gravity T = (0, 1) ^* gravityMagnitude
gravity B = (0, -1) ^* gravityMagnitude

data Game = Game { shipSize :: (Int, Int) , screenWidth :: Int , screenHeight :: Int }

world = World (BLRect 0 10 C.width (C.height - 10)) [
                                                     Thing (BLRect 200 0 15 350) C.goldenRod
                                                   , Thing (BLRect 350 550 20 20) C.yellow
                                                   ]

timeCycle = fmap (`mod'` 24) (timeFrom 0)

-- init: 0 12 24
--  -12: -12 0 12
--  abs: 12 0 12
fractionToNight hour = abs (hour - 12) / 12

skyColor = arr (\dt -> C.dayColor + (C.nightColor - C.dayColor) ^* fractionToNight dt)

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode C.width C.height 32 [SDL.SWSurface]
  let shipThing = runShip (C.shipW, C.shipH) world 
  go mempty screen clockSession (shipThing &&& (sampleFPS 1) &&& (timeCycle >>> skyColor))
 where
  go keysDown screen s w = do
    let draw = paintRect screen C.height
    keysDown' <- parseEvents keysDown
    (((ship, groundSide, boosted), (fps, sky)), w', s') <- stepSession_ w s keysDown'

    forM_ fps print

    wiggle <- if _shipThrust ship /= (0,0) 
                then randomRIO (-2, 2)
                else return 0
    let (sizeX', sizeY') = over both (+ wiggle * 2) (C.shipW, C.shipH)

    -- The backdrop
    paintScreen screen sky
    forM_ (_worldScenery world) $ \(Thing r c) -> draw c r

    let ground = case groundSide of
                   L -> BLRect 0 0 10 C.height
                   R -> BLRect (C.width - 10) 0 10 C.height
                   T -> BLRect 0 (C.height - 10) C.width 10
                   B -> BLRect 0 0 C.width 10

    draw (0, 200, 0) ground

    -- Draw Ship
    let (Ship {..}) = ship
    let (leftEdge, bottomEdge) = over both round _shipPosition - over both (`div` 2) (sizeX', sizeY')
    draw (0, 50, 200) $ BLRect leftEdge bottomEdge sizeX' sizeY'

    -- Draw Thrusters
    let (tx, ty) = _shipThrust

    let thrustColor = if not boosted then (200,50,0) else (255, 100, 0)
    forM_ (thrusters sizeX' sizeY' tx ty boosted) (draw thrustColor . moveRelativeTo (leftEdge, bottomEdge))

    SDL.flip screen
    go keysDown' screen s' w'

thrusters sizeX' sizeY' tx ty boosted = map snd . filter fst $ [
         (tx > 0 , BLRect (-sizeO) ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (tx < 0 , BLRect sizeX' ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (ty > 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) (-sizeO) size sizeO)
       , (ty < 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) sizeY' size sizeO)
     ]
     where size = if boosted then 12 else 10
           sizeO = if boosted then 20 else 10

runShip shipSize world = proc keysDown -> do
  g <- gravityEdge -< keysDown
  thrust <- acceleration -< keysDown
  boost <- boostAccel -< keysDown
  (ObjectState pos _) <- spaceShipObject shipSize -< (Accelerate ((thrust * if boost then 2.5 else 1.0) + gravity g), world)
  returnA -< (Ship pos thrust, g, boost)

boostAccel :: Monad m => Wire e m (Set SDL.Keysym) Bool 
boostAccel = fmap (keyDown SDL.SDLK_LSHIFT) id

acceleration :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) (Double, Double)
acceleration  = 100 *^ (
                  (if' (keyDown SDL.SDLK_LEFT) (pure (-1, 0)) (pure (0,0)))
                + (if' (keyDown SDL.SDLK_RIGHT) (pure (1, 0)) (pure (0,0)))
                + (if' (keyDown SDL.SDLK_UP) (pure (0, 3)) (pure (0,0)))
                + (if' (keyDown SDL.SDLK_DOWN) (pure (0, -3)) (pure (0,0)))
                )

debug x = trace (show x) x

collisions :: (Ord a, Num a) => BLRect a -> BLRect a -> Maybe Edge
collisions (BLRect ix' iy iw ih) (BLRect ox oy ow oh) | ix' < ox = Just L
                                                     | ix' + iw > ox + ow = Just R
                                                     | iy < oy = Just B
                                                     | iy + ih > oy + oh = Just T
                                                     | otherwise = Nothing

collisions' me other = listToMaybe . filter (Prelude.all (\v -> v `within` other) . corners me) $ [L, R, T, B]

collisions'' me other = collisions' me other <|> fmap flip' (collisions' other me)

flip' L = R
flip' R = L
flip' T = B
flip' B = T

between z a b = z > a && z < b

spaceShipObject :: (Scalar a0 ~ Double, Monad m0, Monoid e0, Num a0, Ord a0, Fractional a0, VectorSpace a0) => (Int, Int) -> Wire e0 m0 (ObjectDiff (a0, a0), World) (ObjectState (a0, a0))
spaceShipObject (sizeX, sizeY) = object_ postUpdate (ObjectState (100, 200 - realToFrac sizeY/2) (0,10))
  where postUpdate world (ObjectState pos vel@(vx, vy)) = 
           let shipBox' = centerAndSizeToRect pos (realToFrac sizeX, realToFrac sizeY)
               collision = collisions shipBox' (fmap fromIntegral $ _worldBox world) <|> asum (map (collisions'' shipBox' . fmap fromIntegral . _thingRect) $ _worldScenery world )
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
  case ev of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete' k keysDown)
    SDL.Quit -> error "Thanks"
    _ -> parseEvents keysDown

-- Modifiers might change on key up. we don't care
delete' k = Set.fromList . deleteBy ((==) `on` SDL.symKey) k . Set.toList

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . Set.null . Set.filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym

uncurry3 f (a,b,c) = f a b c

-- TODO
-- Good collision detection using a priority queue
