{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module SDLTemplate where

import Graphics
import Control.Lens
import Data.VectorSpace hiding (Sum)
import Data.Monoid
import Data.Foldable
import Debug.Trace (trace)
import Prelude hiding ((.), id, null, filter)
import qualified Control.Monad as M
import Control.Wire hiding (empty)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL

data Ship = Ship { _shipPosition :: (Double, Double), _shipThrust :: (Double, Double) }
          deriving (Eq, Show)


-- TODO unit vector * magnitude
gravity L = (-1, 0) ^* 80
gravity R = (1, 0) ^* 80
gravity T = (0, 1) ^* 80
gravity B = (0, -1) ^* 80

data Game = Game {
            shipSize :: (Int, Int)
          , screenWidth :: Int
          , screenHeight :: Int
          }


type T3 a = (a,a,a)

black = (0,0,0) :: T3 Double
goldenRod = (238, 232, 170)
papayaWhip = (255, 239, 213)

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  let game = Game (32,64) 400 600
  screen <- SDL.setVideoMode (screenWidth game) (screenHeight game) 32 [SDL.SWSurface]
  let shipThing = runShip (shipSize game)
  let dayAndNight = foreverW $ (interpolateFromTo black papayaWhip 5 `andThen` interpolateFromTo papayaWhip black 5)
  go empty screen clockSession (dayAndNight &&& shipThing &&& sampleFPS 1) game
 where
  go keysDown screen s w game@(Game (sizeX, sizeY) width height) = do
    let draw (r,g,b) = paintRect screen height r g b

    keysDown' <- parseEvents keysDown
    (( (sr, sg, sb), ((ship, groundSide, wiggle), fps')), w', s') <- stepSession_ w s keysDown'

    forM_ fps' print

    let sizeX' = sizeX + wiggle * 2
    let sizeY' = sizeY + wiggle * 2

    -- The backdrop
    paintScreen screen (round sr) (round sg) (round sb)
    draw goldenRod $ BLRect 200 0 15 350
    let ground = case groundSide of
                   L -> BLRect 0 0 10 height
                   R -> BLRect (width - 10) 0 10 height
                   T -> BLRect 0 (height - 10) width 10
                   B -> BLRect 0 0 width 10

    draw (0, 200, 0) ground 

    -- Draw Ship
    let (Ship {..}) = ship
    let (x', y') = over both round _shipPosition
    let (tx, ty) = _shipThrust
    let leftEdge = x' - (sizeX' `div` 2)
    let bottomEdge = y' - (sizeY' `div` 2)
    draw (0, 50, 200) $ BLRect leftEdge bottomEdge sizeX' sizeY'

    -- Draw Thrusters
    M.when (tx > 0) $ draw (200, 50, 0) $ BLRect (leftEdge - 10) (bottomEdge + (sizeY' `div` 2) - 5) 10 10
    M.when (tx < 0) $ draw (200, 50, 0) $ BLRect (leftEdge + sizeX') (bottomEdge + (sizeY' `div` 2) - 5) 10 10
    M.when (ty > 0) $ draw (200, 50, 0) $ BLRect (leftEdge + (sizeX' `div` 2) - 5) (bottomEdge - 10) 10 10
    M.when (ty < 0) $ draw (200, 50, 0) $ BLRect (leftEdge + (sizeX' `div` 2) - 5) (bottomEdge + sizeY') 10 10

    SDL.flip screen
    go keysDown' screen s' w' game

runShip shipSize = proc keysDown -> do
  n <- noiseRM -< (-2 :: Int, 2 :: Int)
  g <- gravity' -< keysDown
  thrust <- acceleration -< keysDown
  let accel = thrust + gravity g
  rec (position, collisions) <- position shipSize -< velocity
      velocity <- velocity -< (accel, (collisions, g))
  returnA -< (Ship position thrust, g, if thrust /= (0,0) then n else 0)

acceleration :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) (Double, Double)
acceleration  = (ala Sum foldMap) ([ 
               pure (-90, 0) . when (keyDown SDL.SDLK_LEFT) <|> pure (0,0)
             , pure (90, 0) . when (keyDown SDL.SDLK_RIGHT) <|> pure (0,0)
             , pure (0, 90) . when (keyDown SDL.SDLK_UP) <|> pure (0,0)
             , pure (0, -90) . when (keyDown SDL.SDLK_DOWN) <|> pure (0,0)
             ])

debug x = trace (show x) x

data Box = L | R | T | B deriving (Eq, Show)

type Collided = Maybe Box

velocity :: Wire e m ((Double, Double), (Collided, Box)) (Double, Double)
velocity = integralLim_ bounce (0, 0)
  where bounce (collisions, grav) _ v = dim grav collisions * velocity' v collisions
        dim grav = maybe 1.0 (\v -> if v == grav then 0.9 else 1.0)
        velocity' v@(vx,vy) = maybe v $ \x -> case x of
                                           L -> (abs vx, vy)
                                           R -> (-(abs vx), vy)
                                           T -> (vx, -(abs vy))
                                           B -> (vx, (abs vy))

collisions :: (Ord a, Num a) => BLRect a -> BLRect a -> Maybe Box
collisions (BLRect ix iy iw ih) (BLRect ox oy ow oh) | ix < ox = Just L
                                                     | ix + iw > ox + ow = Just R
                                                     | iy < oy = Just B
                                                     | iy + ih > oy + oh = Just T
                                                     | otherwise = Nothing

centerAndSizeToRect :: (Fractional a, Num a) => (a, a) -> (a, a) -> BLRect a
centerAndSizeToRect (x,y) (w,h) = BLRect (x - w / 2) (y - h / 2) w h 

position :: (Int, Int) -> Wire e m (Double, Double) ((Double, Double), Collided)
position (sizeX, sizeY) = accumT clamp ((100, 200 - (realToFrac sizeY)/2), Nothing)
  where clamp dt (i@(x, y), _) v =
          let z = i + v ^* dt
              shipBox' = centerAndSizeToRect (x, y) (realToFrac sizeX, realToFrac sizeY)
              collision = collisions shipBox' $ BLRect 0 0 400 600
           in (z, collision)

gravity' :: Wire e m (Set SDL.Keysym) Box 
gravity' = accum (\g keys -> if keyDown SDL.SDLK_q keys then rotate g else g) B
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
    SDL.KeyDown k -> parseEvents (insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym

interpolateFromTo :: (Monad m, VectorSpace c, Monoid e, Scalar c ~ Double) => c -> c -> Scalar c -> Wire e m b c
interpolateFromTo start finish duration = let step dt last = last ^+^ (finish ^-^ start) ^* (dt/duration)
                                           in iterateWT step start . for duration 
                                 
foreverW :: Monad m => Wire e m a b -> Wire e m a b
foreverW a = a `andThen` foreverW a

sampleFPS :: (Monad m, Monoid e, Integral b) => Time -> Wire e m a (Maybe b)
sampleFPS secs = event (periodically secs . fmap (round . (1/)) dtime)

