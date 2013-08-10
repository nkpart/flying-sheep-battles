{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
module SDLTemplate where

import Data.VectorSpace hiding (Sum)
import Data.Monoid
import Control.Newtype
import Data.Foldable
import Debug.Trace (trace)
import Prelude hiding ((.), id, null, filter)
import Control.Monad.Fix (MonadFix)
import qualified Control.Monad as M (when, void)
import Control.Wire hiding (empty)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL

data Ship = Ship { _shipPosition :: (Double, Double), _shipThrust :: (Double, Double) }
          deriving (Eq, Show)

rrr = SDL.mapRGB . SDL.surfaceGetPixelFormat
zzz screen r g b z = rrr screen r g b >>= z

paintScreen screen r g b = zzz screen r g b $ SDL.fillRect screen Nothing
paintRect screen r g b rect = zzz screen r g b $ SDL.fillRect screen (Just rect)

-- TODO unit vector * magnitude
gravity L = (-1, 0) ^* 80
gravity R = (1, 0) ^* 80
gravity T = (0, -1) ^* 80
gravity B = (0, 1) ^* 80

data Game = Game {
          shipSize :: (Int, Int)
          , screenWidth :: Int
          , screenHeight :: Int
          }

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  let game = Game (32,64) 400 600
  screen <- SDL.setVideoMode (screenWidth game) (screenHeight game) 32 [SDL.SWSurface]
  go empty screen clockSession (runShip $ shipSize game) game
 where

  go keysDown screen s w game@(Game (sizeX, sizeY) width height) = do
    keysDown' <- parseEvents keysDown
    ((Ship (x, y) (tx, ty), g, wiggle), w', s') <- stepSession_ w s keysDown'
    let (x', y') = (round x, round y)

    let sizeX' = sizeX + wiggle * 2
    let sizeY' = sizeY + wiggle * 2

-- papaya whip
    paintScreen screen 255 239 213
-- golden rod
    paintRect screen 238 232 170 $ SDL.Rect 85 50 30 150

    let x'' = x' - (sizeX' `div` 2)
    let y'' = y' - (sizeY' `div` 2)
    paintRect screen 0 50 200 $ SDL.Rect (x' - (sizeX' `div` 2)) (y' - (sizeY' `div` 2)) sizeX' sizeY'

    let ground = case g of
                   L -> SDL.Rect 0 0 10 200
                   R -> SDL.Rect 190 0 10 200
                   T -> SDL.Rect 0 0 200 10
                   B -> SDL.Rect 0 190 200 10

    paintRect screen 0 200 0 ground
-- 255, 228, 225
    M.when (tx > 0) $ M.void $ paintRect screen 200 50 0 $ SDL.Rect (x'' - 10) (y'' + (sizeY' `div` 2) - 5) 10 10
    M.when (tx < 0) $ M.void $ paintRect screen 200 50 0 $ SDL.Rect (x'' + sizeX') (y'' + (sizeY' `div` 2) - 5) 10 10
    M.when (ty > 0) $ M.void $ paintRect screen 200 50 0 $ SDL.Rect (x'' + (sizeX' `div` 2) - 5) (y'' - 10) 10 10
    M.when (ty < 0) $ M.void $ paintRect screen 200 50 0 $ SDL.Rect (x'' + (sizeX' `div` 2) - 5) (y'' + sizeY') 10 10

    paintRect screen 0 0 0 $ SDL.Rect x' y' 2 2
    SDL.flip screen
    go keysDown' screen s' w' game

-- runShip :: (MonadFix m, Monoid e) => Wire e m (Set SDL.Keysym) Ship
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
             , pure (0, -90) . when (keyDown SDL.SDLK_UP) <|> pure (0,0)
             , pure (0, 0) . when (keyDown SDL.SDLK_DOWN) <|> pure (0,0)
             ])

debug x = trace (show x) x

data Box = L | R | T | B deriving Eq

type Collided = Maybe Box

velocity :: Wire e m ((Double, Double), (Collided, Box)) (Double, Double)
velocity = integralLim_ bounce (0, 0)
  where bounce (collisions, grav) _ v = dim grav collisions * velocity' v collisions
        dim grav = maybe 1.0 (\v -> if v == grav then 0.9 else 1.0)
        velocity' v@(vx,vy) = maybe v $ \x -> case x of
                                           L -> (abs vx, vy)
                                           R -> (-(abs vx), vy)
                                           T -> (vx, abs vy)
                                           B -> (vx, -(abs vy))

data Rect a = Rect a a a a

collisions :: (Ord a, Num a) => Rect a -> Rect a -> Maybe Box
collisions (Rect ix iy iw ih) (Rect ox oy ow oh) | ix < ox = Just L
                                                         | ix + iw > ox + ow = Just R
                                                         | iy < oy = Just T
                                                         | iy + ih > oy + oh = Just B
                                                         | otherwise = Nothing

shipBox :: (Fractional a, Num a) => (a, a) -> (a, a) -> Rect a
shipBox (x,y) (w,h) = Rect (x - w / 2) (y - h / 2) w h 

position :: (Int, Int) -> Wire e m (Double, Double) ((Double, Double), Collided)
position (sizeX, sizeY) = accumT clamp ((100, 200 - (realToFrac sizeY)/2), Nothing)
  where clamp dt (i@(x, y), _) v =
          let z = i + v ^* dt
              shipBox' = shipBox (x, y) (realToFrac sizeX, realToFrac sizeY)
              collision = collisions shipBox' $ Rect 0 0 200 200
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
