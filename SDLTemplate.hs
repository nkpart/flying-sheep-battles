{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
module SDLTemplate where

import Debug.Trace (trace)
-- import Resources
-- import qualified Graphics.UI.SDL as SDL
-- import qualified Graphics.UI.SDL.Image as SDLI
--     hello <- SDLI.load (resourcePath "image.png")

import Prelude hiding ((.), id, null, filter)
import Control.Monad.Fix (MonadFix)
import qualified Control.Monad as M (when, void)
import Control.Wire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL

data Ship = Ship { _shipPosition :: (Double, Double), _shipThrust :: (Double, Double) }
          deriving (Eq, Show)

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  go empty screen clockSession runShip

 where

  go keysDown screen s w = do
    keysDown' <- parseEvents keysDown
    (Ship (x,y) (tx, ty), w', s') <- stepSession_ w s keysDown'

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) 50 50)

    M.when (tx > 0) $ M.void $ do
      (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 50 0 >>=
          SDL.fillRect screen (Just $ SDL.Rect (round x - 10) (round y + 25 - 5) 10 10)
    M.when (tx < 0) $ M.void $ do
      (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 50 0 >>=
          SDL.fillRect screen (Just $ SDL.Rect (round x + 50) (round y + 25 - 5) 10 10)
    M.when (ty > 0) $ M.void $ do
      (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 50 0 >>=
          SDL.fillRect screen (Just $ SDL.Rect (round x + 25 - 5) (round y - 10) 10 10)
    M.when (ty < 0) $ M.void $ do
      (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 50 0 >>=
          SDL.fillRect screen (Just $ SDL.Rect (round x + 25 - 5) (round y + 50) 10 10)
        
    SDL.flip screen
    go keysDown' screen s' w'

runShip :: (MonadFix m, Monoid e) => Wire e m (Set SDL.Keysym) Ship
runShip = proc keysDown -> do
  accel <- acceleration -< keysDown
  rec (position, collisions) <- position -< velocity
      velocity <- velocity -< (debug accel, collisions)
  returnA -< Ship position accel

acceleration :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) (Double, Double)
acceleration  =  pure (-20, 0) . when (keyDown SDL.SDLK_LEFT)
             <|> pure (20, 0) . when (keyDown SDL.SDLK_RIGHT)
             <|> pure (0, -20) . when (keyDown SDL.SDLK_UP)
             <|> pure (0, 20) . when (keyDown SDL.SDLK_DOWN)
             <|> pure (0, 0)

debug x = trace (show x) x

-- data Collided = Didnt | Horizontal | Vertical deriving (Eq, Show)

data Box = L | R | T | B deriving Eq

type Collided = Maybe Box

velocity :: Wire e m ((Double, Double), Collided) (Double, Double)
velocity = integralLim_ bounce (0, 0)
  where bounce collisions _ v@(vx, vy) = case collisions of
                                           Nothing -> v
                                           Just L -> (abs vx, vy)
                                           Just R -> (-(abs vx), vy)
                                           Just T -> (vx, abs vy)
                                           Just B -> (vx, -(abs vy))

position :: Wire e m (Double, Double) ((Double, Double), Collided)
position = accumT clamp ((0, 0), Nothing)
  where clamp dt ((x, y), _) (vx, vy) =
          let x' = x + dt * vx
              y' = y + dt * vy
              collision = case x of
                            _ | x < 0 -> Just L
                            _ | x > 150 -> Just R
                            _ | y < 0 -> Just T
                            _ | y > 150 -> Just B
                            _ -> Nothing
              collX = maybe False (\l -> l == L || l == R) collision
              collY = maybe False (\l -> l == T || l == B) collision
              boundedX = if collX then max 2 (min 148 x') else x'
              boundedY = if collY then max 2 (min 148 y') else y'
          in ((boundedX, boundedY), collision)

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
