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
import Control.Wire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  go empty screen clockSession challenge3

 where

  go keysDown screen s w = do
    keysDown' <- parseEvents keysDown
    (x, w', s') <- stepSession_ w s keysDown'

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)
        
    SDL.flip screen
    go keysDown' screen s' w'

challenge3 :: (MonadFix m, Monoid e) => Wire e m (Set SDL.Keysym) Double
challenge3 = proc keysDown -> do
  accel <- acceleration -< keysDown
  rec (position, collisions) <- position -< velocity
      velocity <- velocity -< (debug accel, collisions)
  returnA -< position

acceleration :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) Double
acceleration  =  pure (-20) . when (keyDown SDL.SDLK_LEFT)
             <|> pure 20 . when (keyDown SDL.SDLK_RIGHT)
             <|> pure 0

debug x = trace (show x) x

velocity :: Wire e m (Double, Bool) Double
velocity = integralLim_ bounce 0
  where bounce collisions _ v | collisions = debug (-v)
                              | otherwise  = debug v

position :: Wire e m Double (Double, Bool)
position = accumT clamp (0, False)
  where clamp dt (x, _) v =
          let x' = x + dt * v
              coll = x < 0 || x > 150
              bounded = if coll then max 2 (min 148 x') else x'
          in trace (show (bounded, coll)) (bounded, coll)

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym
