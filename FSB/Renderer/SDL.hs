{-# LANGUAGE RecordWildCards #-}
module FSB.Renderer.SDL where

import Prelude hiding ((.), id)
import Control.Lens (over, both)
import FSB.Types
import Config as C
import Graphics
import Control.Monad as M (forM_, when, void)
import Data.VectorSpace
import Control.Wire
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI

import FSB.Renderer

data SDLRenderer = SDLRenderer {
                    _screen :: SDL.Surface,
                    _sheep :: SDL.Surface
                  }

initRenderer = do
    r <- SDLRenderer <$> (SDL.setVideoMode C.width C.height 32 [SDL.SWSurface]) <*> SDLI.load "sheep.png"
    return $ Renderer (drawGame r)

cloudRects w d  = [
                 BLRect d 0 (w - 2 * d) (d `div` 2),
                 BLRect 0 (d `div` 2) w d,
                 BLRect d (d + d `div` 2) (w - 2 * d) (d `div` 2)
                 ]

drawWorld renderer world (Sky sky stars) = do
  let screen = _screen renderer
  let draw = paintRect screen C.height
  paintScreen screen sky
  forM_ stars $ \(x,y) -> draw (lerp sky (255,255,255) ((fromIntegral y / fromIntegral C.height) ^ 2)) $ BLRect x y 2 2
  forM_ (map (moveRelativeTo (10, 500)) $ cloudRects 100 20) $ draw (255, 255, 255)
  forM_ (_worldScenery world) $ \(Thing r c) -> draw c $ fmap round r
  -- ground
  draw C.groundColor $ BLRect 0 0 C.width 10

drawShip screen image ship isDead = do
  let draw = paintRect screen C.height
  wiggle <- if _shipThrust ship /= (0,0)
              then randomRIO (-2, 2)
              else return 0
  let wiggledSize = over both (+ wiggle * 2) (C.shipW, C.shipH)

  let (Ship {..}) = ship
  let rect@(BLRect leftEdge bottomEdge _ _)  = rectForCenter (_shipPosition ship) wiggledSize
  SDL.blitSurface image Nothing screen $ Just $ toSDLRect C.height rect
  M.when isDead $ draw (255, 0, 0) $ BLRect leftEdge bottomEdge C.shipW 10

  -- Draw Thrusters
  let thrustColor = if not _shipBoosting then (200,50,0) else (255, 100, 0)
  forM_ (thrusters wiggledSize _shipThrust _shipBoosting) (draw thrustColor . moveRelativeTo (leftEdge, bottomEdge))

drawGame renderer world (GameState (ship1, ship2, victory)) sky = do
  let screen = _screen renderer
  let image = _sheep renderer
  drawWorld renderer world sky
  drawShip screen image ship1 $ maybe False not victory
  drawShip screen image ship2 $ maybe False id  victory
  SDL.flip screen

thrusters (sizeX', sizeY') (tx, ty) boosted = map snd . filter fst $ [
         (tx > 0 , BLRect (-sizeO) ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (tx < 0 , BLRect sizeX' ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (ty > 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) (-sizeO) size sizeO)
       , (ty < 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) sizeY' size sizeO)
     ]
     where size = if boosted then 12 else 10
           sizeO = if boosted then 20 else 10

paintWithRGB screen r g b z = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b >>= z

paintScreen screen (r, g, b) = paintWithRGB screen (round r) (round g) (round b) $ SDL.fillRect screen Nothing

paintRect screen height (r, g, b) rect = M.void $ paintWithRGB screen (round r) (round g) (round b) $ SDL.fillRect screen (Just $ toSDLRect height rect)

toSDLRect :: Int -> BLRect Int -> SDL.Rect
toSDLRect height (BLRect x y w h) = SDL.Rect x (height - y - h) w h

