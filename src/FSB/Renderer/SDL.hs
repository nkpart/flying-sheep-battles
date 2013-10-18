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
                    _sheep1 :: SDL.Surface,
                    _sheep2 :: SDL.Surface
                  }

sdlGraphics :: IO (Graphics IO SDL.Surface)
sdlGraphics = do
    s <- SDL.setVideoMode C.width C.height 32 [SDL.SWSurface]
    return $ Graphics (return ()) drawFrame SDLI.load (paintScreen s) undefined undefined

cloudRects w d  = [
                 BLRect d 0 (w - 2 * d) (d `div` 2),
                 BLRect 0 (d `div` 2) w d,
                 BLRect d (d + d `div` 2) (w - 2 * d) (d `div` 2)
                 ]


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

