module Graphics where

import Control.Monad as M
import Graphics.UI.SDL as SDL

data BLRect a = BLRect a a a a deriving (Eq, Show)


zzz screen r g b z = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b >>= z

paintScreen screen (r, g, b) = zzz screen (round r) (round g) (round b) $ SDL.fillRect screen Nothing

paintRect screen height (r, g, b) rect = M.void $ zzz screen (round r) (round g) (round b) $ SDL.fillRect screen (Just $ toSDLRect height rect)

toSDLRect :: Int -> BLRect Int -> SDL.Rect
toSDLRect height (BLRect x y w h) = SDL.Rect x (height - y - h) w h

centerAndSizeToRect :: (Fractional a, Num a) => (a, a) -> (a, a) -> BLRect a
centerAndSizeToRect (x,y) (w,h) = BLRect (x - w / 2) (y - h / 2) w h 

