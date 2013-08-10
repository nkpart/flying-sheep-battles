module Graphics where

import Control.Monad as M
import Graphics.UI.SDL as SDL

data BLRect a = BLRect a a a a deriving (Eq, Show)

zzz screen r g b z = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b >>= z

paintScreen screen r g b = zzz screen r g b $ SDL.fillRect screen Nothing

paintRect screen height r g b rect = M.void $ zzz screen r g b $ SDL.fillRect screen (Just $ toSDLRect height rect)

toSDLRect height (BLRect x y w h) = SDL.Rect x (height - y - h) w h
