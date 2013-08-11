{-# LANGUAGE DeriveFunctor #-}
module Graphics where

import Control.Monad as M
import Graphics.UI.SDL as SDL

data BLRect a = BLRect a a a a deriving (Eq, Show, Functor)

data Edge = L | R | T | B deriving (Eq, Show)

zzz screen r g b z = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b >>= z

paintScreen screen (r, g, b) = zzz screen (round r) (round g) (round b) $ SDL.fillRect screen Nothing

paintRect screen height (r, g, b) rect = M.void $ zzz screen (round r) (round g) (round b) $ SDL.fillRect screen (Just $ toSDLRect height rect)

toSDLRect :: Int -> BLRect Int -> SDL.Rect
toSDLRect height (BLRect x y w h) = SDL.Rect x (height - y - h) w h

centerAndSizeToRect :: (Fractional a, Num a) => (a, a) -> (a, a) -> BLRect a
centerAndSizeToRect (x,y) (w,h) = BLRect (x - w / 2) (y - h / 2) w h 

moveRelativeTo (x', y') (BLRect x y w h) = BLRect (x' + x) (y' + y) w h

within (x',y') (BLRect x y w h) = x' > x && x' < x + w && y' > y && y' < y + h

bottomLeft (BLRect x y _ _) = (x,y)
topLeft (BLRect x y _ h) = (x, y + h)
topRight (BLRect x y w h) = (x+w, y+h)
bottomRight (BLRect x y w _) = (x+w, y)

corners r L = [bottomLeft r, topLeft r]
corners r R = [bottomRight r, topRight r]
corners r T = [topLeft r, topRight r]
corners r B = [bottomLeft r, bottomRight r]

