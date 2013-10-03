{-# LANGUAGE DeriveFunctor #-}
module Graphics where

import Control.Applicative
import Control.Monad (join)
import Data.Maybe (listToMaybe)

data BLRect a = BLRect a a a a deriving (Eq, Show, Functor)

data Edge = L | R | T | B deriving (Eq, Show)

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

allCorners r = map (corners r) [L, R, T, B]

overlapping a b = any (`within` b) (join $ allCorners a) || any (`within` a) (join $ allCorners b)

findEdgeWithin me other = listToMaybe . filter (Prelude.all (`within` other) . corners me) $ [L, R, T, B]

flipEdge L = R
flipEdge R = L
flipEdge T = B
flipEdge B = T

overlappingEdge me other = findEdgeWithin me other <|> fmap flipEdge (findEdgeWithin other me)

boundsCheck :: (Ord a, Num a) => BLRect a -> BLRect a -> Maybe Edge
boundsCheck (BLRect ix' iy iw ih) (BLRect ox oy ow oh) | ix' < ox = Just L
                                                     | ix' + iw > ox + ow = Just R
                                                     | iy < oy = Just B
                                                     | iy + ih > oy + oh = Just T
                                                     | otherwise = Nothing

rectForCenter (x,y) (sizeX', sizeY') = BLRect (round x - sizeX' `div` 2) (round y - sizeY' `div` 2) sizeX' sizeY'

data Graphics m a = Graphics {
  initializer :: m (),
  drawFrame :: m () -> m (),
  openImage :: FilePath -> m a,
  paintScreen' :: T3 Double -> m (),
  paintRect' :: Int -> T3 Double -> BLRect Int -> m (),
  paintRectTex' :: Int -> a -> BLRect Int -> m ()
}

type T3 a = (a,a,a)
