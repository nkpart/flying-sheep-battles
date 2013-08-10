{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module SDLTemplate where

import qualified Config as C
import Data.Function (on)
import Graphics
import Wires
import Control.Lens
import Data.VectorSpace hiding (Sum)
import Data.Monoid
import Data.Foldable
import Debug.Trace (trace)
import Prelude hiding ((.), id, null)
import qualified Control.Monad as M
import Control.Wire hiding (empty)
import Data.Set as Set (Set, empty, insert, null, filter, toList, fromList)
import Data.List as List (deleteBy)
import qualified Graphics.UI.SDL as SDL

data Ship = Ship { _shipPosition :: (Double, Double), _shipThrust :: (Double, Double) } deriving (Eq, Show)

isThrusting = (/= (0,0)) . _shipThrust

gravityMagnitude = 240
gravity L = (-1, 0) ^* gravityMagnitude
gravity R = (1, 0) ^* gravityMagnitude
gravity T = (0, 1) ^* gravityMagnitude
gravity B = (0, -1) ^* gravityMagnitude

data Game = Game { shipSize :: (Int, Int) , screenWidth :: Int , screenHeight :: Int }

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
  let shipThing = runShip (sizeX, sizeY)
  let dayAndNight = foreverW $ (interpolateFromTo C.nightColor C.dayColor 5 `andThen` interpolateFromTo C.dayColor C.nightColor 5)
  go empty screen clockSession (dayAndNight &&& shipThing &&& sampleFPS 1)
 where
  sizeX = C.shipW
  sizeY = C.shipH
  height = C.height
  width = C.width
  go keysDown screen s w = do
    let draw = paintRect screen height 

    keysDown' <- parseEvents keysDown
    ((skyColor, ((ship, groundSide), fps)), w', s') <- stepSession_ w s keysDown'

    forM_ fps print

    wiggle <- if (_shipThrust ship /= (0,0)) 
                then randomRIO (-2, 2)
                else return 0
    let (sizeX', sizeY') = over both (+ wiggle * 2) (sizeX, sizeY)

    -- The backdrop
    paintScreen screen skyColor
    draw C.goldenRod $ BLRect 200 0 15 350
    let ground = case groundSide of
                   L -> BLRect 0 0 10 height
                   R -> BLRect (width - 10) 0 10 height
                   T -> BLRect 0 (height - 10) width 10
                   B -> BLRect 0 0 width 10

    draw (0, 200, 0) ground 

    -- Draw Ship
    let (Ship {..}) = ship
    let (leftEdge, bottomEdge) = (over both round _shipPosition) - (over both (`div` 2) (sizeX', sizeY'))
    let (tx, ty) = _shipThrust
    draw (0, 50, 200) $ BLRect leftEdge bottomEdge sizeX' sizeY'

    -- Draw Thrusters
    forM_ (thrusters leftEdge bottomEdge sizeX' sizeY' tx ty) (draw (200, 50, 0))

    SDL.flip screen
    go keysDown' screen s' w'

thrusters leftEdge bottomEdge sizeX' sizeY' tx ty = map snd . Prelude.filter fst $ [
         ((tx > 0) , BLRect (leftEdge - 10) (bottomEdge + (sizeY' `div` 2) - 5) 10 10)
       , ((tx < 0) , BLRect (leftEdge + sizeX') (bottomEdge + (sizeY' `div` 2) - 5) 10 10)
       , ((ty > 0) , BLRect (leftEdge + (sizeX' `div` 2) - 5) (bottomEdge - 10) 10 10)
       , ((ty < 0) , BLRect (leftEdge + (sizeX' `div` 2) - 5) (bottomEdge + sizeY') 10 10)
     ]

runShip shipSize = proc keysDown -> do
  g <- gravity' -< keysDown
  thrust <- acceleration -< keysDown
  (ObjectState pos _) <- spaceShipObject shipSize -< (Accelerate (thrust + gravity g), g)
  returnA -< (Ship pos thrust, g)

acceleration :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) (Double, Double)
acceleration  = fmap debug $ 100 *^ enhanceAccel *^
                ((pure (-1, 0) . when (keyDown SDL.SDLK_LEFT) <|> pure (0,0)) +
                (pure (1, 0) . when (keyDown SDL.SDLK_RIGHT) <|> pure (0,0)) +
                (pure (0, 3) . when (keyDown SDL.SDLK_UP) <|> pure (0,0))   +
                (pure (0, -3) . when (keyDown SDL.SDLK_DOWN) <|> pure (0,0)))

enhanceAccel :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) Double
enhanceAccel = pure 1.5 . when (keyDown SDL.SDLK_LSHIFT) <|> pure 1.0

debug x = trace (show x) x

data Edge = L | R | T | B deriving (Eq, Show)

collisions :: (Ord a, Num a) => BLRect a -> BLRect a -> Maybe Edge
collisions (BLRect ix iy iw ih) (BLRect ox oy ow oh) | ix < ox = Just L
                                                     | ix + iw > ox + ow = Just R
                                                     | iy < oy = Just B
                                                     | iy + ih > oy + oh = Just T
                                                     | otherwise = Nothing

spaceShipObject (sizeX, sizeY) = object_ postUpdate (ObjectState (100, 200 - (realToFrac sizeY)/2) (0,0))
  where postUpdate g (ObjectState pos vel@(vx, vy)) = 
           let shipBox' = centerAndSizeToRect pos (realToFrac sizeX, realToFrac sizeY)
               collision = collisions shipBox' $ BLRect 0 0 400 600
               g' x | x == g    = (^* 0.8)
                    | otherwise = id
            in ObjectState pos $ maybe vel (\e -> g' e $ case e of
                                                   L -> (abs vx, vy)
                                                   R -> (-(abs vx), vy)
                                                   T -> (vx, -(abs vy))
                                                   B -> (vx, (abs vy))
            ) collision

gravity' :: Wire e m (Set SDL.Keysym) Edge 
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
    SDL.KeyUp k -> parseEvents (delete' k keysDown)
    _ -> parseEvents keysDown

-- Modifiers might change on key up. we don't care
delete' k = Set.fromList . deleteBy ((==) `on` SDL.symKey) k . Set.toList

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . Set.filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym

-- TODO
-- Good collision detection using a priority queue
--

uncurry3 f (a,b,c) = f a b c
