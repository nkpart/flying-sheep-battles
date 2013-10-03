{-# LANGUAGE RecordWildCards #-}
module FSB.Renderer.Drawer where

import Prelude hiding ((.), id)
import Control.Lens (over, both)
import FSB.Types
import qualified Config as C
import Graphics
import Control.Monad as M (forM_, when, void)
import Data.VectorSpace
import Control.Wire

import FSB.Renderer

initR wat = do
    initializer wat
    sheep1 <- openImage wat "sheep1.png"
    sheep2 <- openImage wat "sheep2.png"
    return $ Renderer (drawGame wat sheep1 sheep2)

drawGame wat image1 image2 world (GameState (ship1, ship2, victory)) sky = do
  drawFrame wat $ do
    drawWorld wat world sky
    drawShip wat image1 ship1 $ maybe False not victory
    drawShip wat image2 ship2 $ maybe False id  victory


drawWorld wat world (Sky sky stars) = do
  let draw = paintRect' wat C.height
  paintScreen' wat sky
  forM_ stars $ \(x,y) -> draw (lerp sky (255,255,255) ((fromIntegral y / fromIntegral C.height) ^ 2)) (BLRect x y 2 2)
  forM_ (map (moveRelativeTo (10, 500)) $ cloudRects 100 20) $ draw (255, 255, 255)
  forM_ (_worldScenery world) $ \(Thing r c) -> draw c $ fmap round r
  draw C.groundColor $ BLRect 0 0 C.width 10

cloudRects w d = [
                 BLRect d 0 (w - 2 * d) (d `div` 2),
                 BLRect 0 (d `div` 2) w d,
                 BLRect d (d + d `div` 2) (w - 2 * d) (d `div` 2)
                 ]

drawShip wat image ship isDead = do
  let draw = paintRect' wat C.height
  wiggle <- if _shipThrust ship /= (0,0)
              then randomRIO (-2, 2)
              else return 0
  let wiggledSize = over both (+ wiggle * 2) (C.shipW, C.shipH)

  let (Ship {..}) = ship
  let rect@(BLRect leftEdge bottomEdge _ _)  = rectForCenter (_shipPosition ship) wiggledSize
  paintRectTex' wat C.height image rect
  M.when isDead $ draw (255, 0, 0) $ BLRect leftEdge bottomEdge C.shipW 10

  -- Draw Thrusters
  let thrustColor = if not _shipBoosting then (200,50,0) else (255, 100, 0)
  forM_ (thrusters wiggledSize _shipThrust _shipBoosting) (draw thrustColor . moveRelativeTo (leftEdge, bottomEdge))

thrusters (sizeX', sizeY') (tx, ty) boosted = map snd . filter fst $ [
         (tx > 0 , BLRect (-sizeO) ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (tx < 0 , BLRect sizeX' ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (ty > 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) (-sizeO) size sizeO)
       , (ty < 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) sizeY' size sizeO)
     ]
     where size = if boosted then 12 else 10
           sizeO = if boosted then 20 else 10

