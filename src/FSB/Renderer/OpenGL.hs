{-# LANGUAGE RecordWildCards #-}
module FSB.Renderer.OpenGL where

import Data.Vector.Storable (unsafeWith)
import Codec.Picture
import Foreign.C.Types
import Prelude hiding ((.), id)
import Control.Lens (over, both)
import FSB.Types
import qualified Config as C
import Graphics
import Control.Monad as M (forM_, when, void)
import Data.VectorSpace
import Control.Wire
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))

import FSB.Renderer

initRenderer = do
    SDL.glSetAttribute SDL.glDoubleBuffer 1
    _ <- SDL.setVideoMode C.width C.height 32 [SDL.OpenGL]
    GL.clearColor $= GL.Color4 0 0 0 0
    -- GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral C.width) (fromIntegral C.height))
    GL.clear [GL.ColorBuffer]
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 (fromIntegral C.width) (fromIntegral C.height) 0 (-1) 1
    GL.matrixMode $= GL.Modelview 0
    sheep1 <- imageToTex "sheep1.png"
    sheep2 <- imageToTex "sheep2.png"
    return $ Renderer (drawGame sheep1 sheep2)

cloudRects w d  = [
                 BLRect d 0 (w - 2 * d) (d `div` 2),
                 BLRect 0 (d `div` 2) w d,
                 BLRect d (d + d `div` 2) (w - 2 * d) (d `div` 2)
                 ]

drawWorld world (Sky sky stars) = do
  let draw = paintRect C.height
  paintScreen sky
  forM_ stars $ \(x,y) -> draw (lerp sky (255,255,255) ((fromIntegral y / fromIntegral C.height) ^ 2)) (BLRect x y 2 2)
  forM_ (map (moveRelativeTo (10, 500)) $ cloudRects 100 20) $ draw (255, 255, 255)
  forM_ (_worldScenery world) $ \(Thing r c) -> draw c $ fmap round r
  -- ground
  draw C.groundColor $ BLRect 0 0 C.width 10

drawShip image ship isDead = do
  let draw = paintRect C.height
  wiggle <- if _shipThrust ship /= (0,0)
              then randomRIO (-2, 2)
              else return 0
  let wiggledSize = over both (+ wiggle * 2) (C.shipW, C.shipH)

  let (Ship {..}) = ship
  let rect@(BLRect leftEdge bottomEdge _ _)  = rectForCenter (_shipPosition ship) wiggledSize
  paintRectTex C.height image rect
  M.when isDead $ draw (255, 0, 0) $ BLRect leftEdge bottomEdge C.shipW 10

  -- Draw Thrusters
  let thrustColor = if not _shipBoosting then (200,50,0) else (255, 100, 0)
  forM_ (thrusters wiggledSize _shipThrust _shipBoosting) (draw thrustColor . moveRelativeTo (leftEdge, bottomEdge))

drawGame image1 image2 world (GameState (ship1, ship2, victory)) sky = do
  GL.clear [GL.ColorBuffer]
  drawWorld world sky
  drawShip image1 ship1 $ maybe False not victory
  drawShip image2 ship2 $ maybe False id  victory
  SDL.glSwapBuffers

thrusters (sizeX', sizeY') (tx, ty) boosted = map snd . filter fst $ [
         (tx > 0 , BLRect (-sizeO) ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (tx < 0 , BLRect sizeX' ((sizeY' `div` 2) - (size `div` 2)) sizeO size)
       , (ty > 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) (-sizeO) size sizeO)
       , (ty < 0 , BLRect ((sizeX' `div` 2) - (size `div` 2)) sizeY' size sizeO)
     ]
     where size = if boosted then 12 else 10
           sizeO = if boosted then 20 else 10

paintScreen :: T3 Double -> IO ()
paintScreen (r, g, b) = do
    GL.renderPrimitive GL.Quads $ do
      GL.color $ GL.Color3 (CDouble r / 255) (CDouble g / 255) (CDouble b / 255)
      GL.vertex $ GL.Vertex2 (CDouble 0) (CDouble 0) -- (CDouble 0)
      GL.vertex $ GL.Vertex2 (CDouble 0) (fromIntegral C.height) -- (CDouble 0)
      GL.vertex $ GL.Vertex2 (CDouble $ fromIntegral C.width) (fromIntegral C.height) -- (CDouble 0)
      GL.vertex $ GL.Vertex2 (fromIntegral C.width) (CDouble 0) -- (CDouble 0)

paintRect height (r, g, b) rect = do
    let (SDL.Rect x' y' w' h') = toSDLRect height rect
        x = fromIntegral x'
        y = fromIntegral y'
        x2 = fromIntegral $ x' + w'
        y2 = fromIntegral $ y' + h'
    GL.renderPrimitive GL.Quads $ do
      GL.color $ GL.Color3 (CDouble r / 255) (CDouble g / 255) (CDouble b / 255)
      GL.vertex $ GL.Vertex2 (CDouble x) (CDouble y)
      GL.vertex $ GL.Vertex2 (CDouble x2) (CDouble y)
      GL.vertex $ GL.Vertex2 (CDouble x2) (CDouble y2)
      GL.vertex $ GL.Vertex2 (CDouble x) (CDouble y2)

paintRectTex height tex rect = do
    let v@(SDL.Rect x' y' w' h') = toSDLRect height rect
        x = fromIntegral x'
        y = fromIntegral y'
        x2 = fromIntegral $ x' + w'
        y2 = fromIntegral $ y' + h'
    let texV t1 t2 a b = do
          GL.texCoord $ GL.TexCoord2 (CDouble t1) (CDouble t2)
          GL.vertex $ GL.Vertex3 (CDouble a) (CDouble b) 0
    GL.color (GL.Color4 1 1 1 1 :: GL.Color4 GL.GLfloat)
    GL.blend $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.renderPrimitive GL.Quads $ do
      texV 0 0 x y
      texV 1 0 x2 y
      texV 1 1 x2 y2
      texV 0 1 x y2
    GL.texture GL.Texture2D $= GL.Disabled

toSDLRect :: Int -> BLRect Int -> SDL.Rect
toSDLRect height (BLRect x y w h) = SDL.Rect x (height - y - h) w h

imageToTex image = do
  Right (ImageRGBA8 (Image width height dat)) <- readImage image
  [texName] <- GL.genObjectNames 1  -- generate our texture.
  GL.textureBinding GL.Texture2D $= Just texName  -- make our new texture the current texture.
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  unsafeWith dat $ \ptr ->
    GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
  return texName
