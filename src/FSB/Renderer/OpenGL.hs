{-# LANGUAGE RecordWildCards #-}
module FSB.Renderer.OpenGL where

import Data.Vector.Storable (unsafeWith)
import Codec.Picture
import Foreign.C.Types
import Prelude hiding ((.), id)
import FSB.Types
import qualified Config as C
import Graphics
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))

import FSB.Renderer.Drawer

xxx = let x = 3
          y = 2
          z = 1
       in y + z

openglGraphics = Graphics init' drawFrame imageToTex paintScreen  paintRect paintRectTex
  where init' = do
          SDL.glSetAttribute SDL.glDoubleBuffer 1
          _ <- SDL.setVideoMode C.width C.height 32 [SDL.OpenGL]
          GL.clearColor $= GL.Color4 0 0 0 0
          GL.clear [GL.ColorBuffer]
          GL.matrixMode $= GL.Projection
          GL.loadIdentity
          GL.ortho 0 (fromIntegral C.width) (fromIntegral C.height) 0 (-1) 1
          GL.matrixMode $= GL.Modelview 0
        drawFrame f = do
          GL.clear [GL.ColorBuffer]
          f
          SDL.glSwapBuffers

paintScreen :: (Double, Double, Double) -> IO ()
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
