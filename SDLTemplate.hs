module SDLTemplate where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI

main :: IO ()
main = do
    screen <- SDL.setVideoMode 640 480 32 [SDL.SWSurface]
    hello <- SDLI.load "image.png"

    SDL.blitSurface hello Nothing screen Nothing
    SDL.flip screen
    SDL.delay 2000
