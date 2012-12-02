module SDLTemplate where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI

import qualified DynPaths_SDLTemplate as DPaths
import System.FilePath ((</>))


resourcePath :: FilePath -> FilePath
resourcePath res_name = DPaths.resourceDir </> res_name


main :: IO ()
main = do
    screen <- SDL.setVideoMode 640 480 32 [SDL.SWSurface]
    hello <- SDLI.load (resourcePath "image.png")

    SDL.blitSurface hello Nothing screen Nothing
    SDL.flip screen
    SDL.delay 2000
