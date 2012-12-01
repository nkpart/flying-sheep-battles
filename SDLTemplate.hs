module SDLTemplate where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI

import System.Environment.FindBin
import System.FilePath
import System.IO.Unsafe


resourcePath :: FilePath -> FilePath
resourcePath res_name = combine res_path res_name where
    -- Assuming the path to binary will not change and saving ourselves a lot of work
    bin_path = unsafePerformIO getProgPath
    res_path = combine (dropFileName bin_path) "Resources"


main :: IO ()
main = do
    screen <- SDL.setVideoMode 640 480 32 [SDL.SWSurface]
    hello <- SDLI.load (resourcePath "image.png")

    SDL.blitSurface hello Nothing screen Nothing
    SDL.flip screen
    SDL.delay 2000
