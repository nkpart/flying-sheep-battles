module FSB.Renderer where

import FSB.Types

newtype Renderer = Renderer {
                     render :: World -> GameState -> Sky -> IO ()
                   }
