module Config where

import FSB.Types


gravityMagnitude = 240 :: Double

-- UI
nightColor, dayColor, groundColor :: (Double, Double, Double)
nightColor = black
dayColor = niceSky'
groundColor = badAss

height, width, shipW, shipH :: Int
height = 600
width = 400

shipW = 64
shipH = 32

black = (0,0,0)

goldenRod, papayaWhip, yellow, niceSky', badAss :: Num a => (a,a,a)
goldenRod = (238, 232, 170)
papayaWhip = (255, 239, 213)
yellow = (0, 255, 255)

niceSky = (224, 236, 239)
niceSky' = (128, 192, 219)
badAss = (187, 215, 82)

