module Config where

import Data.VectorSpace

type T3 a = (a,a,a)

nightColor, dayColor, groundColor :: T3 Double
nightColor = black
dayColor = niceSky' -- ^* 0.5
groundColor = badAss

height, width, shipW, shipH :: Int
height = 600
width = 400

shipW = 64
shipH = 32

black = (0,0,0)

goldenRod, papayaWhip, yellow, niceSky', badAss :: Num a => T3 a
goldenRod = (238, 232, 170)
papayaWhip = (255, 239, 213)
yellow = (0, 255, 255)

niceSky = (224, 236, 239)
niceSky' = (128, 192, 219)
badAss = (187, 215, 82)
