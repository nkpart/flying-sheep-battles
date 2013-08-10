module Config where

type T3 a = (a,a,a)

nightColor, dayColor :: T3 Double
nightColor = black
dayColor = papayaWhip

height, width, shipW, shipH :: Num a => a
height = 600
width = 400

shipW = 32
shipH = 64

black = (0,0,0)

goldenRod, papayaWhip :: Num a => T3 a
goldenRod = (238, 232, 170)
papayaWhip = (255, 239, 213)
