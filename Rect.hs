module Rect where

import Graphics.UI.SDL
import Control.Lens

rectX', rectY', rectW', rectH' :: Lens Rect Rect Int Int

rectX' = lens rectX (\r n -> r { rectX = n })
rectY' = lens rectY (\r n -> r { rectY = n })
rectW' = lens rectW (\r n -> r { rectW = n })
rectH' = lens rectH (\r n -> r { rectH = n })
