module FSB.Controls where

import Prelude hiding ((.))
import FSB.Types
import Graphics.UI.SDL as SDL
import Control.Wire
import qualified Data.Set as Set (insert, toList, delete, empty)
import Data.Set (Set)
import Data.Maybe (mapMaybe)

sdlThrustControl = keysDownW >>> (arr (([] ++) . applyControls player1) &&& arr (([] ++) . applyControls player2))

keysDownW = mkStateM Set.empty $ \_ (_, keys) -> do
  newKeys <- parseEvents keys
  return (Right newKeys, newKeys)

player1 :: Controls SDL.SDLKey
player1 v = case v of
              SDL.SDLK_a -> Just ThrustLeft
              SDL.SDLK_d -> Just ThrustRight
              SDL.SDLK_s -> Just ThrustDown
              SDL.SDLK_w -> Just ThrustUp
              SDL.SDLK_LSHIFT -> Just ThrustBoost
              _ -> Nothing
    
player2 :: Controls SDL.SDLKey
player2 v = case v of
              SDL.SDLK_LEFT -> Just ThrustLeft
              SDL.SDLK_RIGHT -> Just ThrustRight
              SDL.SDLK_DOWN -> Just ThrustDown
              SDL.SDLK_UP -> Just ThrustUp
              SDL.SDLK_RSHIFT -> Just ThrustBoost
              _ -> Nothing

parseEvents :: Set SDL.SDLKey -> IO (Set SDL.SDLKey)
parseEvents keysDown = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k | SDL.symKey k == SDL.SDLK_q && elem SDL.KeyModLeftMeta (SDL.symModifiers k) -> error "Done"
    SDL.KeyDown k -> parseEvents (Set.insert (SDL.symKey k) keysDown)
    SDL.KeyUp k -> parseEvents (Set.delete (SDL.symKey k) keysDown)
    SDL.Quit -> error "Done"
    _ -> parseEvents keysDown

applyControls f = mapMaybe f . Set.toList
