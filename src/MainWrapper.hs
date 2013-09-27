{-# LANGUAGE ForeignFunctionInterface #-}
module MainWrapper where
import FlyingSheepBattles (main)
foreign export ccall "haskell_main" main :: IO ()
