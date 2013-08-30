{-# LANGUAGE ForeignFunctionInterface #-}
module MainWrapper where
import SDLTemplate (main)
foreign export ccall "haskell_main" main :: IO ()
