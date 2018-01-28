module Main where

import System.Framebuffer

main :: IO ()
main = do
  v <- getVarScreenInfo "/dev/fb0"
  print v
