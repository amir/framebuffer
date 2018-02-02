module Main where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Array (copyArray)
import Data.Word (Word8)
import System.Framebuffer
import Control.Monad (forM_)

main :: IO ()
main =
  withFramebuffer "/dev/fb0" $ \vinfo finfo fbp ->
    forM_ [0 .. varXRes vinfo - 1] $ \row ->
      let dst = fbp `plusPtr` fromIntegral((row + varYOffset vinfo) * fixLineLength finfo)
      in copyArray (dst :: Ptr Word8) (dst :: Ptr Word8) (fromIntegral (varXRes vinfo * 4))
