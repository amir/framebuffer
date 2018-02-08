module Main where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Marshal.Array (copyArray)
import Data.Word (Word8)
import System.Framebuffer
import Control.Monad (forM_)
import Data.Vector.Storable (generate, unsafeWith, Vector)

main :: IO ()
main =
  withFramebuffer "/dev/fb0" $ \vinfo finfo fbp ->
    forM_ [0 .. varXRes vinfo - 1] $ \row ->
      let dst = fbp `plusPtr` fromIntegral((row + varYOffset vinfo) * fixLineLength finfo)
          src = generate (fromIntegral(fixLineLength finfo)) (const 1) :: Vector Word8
      in unsafeWith src $ \srcPtr ->
        copyArray (dst :: Ptr Word8) srcPtr (fromIntegral(fixLineLength finfo))
