{-# LANGUAGE MultiParamTypeClasses #-}

module System.Framebuffer (
    getVarScreenInfo
  , getFixScreenInfo
) where

import Data.Word
import System.Posix.IO
import System.Posix.IOCtl
import Control.Applicative ((<$>), (<*>))
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

data Bitfield = Bitfield {
    bitFieldOffset   :: Word32
  , bitFieldLength   :: Word32
  , bitFieldMsbRight :: Word32
} deriving (Show)

instance Storable Bitfield where
  alignment _ = alignment (undefined :: Word32)
  sizeOf _ = 12
  peek ptr = Bitfield
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
  poke ptr (Bitfield o l msb) = do
    pokeByteOff ptr 0 o
    pokeByteOff ptr 4 l
    pokeByteOff ptr 8 msb

data VarScreenInfo = VarScreenInfo {
    varXRes              :: Word32
  , varYRes              :: Word32
  , varXResVirtual       :: Word32
  , varYResVirtual       :: Word32
  , varXOffset           :: Word32
  , varYOffset           :: Word32
  , varBitsPerPixel      :: Word32
  , varGrayscale         :: Word32
  , varRed               :: Bitfield
  , varGreen             :: Bitfield
  , varBlue              :: Bitfield
  , varTransp            :: Bitfield
  , varNonStd            :: Word32
  , varActivate          :: Word32
  , varHeight            :: Word32
  , varWidth             :: Word32
  , varAccelFlags        :: Word32
  , varPixClock          :: Word32
  , varLeftMargin        :: Word32
  , varRightMargin       :: Word32
  , varUpperMargin       :: Word32
  , varLowerMargin       :: Word32
  , varHorizontalSyncLen :: Word32
  , varVerticalSyncLen   :: Word32
  , varSync              :: Word32
  , varVMode             :: Word32
  , varRotate            :: Word32
  , varColorSpace        :: Word32
  , varReserved          :: (Word32, Word32, Word32, Word32)
} deriving (Show)

instance Storable VarScreenInfo where
  alignment _ = alignment (undefined :: Word32)
  sizeOf _ = 160
  peek ptr =
    VarScreenInfo <$> peekByteOff ptr 0
                  <*> peekByteOff ptr 4
                  <*> peekByteOff ptr 8
                  <*> peekByteOff ptr 12
                  <*> peekByteOff ptr 16
                  <*> peekByteOff ptr 20
                  <*> peekByteOff ptr 24
                  <*> peekByteOff ptr 28
                  <*> peekByteOff ptr 32
                  <*> peekByteOff ptr 44
                  <*> peekByteOff ptr 56
                  <*> peekByteOff ptr 68
                  <*> peekByteOff ptr 80
                  <*> peekByteOff ptr 84
                  <*> peekByteOff ptr 88
                  <*> peekByteOff ptr 92
                  <*> peekByteOff ptr 96
                  <*> peekByteOff ptr 100
                  <*> peekByteOff ptr 104
                  <*> peekByteOff ptr 108
                  <*> peekByteOff ptr 112
                  <*> peekByteOff ptr 116
                  <*> peekByteOff ptr 120
                  <*> peekByteOff ptr 124
                  <*> peekByteOff ptr 128
                  <*> peekByteOff ptr 132
                  <*> peekByteOff ptr 136
                  <*> peekByteOff ptr 140
                  <*> pure (0x00, 0x00, 0x00, 0x00)

  poke ptr varInfo = do
    pokeByteOff ptr 0   (varXRes              varInfo)
    pokeByteOff ptr 4   (varYRes              varInfo)
    pokeByteOff ptr 8   (varXResVirtual       varInfo)
    pokeByteOff ptr 12  (varYResVirtual       varInfo)
    pokeByteOff ptr 16  (varXOffset           varInfo)
    pokeByteOff ptr 20  (varYOffset           varInfo)
    pokeByteOff ptr 24  (varBitsPerPixel      varInfo)
    pokeByteOff ptr 28  (varGrayscale         varInfo)
    pokeByteOff ptr 32  (varRed               varInfo)
    pokeByteOff ptr 44  (varGreen             varInfo)
    pokeByteOff ptr 56  (varBlue              varInfo)
    pokeByteOff ptr 68  (varTransp            varInfo)
    pokeByteOff ptr 80  (varNonStd            varInfo)
    pokeByteOff ptr 84  (varActivate          varInfo)
    pokeByteOff ptr 88  (varHeight            varInfo)
    pokeByteOff ptr 92  (varWidth             varInfo)
    pokeByteOff ptr 96  (varAccelFlags        varInfo)
    pokeByteOff ptr 100 (varPixClock          varInfo)
    pokeByteOff ptr 104 (varLeftMargin        varInfo)
    pokeByteOff ptr 108 (varRightMargin       varInfo)
    pokeByteOff ptr 112 (varUpperMargin       varInfo)
    pokeByteOff ptr 116 (varLowerMargin       varInfo)
    pokeByteOff ptr 120 (varHorizontalSyncLen varInfo)
    pokeByteOff ptr 124 (varVerticalSyncLen   varInfo)
    pokeByteOff ptr 128 (varSync              varInfo)
    pokeByteOff ptr 132 (varVMode             varInfo)
    pokeByteOff ptr 136 (varRotate            varInfo)
    pokeByteOff ptr 130 (varColorSpace        varInfo)

data FixScreenInfo = FixScreenInfo {
    fixId           :: String
  , fixMemStart     :: Word32
  , fixMemLength    :: Word32
  , fixType         :: Word32
  , fixTypeAux      :: Word32
  , fixVisual       :: Word32
  , fixXPanStep     :: Word16
  , fixYPanStep     :: Word16
  , fixYWrapStep    :: Word16
  , fixLineLength   :: Word32
  , fixMmioStart    :: Word32
  , fixMmioLen      :: Word32
  , fixAccel        :: Word32
  , fixCapabilities :: Word16
  , fixReserved     :: (Word16, Word16)
} deriving (Show)

instance Storable FixScreenInfo where
  alignment _ = alignment (undefined :: Word32)
  sizeOf _ = 80
  peek ptr = FixScreenInfo <$> peekString16 ptr
                           <*> peekByteOff ptr 16
                           <*> peekByteOff ptr 24
                           <*> peekByteOff ptr 28
                           <*> peekByteOff ptr 32
                           <*> peekByteOff ptr 36
                           <*> peekByteOff ptr 40
                           <*> peekByteOff ptr 42
                           <*> peekByteOff ptr 44
                           <*> peekByteOff ptr 48
                           <*> peekByteOff ptr 56
                           <*> peekByteOff ptr 64
                           <*> peekByteOff ptr 68
                           <*> peekByteOff ptr 72
                           <*> pure (0x00, 0x00)
    where
      peekString16 :: Ptr FixScreenInfo -> IO String
      peekString16 = peekCAString . castPtr

  poke ptr fixInfo = do
    pokeString16 ptr (fixId fixInfo)
    pokeByteOff  ptr 16 (fixMemStart     fixInfo)
    pokeByteOff  ptr 24 (fixMemLength    fixInfo)
    pokeByteOff  ptr 28 (fixType         fixInfo)
    pokeByteOff  ptr 32 (fixTypeAux      fixInfo)
    pokeByteOff  ptr 36 (fixVisual       fixInfo)
    pokeByteOff  ptr 40 (fixXPanStep     fixInfo)
    pokeByteOff  ptr 42 (fixYPanStep     fixInfo)
    pokeByteOff  ptr 44 (fixYWrapStep    fixInfo)
    pokeByteOff  ptr 48 (fixLineLength   fixInfo)
    pokeByteOff  ptr 56 (fixMmioStart    fixInfo)
    pokeByteOff  ptr 64 (fixMmioLen      fixInfo)
    pokeByteOff  ptr 68 (fixAccel        fixInfo)
    pokeByteOff  ptr 72 (fixCapabilities fixInfo)

    where
      pokeString16 :: Ptr FixScreenInfo -> String -> IO ()
      pokeString16 ptr str = do
        withCStringLen str $ \(strPtr, len) ->
          copyArray (castPtr ptr) strPtr (max len 15)
        pokeByteOff ptr 16 (0 :: Word8)

data GETVSCREENINFO = GETVSCREENINFO
data GETFSCREENINFO = GETFSCREENINFO

instance IOControl GETVSCREENINFO VarScreenInfo where
  ioctlReq _ = 0x4600

instance IOControl GETFSCREENINFO FixScreenInfo where
  ioctlReq _ = 0x4602

getVarScreenInfo :: FilePath -> IO VarScreenInfo
getVarScreenInfo p = do
  fd <- openFd p ReadOnly Nothing defaultFileFlags
  ioctl' fd GETVSCREENINFO

getFixScreenInfo :: FilePath -> IO FixScreenInfo
getFixScreenInfo p = do
  fd <- openFd p ReadOnly Nothing defaultFileFlags
  ioctl' fd GETFSCREENINFO
