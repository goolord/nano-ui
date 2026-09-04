{-# LANGUAGE BangPatterns #-}

module NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , newRgfwSurface
  , newOffscreenRgfwSurface
  , resizeRgfwSurface
  , freeRgfwSurface
  , clearScreen
  , pushClip
  , popClip
  , fillRect
  , drawRectOutline
  , drawText
  , drawTextScaled
  , toPhysRect
  , packColor
  , currentClip
  , upscaleSurface
  ) where

import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , readPrimArray
  , writePrimArray
  )
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import NanoUI (Color (..))
import NanoUI.Rgfw.Font.Cozette
  ( CozetteFont
  , renderTextScaledToBuffer
  , renderTextToBuffer
  )
import qualified RGFW

data ClipRect = ClipRect
  { crX0 :: {-# UNPACK #-} !Int
  , crY0 :: {-# UNPACK #-} !Int
  , crX1 :: {-# UNPACK #-} !Int
  , crY1 :: {-# UNPACK #-} !Int
  }

data RgfwSurface = RgfwSurface
  { sWidth       :: {-# UNPACK #-} !Int
  , sHeight      :: {-# UNPACK #-} !Int
  , sBuffer      :: {-# UNPACK #-} !(Ptr Word32)
  , sRgfwSurface :: {-# UNPACK #-} !RGFW.Surface
  , sClipArr     :: {-# UNPACK #-} !(MutablePrimArray RealWorld Int)
  , sClipDepth   :: {-# UNPACK #-} !(IORef Int)
  }

-- | Converts NanoUI Color to native BGRA32 pixel word
{-# INLINE packColor #-}
packColor :: Color -> Word32
packColor (Color w) =
  let !r = (w `shiftR` 24) .&. 0xFF
      !g = (w `shiftR` 16) .&. 0xFF
      !b = (w `shiftR` 8) .&. 0xFF
      !a = w .&. 0xFF
   in (a `shiftL` 24) .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b

initClipStack :: Int -> Int -> IO (MutablePrimArray RealWorld Int, IORef Int)
initClipStack !w !h = do
  arr <- newPrimArray 256
  writePrimArray arr 0 0
  writePrimArray arr 1 0
  writePrimArray arr 2 w
  writePrimArray arr 3 h
  ref <- newIORef 0
  pure (arr, ref)

newRgfwSurface :: RGFW.Window -> Int -> Int -> IO RgfwSurface
newRgfwSurface win w h = do
  let !safeW = max 1 w
      !safeH = max 1 h
      !numBytes = safeW * safeH * 4
  buf <- mallocBytes numBytes
  surf <- RGFW.createSurface win (castPtr buf) safeW safeH RGFW.rgfw_formatBGRA8
  (clipArr, depthRef) <- initClipStack safeW safeH
  pure $ RgfwSurface safeW safeH buf surf clipArr depthRef

newOffscreenRgfwSurface :: Int -> Int -> IO RgfwSurface
newOffscreenRgfwSurface w h = do
  let !safeW = max 1 w
      !safeH = max 1 h
      !numBytes = safeW * safeH * 4
  buf <- mallocBytes numBytes
  (clipArr, depthRef) <- initClipStack safeW safeH
  pure $ RgfwSurface safeW safeH buf (RGFW.Surface nullPtr) clipArr depthRef

resizeRgfwSurface :: RGFW.Window -> RgfwSurface -> Int -> Int -> IO RgfwSurface
resizeRgfwSurface win surf newW newH = do
  if newW == sWidth surf && newH == sHeight surf
    then pure surf
    else do
      freeRgfwSurface surf
      newRgfwSurface win newW newH

freeRgfwSurface :: RgfwSurface -> IO ()
freeRgfwSurface surf = do
  let RGFW.Surface p = sRgfwSurface surf
  when (p /= nullPtr) $
    RGFW.freeSurface (sRgfwSurface surf)
  free (sBuffer surf)

{-# INLINE clearScreen #-}
clearScreen :: RgfwSurface -> Word32 -> IO ()
clearScreen surf color = do
  let !total = sWidth surf * sHeight surf
      !ptr = sBuffer surf
      !w64 = (fromIntegral color `shiftL` 32) .|. fromIntegral color :: Word64
      !ptr64 = castPtr ptr :: Ptr Word64
      !total64 = total `shiftR` 1

      fill8 !i
        | i + 7 < total64 = do
            pokeElemOff ptr64 i w64
            pokeElemOff ptr64 (i + 1) w64
            pokeElemOff ptr64 (i + 2) w64
            pokeElemOff ptr64 (i + 3) w64
            pokeElemOff ptr64 (i + 4) w64
            pokeElemOff ptr64 (i + 5) w64
            pokeElemOff ptr64 (i + 6) w64
            pokeElemOff ptr64 (i + 7) w64
            fill8 (i + 8)
        | i < total64 = do
            pokeElemOff ptr64 i w64
            fill8 (i + 1)
        | otherwise = pure ()
  fill8 0
  when ((total .&. 1) /= 0) $
    pokeElemOff ptr (total - 1) color

{-# INLINE currentClip #-}
currentClip :: RgfwSurface -> IO ClipRect
currentClip surf = do
  !d <- readIORef (sClipDepth surf)
  let !base = d * 4
      !arr = sClipArr surf
  !x0 <- readPrimArray arr base
  !y0 <- readPrimArray arr (base + 1)
  !x1 <- readPrimArray arr (base + 2)
  !y1 <- readPrimArray arr (base + 3)
  pure (ClipRect x0 y0 x1 y1)

{-# INLINE pushClip #-}
pushClip :: RgfwSurface -> Int -> Int -> Int -> Int -> IO ()
pushClip surf x y w h = do
  !d <- readIORef (sClipDepth surf)
  let !base = d * 4
      !arr = sClipArr surf
  !cx0 <- readPrimArray arr base
  !cy0 <- readPrimArray arr (base + 1)
  !cx1 <- readPrimArray arr (base + 2)
  !cy1 <- readPrimArray arr (base + 3)
  let !nx0 = max cx0 x
      !ny0 = max cy0 y
      !nx1 = min cx1 (x + w)
      !ny1 = min cy1 (y + h)
      !nextBase = base + 4
  when (nextBase + 3 < 256) $ do
    writePrimArray arr nextBase nx0
    writePrimArray arr (nextBase + 1) ny0
    writePrimArray arr (nextBase + 2) (max nx0 nx1)
    writePrimArray arr (nextBase + 3) (max ny0 ny1)
    writeIORef (sClipDepth surf) (d + 1)

{-# INLINE popClip #-}
popClip :: RgfwSurface -> IO ()
popClip surf = do
  !d <- readIORef (sClipDepth surf)
  when (d > 0) $
    writeIORef (sClipDepth surf) (d - 1)

{-# INLINE fillSpan #-}
fillSpan :: Ptr Word32 -> Int -> Int -> Word32 -> Word64 -> IO ()
fillSpan !ptr !startIdx !len !c32 !c64
  | len <= 0 = pure ()
  | len < 8 = do
      let small !k
            | k >= len = pure ()
            | otherwise = do
                pokeElemOff ptr (startIdx + k) c32
                small (k + 1)
      small 0
  | otherwise = do
      let (!alignedStart, !remLen) =
            if (startIdx .&. 1) /= 0
              then (startIdx + 1, len - 1)
              else (startIdx, len)
      when ((startIdx .&. 1) /= 0) $
        pokeElemOff ptr startIdx c32
      let !p64 = castPtr (ptr `plusPtr` (alignedStart * 4)) :: Ptr Word64
          !numPairs = remLen `shiftR` 1
          loop4 !k
            | k + 3 < numPairs = do
                pokeElemOff p64 k c64
                pokeElemOff p64 (k + 1) c64
                pokeElemOff p64 (k + 2) c64
                pokeElemOff p64 (k + 3) c64
                loop4 (k + 4)
            | k < numPairs = do
                pokeElemOff p64 k c64
                loop4 (k + 1)
            | otherwise = pure ()
      loop4 0
      when ((remLen .&. 1) /= 0) $
        pokeElemOff ptr (alignedStart + remLen - 1) c32

{-# INLINE fillRect #-}
fillRect :: RgfwSurface -> Int -> Int -> Int -> Int -> Word32 -> IO ()
fillRect surf x y w h color = do
  clip <- currentClip surf
  let !x0 = max (crX0 clip) x
      !y0 = max (crY0 clip) y
      !x1 = min (crX1 clip) (x + w)
      !y1 = min (crY1 clip) (y + h)
  if x0 >= x1 || y0 >= y1
    then pure ()
    else do
      let !stride = sWidth surf
          !ptr = sBuffer surf
          !len = x1 - x0
          !w64 = (fromIntegral color `shiftL` 32) .|. fromIntegral color :: Word64

          rowLoop !cy
            | cy >= y1 = pure ()
            | otherwise = do
                let !rowStart = cy * stride + x0
                fillSpan ptr rowStart len color w64
                rowLoop (cy + 1)
      rowLoop y0

{-# INLINE drawRectOutline #-}
drawRectOutline :: RgfwSurface -> Int -> Int -> Int -> Int -> Word32 -> IO ()
drawRectOutline surf x y w h color
  | w <= 0 || h <= 0 = pure ()
  | w <= 2 || h <= 2 = fillRect surf x y w h color
  | otherwise = do
      fillRect surf x y w 1 color -- Top
      fillRect surf x (y + h - 1) w 1 color -- Bottom
      fillRect surf x (y + 1) 1 (h - 2) color -- Left
      fillRect surf (x + w - 1) (y + 1) 1 (h - 2) color -- Right

{-# INLINE toPhysRect #-}
toPhysRect :: Float -> Float -> Float -> Float -> Float -> (Int, Int, Int, Int)
toPhysRect !scale !rx !ry !rw !rh =
  let !x0 = round (rx * scale)
      !y0 = round (ry * scale)
      !x1 = round ((rx + rw) * scale)
      !y1 = round ((ry + rh) * scale)
   in (x0, y0, max 0 (x1 - x0), max 0 (y1 - y0))

{-# INLINE drawText #-}
drawText :: RgfwSurface -> CozetteFont -> Int -> Int -> Text -> Word32 -> IO ()
drawText surf font x y txt color = do
  clip <- currentClip surf
  renderTextToBuffer
    (sBuffer surf)
    (sWidth surf)
    (crX0 clip)
    (crY0 clip)
    (crX1 clip)
    (crY1 clip)
    x
    y
    color
    font
    txt

{-# INLINE drawTextScaled #-}
drawTextScaled :: RgfwSurface -> CozetteFont -> Float -> Float -> Float -> Text -> Word32 -> IO ()
drawTextScaled surf font !scale !logX !logY txt color = do
  clip <- currentClip surf
  renderTextScaledToBuffer
    (sBuffer surf)
    (sWidth surf)
    (crX0 clip)
    (crY0 clip)
    (crX1 clip)
    (crY1 clip)
    scale
    logX
    logY
    color
    font
    txt

-- | Integer nearest-neighbor upscale from logical surface to physical surface.
upscaleSurface :: RgfwSurface -> RgfwSurface -> Int -> IO ()
upscaleSurface src dst scale
  | scale <= 1 = do
      let !bytes = min (sWidth src * sHeight src * 4) (sWidth dst * sHeight dst * 4)
      copyBytes (castPtr (sBuffer dst)) (castPtr (sBuffer src)) bytes
  | otherwise = do
      let !sw = sWidth src
          !sh = sHeight src
          !dw = sWidth dst
          !dh = sHeight dst
          !maxSy = min sh (dh `div` scale)
          !maxSx = min sw (dw `div` scale)
          !sPtr = sBuffer src
          !dPtr = sBuffer dst
          !rowBytes = maxSx * scale * 4

      let loopY !sy
            | sy >= maxSy = pure ()
            | otherwise = do
                let !baseDy = sy * scale
                    !dRow0 = dPtr `plusPtr` (baseDy * dw * 4)
                    loopX !sx
                      | sx >= maxSx = pure ()
                      | otherwise = do
                          !pix <- peekElemOff sPtr (sy * sw + sx)
                          let !baseDx = sx * scale
                              fillDx !k
                                | k >= scale = pure ()
                                | otherwise = do
                                    pokeElemOff (castPtr dRow0) (baseDx + k) pix
                                    fillDx (k + 1)
                          fillDx 0
                          loopX (sx + 1)
                loopX 0
                -- Replicate row for remaining scale-1 lines
                let repY !dy
                      | dy >= scale || baseDy + dy >= dh = pure ()
                      | otherwise = do
                          let !dRowDy = dPtr `plusPtr` ((baseDy + dy) * dw * 4)
                          copyBytes dRowDy dRow0 rowBytes
                          repY (dy + 1)
                repY 1
                loopY (sy + 1)
      loopY 0
