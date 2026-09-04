{-# LANGUAGE BangPatterns #-}

module NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , newRgfwSurface
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

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
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
  , sClipStack   :: {-# UNPACK #-} !(IORef [ClipRect])
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

newRgfwSurface :: RGFW.Window -> Int -> Int -> IO RgfwSurface
newRgfwSurface win w h = do
  let !safeW = max 1 w
      !safeH = max 1 h
      !numBytes = safeW * safeH * 4
  buf <- mallocBytes numBytes
  surf <- RGFW.createSurface win (castPtr buf) safeW safeH RGFW.rgfw_formatBGRA8
  clipRef <- newIORef [ClipRect 0 0 safeW safeH]
  pure $ RgfwSurface safeW safeH buf surf clipRef

resizeRgfwSurface :: RGFW.Window -> RgfwSurface -> Int -> Int -> IO RgfwSurface
resizeRgfwSurface win surf newW newH = do
  if newW == sWidth surf && newH == sHeight surf
    then pure surf
    else do
      freeRgfwSurface surf
      newRgfwSurface win newW newH

freeRgfwSurface :: RgfwSurface -> IO ()
freeRgfwSurface surf = do
  RGFW.freeSurface (sRgfwSurface surf)
  free (sBuffer surf)

clearScreen :: RgfwSurface -> Word32 -> IO ()
clearScreen surf color = do
  let !total = sWidth surf * sHeight surf
      !ptr = sBuffer surf
      fill !i
        | i >= total = pure ()
        | otherwise = do
            pokeElemOff ptr i color
            fill (i + 1)
  fill 0

{-# INLINE currentClip #-}
currentClip :: RgfwSurface -> IO ClipRect
currentClip surf = do
  stack <- readIORef (sClipStack surf)
  case stack of
    (c : _) -> pure c
    [] -> pure (ClipRect 0 0 (sWidth surf) (sHeight surf))

pushClip :: RgfwSurface -> Int -> Int -> Int -> Int -> IO ()
pushClip surf x y w h = do
  stack <- readIORef (sClipStack surf)
  let !cur = case stack of
        (c : _) -> c
        [] -> ClipRect 0 0 (sWidth surf) (sHeight surf)
      !nx0 = max (crX0 cur) x
      !ny0 = max (crY0 cur) y
      !nx1 = min (crX1 cur) (x + w)
      !ny1 = min (crY1 cur) (y + h)
  writeIORef (sClipStack surf) (ClipRect nx0 ny0 (max nx0 nx1) (max ny0 ny1) : stack)

popClip :: RgfwSurface -> IO ()
popClip surf = do
  stack <- readIORef (sClipStack surf)
  case stack of
    (_ : rest@(_ : _)) -> writeIORef (sClipStack surf) rest
    _ -> writeIORef (sClipStack surf) [ClipRect 0 0 (sWidth surf) (sHeight surf)]

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
          rowLoop !cy
            | cy >= y1 = pure ()
            | otherwise = do
                let !rowStart = cy * stride
                    colLoop !cx
                      | cx >= x1 = pure ()
                      | otherwise = do
                          pokeElemOff ptr (rowStart + cx) color
                          colLoop (cx + 1)
                colLoop x0
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
