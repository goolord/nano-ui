-- | Software pixel surface: a BGRA buffer with a clip stack, for the
-- reference rasterizer ('NanoUI.Rgfw.Render') the tests check frames with.
module NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , newOffscreenRgfwSurface
  , freeRgfwSurface
  , clearScreen
  , pushClip
  , popClip
  , fillRect
  , drawTextScaled
  , packColor
  ) where

import Control.Exception (bracketOnError)
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
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeElemOff)
import NanoUI (Color (..))
import NanoUI.Rgfw.Internal.Font.Cozette (CozetteFont, foldPenPositions, renderGlyphScaledToBuffer)

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

newOffscreenRgfwSurface :: Int -> Int -> IO RgfwSurface
newOffscreenRgfwSurface w h = do
  let !safeW = max 1 w
      !safeH = max 1 h
  when (safeW > maxBound `div` 4 `div` safeH) $
    fail "RGFW surface dimensions overflow the pixel buffer size"
  bracketOnError (mallocBytes (safeW * safeH * 4)) free $ \buf -> do
    (clipArr, depthRef) <- initClipStack safeW safeH
    pure $ RgfwSurface safeW safeH buf clipArr depthRef

freeRgfwSurface :: RgfwSurface -> IO ()
freeRgfwSurface = free . sBuffer

{-# INLINE clearScreen #-}
clearScreen :: RgfwSurface -> Word32 -> IO ()
clearScreen surf color =
  fillSpan (sBuffer surf) 0 (sWidth surf * sHeight surf) color (pixelPair color)

{-# INLINE pixelPair #-}
pixelPair :: Word32 -> Word64
pixelPair color = (fromIntegral color `shiftL` 32) .|. fromIntegral color

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
          !w64 = pixelPair color

          rowLoop !cy
            | cy >= y1 = pure ()
            | otherwise = do
                let !rowStart = cy * stride + x0
                fillSpan ptr rowStart len color w64
                rowLoop (cy + 1)
      rowLoop y0

{-# INLINE drawTextScaled #-}
drawTextScaled :: RgfwSurface -> CozetteFont -> Float -> Float -> Float -> Text -> Word32 -> IO ()
drawTextScaled surf font !scale !logX !logY txt color = do
  clip <- currentClip surf
  foldPenPositions font scale logX logY () (\() penX penY gid ->
    renderGlyphScaledToBuffer (sBuffer surf) (sWidth surf) (crX0 clip) (crY0 clip) (crX1 clip) (crY1 clip) scale penX penY color font gid) txt
