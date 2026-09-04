-- | Rasterises a frame's draw commands and text spans into a packed grid of
-- terminal cells. This is the same lowering the vty backend did, minus vty:
-- quads become background fills and spans become characters, so the ANSI
-- writer can diff two frames cell by cell.
module NanoUI.Term.Cells
  ( Cells (..)
  , cellsSize
  , cellChar
  , cellFg
  , cellBg
  , rasterize
  , rasterizeLayered
  , rasterizeLayeredArena
  , cellRows
  , narrowChar
  , wideTrailChar
  ) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimState)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , indexPrimArray
  , mutablePrimArrayContents
  , newPrimArray
  , readPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)

import NanoUI
  ( Color (..)
  , Rect (..)
  , colorToWord32
  , fontAwesomeIcon
  )
import NanoUI.Testing
  ( DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , SpanArena
  , backdropDimTextureId
  , forDrawCmdsInLayer_
  , foldSpanArena
  , indexSize
  , terminalTextColumns
  , terminalTextPositions
  , vertexSize
  , wideTrailChar
  )

foreign import ccall unsafe "nano_ui_term_fill_blanks_simd"
  c_fill_blanks_simd :: Ptr Word32 -> CInt -> IO ()

foreign import ccall unsafe "nano_ui_term_stamp_quad_cells_simd"
  c_stamp_quad_cells_simd :: Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> Word32 -> IO ()

foreign import ccall unsafe "nano_ui_term_blend_dim_simd"
  c_blend_dim_simd :: Ptr Word32 -> CInt -> CInt -> CInt -> CInt -> CInt -> Word32 -> IO ()

-- | Row-major grid, three 'Word32' per cell: codepoint, foreground RGBA,
-- background RGBA. An RGBA whose alpha is below 32 means "terminal default".
data Cells = Cells
  { cellsW :: !Int
  , cellsH :: !Int
  , cellsData :: !(PrimArray Word32)
  }
  deriving (Eq, Show)

cellsSize :: Cells -> (Int, Int)
cellsSize cs = (cellsW cs, cellsH cs)

{-# INLINE cellChar #-}
cellChar :: Cells -> Int -> Int -> Char
cellChar cs x y = chr (fromIntegral (indexPrimArray (cellsData cs) (base cs x y)))

{-# INLINE cellFg #-}
cellFg :: Cells -> Int -> Int -> Word32
cellFg cs x y = indexPrimArray (cellsData cs) (base cs x y + 1)

{-# INLINE cellBg #-}
cellBg :: Cells -> Int -> Int -> Word32
cellBg cs x y = indexPrimArray (cellsData cs) (base cs x y + 2)

{-# INLINE base #-}
base :: Cells -> Int -> Int -> Int
base cs x y = (y * cellsW cs + x) * 3

{-# INLINE clampByte #-}
clampByte :: Float -> Word32
clampByte f = fromIntegral (clamp 0 255 (round (f * 255)))

{-# INLINE packRgba #-}
packRgba :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
packRgba r g b a = (r `shiftL` 24) .|. (g `shiftL` 16) .|. (b `shiftL` 8) .|. a

{-# INLINE isTerminalDefault #-}
isTerminalDefault :: Word32 -> Bool
isTerminalDefault w = (w .&. 0xff) < 32

-- | Character-only view, for snapshot tests.
cellRows :: Cells -> [String]
cellRows cs =
  [ [cellChar cs x y | x <- [0 .. cellsW cs - 1]]
  | y <- [0 .. cellsH cs - 1]
  ]

rasterize :: Int -> Int -> DrawData -> [(Rect, Text, Color, Color, Rect)] -> IO Cells
rasterize width height drawData spans = rasterizeLayered width height drawData spans []

newBlankCellArray :: Int -> Int -> IO (MutablePrimArray (PrimState IO) Word32, Int, Int)
newBlankCellArray width height = do
  let w = max 1 width
      h = max 1 height
      len = w * h * 3
  arr <- newPrimArray len
  let !p = mutablePrimArrayContents arr
  c_fill_blanks_simd p (fromIntegral len)
  pure (arr, w, h)

rasterizeLayered ::
  Int ->
  Int ->
  DrawData ->
  [(Rect, Text, Color, Color, Rect)] ->
  [(Rect, Text, Color, Color, Rect)] ->
  IO Cells
rasterizeLayered width height drawData baseSpans overlaySpans = do
  (arr, w, h) <- newBlankCellArray width height
  forDrawCmdsInLayer_ LayerBackground drawData (applyCmd arr w h drawData)
  forDrawCmdsInLayer_ LayerContent drawData (applyCmd arr w h drawData)
  -- Text spans after content quads so scroll tracks do not erase box rules.
  mapM_ (stampSpan arr w h) baseSpans
  forDrawCmdsInLayer_ LayerOverlay drawData (applyCmd arr w h drawData)
  -- Chrome (window scrollbars) before overlay text. A 1-cell bar expands to 2
  -- cells under floor/ceiling, and would wipe a close-icon trail if it ran last.
  forDrawCmdsInLayer_ LayerChrome drawData (applyCmd arr w h drawData)
  mapM_ (stampSpan arr w h) overlaySpans
  frozen <- unsafeFreezePrimArray arr
  pure Cells {cellsW = w, cellsH = h, cellsData = frozen}

rasterizeLayeredArena ::
  Int ->
  Int ->
  DrawData ->
  SpanArena ->
  SpanArena ->
  IO Cells
rasterizeLayeredArena width height drawData baseArena overlayArena = do
  (arr, w, h) <- newBlankCellArray width height
  let stamp r t fg bg' c = stampSpan arr w h (r, t, fg, bg', c)
  forDrawCmdsInLayer_ LayerBackground drawData (applyCmd arr w h drawData)
  forDrawCmdsInLayer_ LayerContent drawData (applyCmd arr w h drawData)
  foldSpanArena baseArena stamp
  forDrawCmdsInLayer_ LayerOverlay drawData (applyCmd arr w h drawData)
  forDrawCmdsInLayer_ LayerChrome drawData (applyCmd arr w h drawData)
  foldSpanArena overlayArena stamp
  frozen <- unsafeFreezePrimArray arr
  pure Cells {cellsW = w, cellsH = h, cellsData = frozen}

applyCmd ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  DrawData ->
  DrawCmd ->
  IO ()
applyCmd arr w h drawData cmd = go start
  where
    start = fromIntegral (cmdIndexOffset cmd)
    count = fromIntegral (cmdIndexCount cmd)
    end = start + count
    isDim = cmdTextureId cmd == backdropDimTextureId
    go !i
      | i >= end = pure ()
      | otherwise = stampQuad arr w h drawData isDim i >> go (i + 6)

-- | Each quad covers a rectangle of cells; fill them with its colour as the
-- background so a solid rect renders flat regardless of the font's block
-- glyph coverage.
stampQuad ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  DrawData ->
  Bool ->
  Int ->
  IO ()
stampQuad arr w h drawData isDim i = do
  v0 <- vertexAt drawData i
  v1 <- vertexAt drawData (i + 1)
  v2 <- vertexAt drawData (i + 2)
  v3 <- vertexAt drawData (i + 5)
  case (v0, v1, v2, v3) of
    (Just (x0, y0, rgba), Just (x1, y1, _), Just (x2, y2, _), Just (x3, y3, _)) -> do
      let xs = [x0, x1, x2, x3]
          ys = [y0, y1, y2, y3]
          xmin = minimum xs
          xmax = maximum xs
          ymin = minimum ys
          ymax = maximum ys
          -- Fill a cell only when the quad covers its center. floor/ceiling of a
          -- 1-cell bar at *.4 expands to two columns and wipes neighbors.
          firstX = ceiling (xmin - 0.5)
          lastX = floor (xmax - 0.5)
          firstY = ceiling (ymin - 0.5)
          lastY = floor (ymax - 0.5)
          ix = max 0 firstX
          iy = max 0 firstY
          iw = min (lastX - ix + 1) (w - ix)
          ih = min (lastY - iy + 1) (h - iy)
      when (iw > 0 && ih > 0) $
        if isDim
          then stampBackdropDim arr w h ix iy iw ih rgba
          else
            case rgba .&. 0xff of
              n
                | n >= 32 -> do
                    let !p = mutablePrimArrayContents arr
                    c_stamp_quad_cells_simd
                      p
                      (fromIntegral w)
                      (fromIntegral ix)
                      (fromIntegral iy)
                      (fromIntegral iw)
                      (fromIntegral ih)
                      rgba
              _ -> pure ()
    _ -> pure ()
-- | Uniform backdrop dim; keeps glyphs, lerps fg and bg toward dim.
stampBackdropDim ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Word32 ->
  IO ()
stampBackdropDim arr w _ ix iy iw ih rgba = do
  let !p = mutablePrimArrayContents arr
  c_blend_dim_simd
    p
    (fromIntegral w)
    (fromIntegral ix)
    (fromIntegral iy)
    (fromIntegral iw)
    (fromIntegral ih)
    rgba

stampSpan ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  (Rect, Text, Color, Color, Rect) ->
  IO ()
stampSpan arr w h (Rect rx ry rw _rh, txt, fg, bg, clip) =
  mapM_
    ( \(cx, c) -> do
        let px = fromIntegral cx + 0.5
            py = fromIntegral y0 + 0.5
            inClip =
              px >= rectX clip
                && px <= rectX clip + rectW clip
                && py >= rectY clip
                && py <= rectY clip + rectH clip
        if c == wideTrailChar
          then do
            -- Keep the partner even when clipW / pixel clip dropped it.
            let leadCx = cx - 1
            when (leadCx >= 0 && cx < w && y0 >= 0 && y0 < h) $ do
              leadCh <- readPrimArray arr ((y0 * w + leadCx) * 3)
              when (fontAwesomeIcon (chr (fromIntegral leadCh))) $ do
                existingBg <- readPrimArray arr ((y0 * w + cx) * 3 + 2)
                writeCell arr w h cx y0 (fromIntegral (ord c)) fgW existingBg
          else
            when inClip $ do
              breakWidePair arr w h cx y0
              if isTerminalDefault bgW
                then do
                  let off = (y0 * w + cx) * 3
                  writePrimArray arr off (fromIntegral (ord c))
                  writePrimArray arr (off + 1) fgW
                else writeCell arr w h cx y0 (fromIntegral (ord c)) fgW bgW
    )
    positions
  where
    fgW = colorToWord32 fg
    bgW = colorToWord32 bg
    x0 = clamp 0 (w - 1) (round rx)
    y0 = clamp 0 (h - 1) (round ry)
    clipW = min (terminalTextColumns txt) (min (max 0 (ceiling rw)) (w - x0))
    clipEnd = x0 + clipW
    positions = keepWideTrails w clipEnd
      [ (x0 + col, c)
      | (col, c) <- terminalTextPositions txt
      , col < clipW
      , narrowChar c || c == wideTrailChar
      ]

-- Keep a Font Awesome trail only when the paint span reserved that column.
keepWideTrails :: Int -> Int -> [(Int, Char)] -> [(Int, Char)]
keepWideTrails w clipEnd = go
  where
    go [] = []
    go ((cx, c) : rest)
      | fontAwesomeIcon c =
          let rest' = dropWhile (\(x, _) -> x == cx + 1) rest
           in if cx + 1 < w && cx + 1 < clipEnd
                then (cx, c) : (cx + 1, wideTrailChar) : go rest'
                else (cx, c) : go rest'
      | otherwise = (cx, c) : go rest

-- A later span (button '[', title text) must not sit in a wide-glyph trail
-- cell. That pair would emit as one glyph plus a real character and shift the
-- rest of the row.
breakWidePair ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO ()
breakWidePair arr w h x y
  | x <= 0 || x >= w || y < 0 || y >= h = pure ()
  | otherwise = do
      leadCh <- readPrimArray arr ((y * w + (x - 1)) * 3)
      when (fontAwesomeIcon (chr (fromIntegral leadCh))) $ do
        leadBg <- readPrimArray arr ((y * w + (x - 1)) * 3 + 2)
        writeCell arr w h (x - 1) y 32 leadBg leadBg

{-# INLINE writeCell #-}
writeCell ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Word32 ->
  Word32 ->
  Word32 ->
  IO ()
writeCell arr w h x y ch fg bg
  | x < 0 || y < 0 || x >= w || y >= h = pure ()
  | otherwise = do
      let off = (y * w + x) * 3
      writePrimArray arr off ch
      writePrimArray arr (off + 1) fg
      writePrimArray arr (off + 2) bg

vertexAt :: DrawData -> Int -> IO (Maybe (Float, Float, Word32))
vertexAt dd slot = do
  mVi <- readIndexAt dd slot
  case mVi of
    Nothing -> pure Nothing
    Just vi
      | vi < 0 || vi >= drawVertexCount dd -> pure Nothing
      | otherwise -> do
          let !off = vi * vertexSize
          x <- peekFloatAt (drawVertices dd) off
          y <- peekFloatAt (drawVertices dd) (off + 4)
          r <- peekFloatAt (drawVertices dd) (off + 8)
          g <- peekFloatAt (drawVertices dd) (off + 12)
          b <- peekFloatAt (drawVertices dd) (off + 16)
          a <- peekFloatAt (drawVertices dd) (off + 20)
          let !rgba = packRgba (clampByte r) (clampByte g) (clampByte b) (clampByte a)
          pure (Just (x, y, rgba))

readIndexAt :: DrawData -> Int -> IO (Maybe Int)
readIndexAt dd i
  | i < 0 || i >= drawIndexCount dd = pure Nothing
  | otherwise =
      Just . fromIntegral <$> peekWord32At (drawIndices dd) (i * indexSize)

peekWord32At :: ForeignPtr Word8 -> Int -> IO Word32
peekWord32At fp off = withForeignPtr fp $ \p -> peekByteOff p off

peekFloatAt :: ForeignPtr Word8 -> Int -> IO Float
peekFloatAt fp off = withForeignPtr fp $ \p -> peekByteOff p off

clamp :: Int -> Int -> Int -> Int
clamp lo hi v = max lo (min hi v)

-- | Terminals advance the cursor one column per emitted character. Wide
-- codepoints would desync the diff writer, so reject known double-width
-- ranges while keeping the box-drawing and block glyphs the TUI uses.
narrowChar :: Char -> Bool
narrowChar c =
  let o = ord c
   in not (isWideEastAsian o)
  where
    isWideEastAsian o =
      o >= 0x1100 && o <= 0x115F
        || o >= 0x2E80 && o <= 0xA4CF
        || o >= 0xAC00 && o <= 0xD7AF
        || o >= 0xF900 && o <= 0xFAFF
        || o >= 0xFE10 && o <= 0xFE6F
        || o >= 0xFF01 && o <= 0xFF60
        || o >= 0xFFE0 && o <= 0xFFE6
        || o >= 0x20000
