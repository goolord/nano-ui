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
  , cellRows
  , narrowChar
  ) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimState)
import Data.Char (chr, ord)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , indexPrimArray
  , newPrimArray
  , setPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Storable (peekByteOff)
import NanoUI
  ( Color
  , DrawCmd (..)
  , DrawData (..)
  , Layer (..)
  , Rect (..)
  , V2 (..)
  , colorToWord32
  , indexSize
  , vertexSize
  )
import NanoUI (rectContains)
import qualified Data.Text as T

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

-- | Character-only view, for snapshot tests.
cellRows :: Cells -> [String]
cellRows cs =
  [ [cellChar cs x y | x <- [0 .. cellsW cs - 1]]
  | y <- [0 .. cellsH cs - 1]
  ]

rasterize :: Int -> Int -> DrawData -> [(Rect, Text, Color, Color, Rect)] -> IO Cells
rasterize width height drawData spans = rasterizeLayered width height drawData spans []

rasterizeLayered ::
  Int ->
  Int ->
  DrawData ->
  [(Rect, Text, Color, Color, Rect)] ->
  [(Rect, Text, Color, Color, Rect)] ->
  IO Cells
rasterizeLayered width height drawData baseSpans overlaySpans = do
  let w = max 1 width
      h = max 1 height
      len = w * h * 3
  arr <- newPrimArray len
  setPrimArray arr 0 len 0
  fillBlanks arr len
  let cmds = drawCommands drawData
      ofLayer ly = filter ((== ly) . cmdLayer) cmds
  mapM_ (applyCmd arr w h drawData) (ofLayer LayerBackground)
  mapM_ (stampSpan arr w h) baseSpans
  mapM_ (applyCmd arr w h drawData) (ofLayer LayerContent)
  mapM_ (applyCmd arr w h drawData) (ofLayer LayerOverlay)
  mapM_ (stampSpan arr w h) overlaySpans
  mapM_ (applyCmd arr w h drawData) (ofLayer LayerChrome)
  frozen <- unsafeFreezePrimArray arr
  pure Cells {cellsW = w, cellsH = h, cellsData = frozen}
  where
    fillBlanks arr len = go 0
      where
        go i
          | i >= len = pure ()
          | otherwise = writePrimArray arr i 32 >> go (i + 3)

applyCmd ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  DrawData ->
  DrawCmd ->
  IO ()
applyCmd arr w h drawData cmd = mapM_ (stampQuad arr w h drawData) [start, start + 6 .. end - 1]
  where
    start = fromIntegral (cmdIndexOffset cmd)
    count = fromIntegral (cmdIndexCount cmd)
    end = start + count

-- | Each quad covers a rectangle of cells; fill them with its colour as the
-- background so a solid rect renders flat regardless of the font's block
-- glyph coverage.
stampQuad ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  DrawData ->
  Int ->
  IO ()
stampQuad arr w h drawData i = do
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
          ix = clamp 0 (w - 1) (floor xmin)
          iy = clamp 0 (h - 1) (floor ymin)
          iw = min (ceiling xmax - ix) (w - ix)
          ih = min (ceiling ymax - iy) (h - iy)
      when (iw > 0 && ih > 0) $
        mapM_
          ( \dy ->
              mapM_
                (\dx -> writeCell arr w h (ix + dx) (iy + dy) 32 rgba rgba)
                [0 .. iw - 1]
          )
          [0 .. ih - 1]
    _ -> pure ()

stampSpan ::
  MutablePrimArray (PrimState IO) Word32 ->
  Int ->
  Int ->
  (Rect, Text, Color, Color, Rect) ->
  IO ()
stampSpan arr w h (Rect rx ry rw _rh, txt, fg, bg, clip) =
  mapM_
    (\(dx, c) -> do
      let cx = x0 + dx
      when (rectContains clip (V2 (fromIntegral cx + 0.5) (fromIntegral y0 + 0.5))) $
        writeCell arr w h cx y0 (fromIntegral (ord c)) fgW bgW
    )
    (zip [0 ..] chars)
  where
    fgW = colorToWord32 fg
    bgW = colorToWord32 bg
    x0 = clamp 0 (w - 1) (round rx)
    y0 = clamp 0 (h - 1) (round ry)
    n = min (T.length txt) (min (max 0 (round rw)) (w - x0))
    chars = filter narrowChar (take n (T.unpack txt))

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
          x <- peekFloatAt (drawVertices dd) (vi * vertexSize)
          y <- peekFloatAt (drawVertices dd) (vi * vertexSize + 4)
          rgba <- peekWord32At (drawVertices dd) (vi * vertexSize + 16)
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
