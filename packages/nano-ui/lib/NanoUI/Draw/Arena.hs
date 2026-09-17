{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Draw arena lifecycle, command batching and vertex reservation. The shape
-- and text emitters reserve room here and poke vertices straight into the
-- pinned buffers.
module NanoUI.Draw.Arena
  ( newDrawArena
  , resetDrawArena
  , setDrawSnapScale
  , getDrawSnapScale
  , setDrawSquareGeometry
  , setDrawExternalText
  , beginLayer
  , currentLayer
  , setClip
  , withClip
  , setTexture
  , finishDraw
  , withVerts
  , withVertsRaw
  , withVertsReserve
  , pushQuad
  , snapRectOrigin
  , unpackColorF
  , pokeQuadIndices
  , loopIO
  , whitePixelU
  , whitePixelV
  ) where

import Control.Monad (unless, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , newPrimArray
  , readPrimArray
  , setPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  , resizeMutablePrimArray
  )
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (RealWorld)
import NanoUI.Draw.Types
import NanoUI.SIMD (pokeQuadSIMD)
import NanoUI.Types (Color (..), Rect (..), onGrid, rectIntersect)

vertexCapacity :: Int
vertexCapacity = 4096

indexCapacity :: Int
indexCapacity = 8192

bufferPoolLimit :: Int
bufferPoolLimit = 4

cmdInitialCapacity :: Int
cmdInitialCapacity = 64

newDrawArena :: IO DrawArena
newDrawArena = do
  vFPtr <- mallocForeignPtrBytes (vertexCapacity * vertexSize)
  iFPtr <- mallocForeignPtrBytes (indexCapacity * indexSize)
  daVertexFPtr <- newIORef vFPtr
  daVertexPtr <- newIORef (unsafeForeignPtrToPtr vFPtr)
  daVertexCap <- newIORef vertexCapacity
  daVertexCount <- newIORef 0
  daVertexPool <- newIORef []
  daIndexFPtr <- newIORef iFPtr
  daIndexPtr <- newIORef (unsafeForeignPtrToPtr iFPtr)
  daIndexCap <- newIORef indexCapacity
  daIndexCount <- newIORef 0
  daIndexPool <- newIORef []
  daCmdStore <- newIORef =<< newPrimArray cmdInitialCapacity
  daCmdCount <- newIORef 0
  daCmdCapacity <- newIORef cmdInitialCapacity
  daCurrentLayer <- newIORef LayerContent
  daCurrentClip <- newIORef (0, 0, 1e9, 1e9)
  daCurrentTexture <- newIORef glyphAtlasTextureId
  daCmdStartIndex <- newIORef 0
  daSnapScale <- newIORef 0.0
  daSquareGeometry <- newIORef False
  daExternalText <- newIORef False
  pure DrawArena {..}

resetDrawArena :: DrawArena -> IO ()
resetDrawArena da = do
  writeIORef (daVertexCount da) 0
  writeIORef (daIndexCount da) 0
  writeIORef (daCmdCount da) 0
  writeIORef (daCurrentLayer da) LayerContent
  writeIORef (daCurrentClip da) (0, 0, 1e9, 1e9)
  writeIORef (daCurrentTexture da) glyphAtlasTextureId
  writeIORef (daCmdStartIndex da) 0

-- | Device pixel scale used to snap primitive origins/endpoints to whole
-- device pixels. A non-positive value disables snapping. The SDL backend keeps
-- this in sync with the display scale; cell and headless hosts leave it
-- disabled so their grid/ASCII rasterizers keep their original coordinates.
{-# INLINE setDrawSnapScale #-}
setDrawSnapScale :: DrawArena -> Float -> IO ()
setDrawSnapScale da s = writeIORef (daSnapScale da) (if s > 0 then s else 0)

{-# INLINE getDrawSnapScale #-}
getDrawSnapScale :: DrawArena -> IO Float
getDrawSnapScale da = readIORef (daSnapScale da)

-- | Square geometry for hosts that rasterize flat, axis-aligned fills (software
-- framebuffers, cell grids). Rounded rects, circles and their strokes lower to
-- plain rects, and coverage-AA strips lower to solid quads with no
-- transparent fringe vertices. Persists across 'resetDrawArena'.
{-# INLINE setDrawSquareGeometry #-}
setDrawSquareGeometry :: DrawArena -> Bool -> IO ()
setDrawSquareGeometry da = writeIORef (daSquareGeometry da)

-- | External text for hosts that rasterize text themselves from the collected
-- text spans. Text emitters push no quads, so fonts without a glyph atlas do
-- not leave per-character advance boxes in the buffer. Persists across
-- 'resetDrawArena'.
{-# INLINE setDrawExternalText #-}
setDrawExternalText :: DrawArena -> Bool -> IO ()
setDrawExternalText da = writeIORef (daExternalText da)

{-# NOINLINE poolTake #-}
poolTake :: BufferPool -> Int -> Int -> IO (ForeignPtr Word8)
poolTake pool bytes minCap = do
  entries <- readIORef pool
  case break (\(_, cap) -> cap >= minCap) entries of
    (before, (ptr, _) : after) -> do
      writeIORef pool (before ++ after)
      pure ptr
    _ -> mallocForeignPtrBytes bytes

{-# NOINLINE poolGive #-}
poolGive :: BufferPool -> ForeignPtr Word8 -> Int -> IO ()
poolGive pool ptr cap = do
  entries <- readIORef pool
  writeIORef pool (take bufferPoolLimit ((ptr, cap) : entries))

{-# NOINLINE growBuffer #-}
growBuffer ::
  Int ->
  IORef (ForeignPtr Word8) ->
  IORef (Ptr Word8) ->
  IORef Int ->
  BufferPool ->
  Int ->
  Int ->
  IO ()
growBuffer count fptrRef ptrRef capRef pool elemBytes needElems = do
  cap <- readIORef capRef
  let required = count + needElems
  when (required > cap) $ do
    oldFPtr <- readIORef fptrRef
    let newCap = max (cap * 2) required
    newFPtr <- poolTake pool (newCap * elemBytes) newCap
    withForeignPtr newFPtr $ \newP ->
      withForeignPtr oldFPtr $ \oldP ->
        copyArray newP oldP (count * elemBytes)
    poolGive pool oldFPtr cap
    writeIORef fptrRef newFPtr
    writeIORef ptrRef (unsafeForeignPtrToPtr newFPtr)
    writeIORef capRef newCap

ensureCapacity :: DrawArena -> Int -> Int -> IO ()
ensureCapacity da needVerts needIndices = do
  vCount <- readIORef (daVertexCount da)
  growBuffer vCount (daVertexFPtr da) (daVertexPtr da) (daVertexCap da) (daVertexPool da) vertexSize needVerts
  iCount <- readIORef (daIndexCount da)
  growBuffer iCount (daIndexFPtr da) (daIndexPtr da) (daIndexCap da) (daIndexPool da) indexSize needIndices

{-# INLINE ensureAndAlloc #-}
ensureAndAlloc :: DrawArena -> Int -> Int -> IO (Ptr Word8, Ptr Word8, Int, Int)
ensureAndAlloc da needV needI = do
  vCount <- readIORef (daVertexCount da)
  iCount <- readIORef (daIndexCount da)
  vCap <- readIORef (daVertexCap da)
  iCap <- readIORef (daIndexCap da)
  unless (vCount + needV <= vCap && iCount + needI <= iCap) $
    ensureCapacity da needV needI
  vp <- readIORef (daVertexPtr da)
  ip <- readIORef (daIndexPtr da)
  pure (vp, ip, vCount, iCount)

{-# NOINLINE growCmdStore #-}
growCmdStore :: DrawArena -> Int -> IO ()
growCmdStore da oldCap = do
  let newCap = oldCap * 2
  arr <- readIORef (daCmdStore da)
  newArr <- resizeMutablePrimArray arr newCap
  writeIORef (daCmdStore da) newArr
  writeIORef (daCmdCapacity da) newCap

-- | Close the pending index run as a command. A run that continues the last
-- command's state and index range extends that command instead. Only reached
-- when the layer, clip or texture changes and from 'finishDraw', so it stays
-- out of the emitters.
{-# NOINLINE flushCmd #-}
flushCmd :: DrawArena -> IO ()
flushCmd da = do
  start <- readIORef (daCmdStartIndex da)
  end <- readIORef (daIndexCount da)
  when (end > start) $ do
    (cx, cy, cw, ch) <- readIORef (daCurrentClip da)
    tex <- readIORef (daCurrentTexture da)
    layer <- readIORef (daCurrentLayer da)
    n <- readIORef (daCmdCount da)
    arr <- readIORef (daCmdStore da)
    let off = fromIntegral start :: Word32
        cnt = fromIntegral (end - start) :: Word32
    extended <-
      if n <= 0
        then pure False
        else do
          prev <- readPrimArray arr (n - 1)
          let same =
                cmdClipX prev == cx
                  && cmdClipY prev == cy
                  && cmdClipW prev == cw
                  && cmdClipH prev == ch
                  && cmdTextureId prev == tex
                  && cmdLayer prev == layer
                  && cmdIndexOffset prev + cmdIndexCount prev == off
          when same $
            writePrimArray arr (n - 1) prev {cmdIndexCount = cmdIndexCount prev + cnt}
          pure same
    unless extended $ do
      cap <- readIORef (daCmdCapacity da)
      when (n >= cap) $ growCmdStore da cap
      arr' <- readIORef (daCmdStore da)
      writePrimArray arr' n (DrawCmd cx cy cw ch tex off cnt layer)
      writeIORef (daCmdCount da) (n + 1)
    writeIORef (daCmdStartIndex da) end

{-# INLINE currentLayer #-}
currentLayer :: DrawArena -> IO Layer
currentLayer = readIORef . daCurrentLayer

beginLayer :: DrawArena -> Layer -> IO ()
beginLayer da layer = do
  cur <- readIORef (daCurrentLayer da)
  when (cur /= layer) $ do
    flushCmd da
    writeIORef (daCurrentLayer da) layer
    readIORef (daIndexCount da) >>= writeIORef (daCmdStartIndex da)

setClip :: DrawArena -> Rect -> IO ()
setClip da (Rect x y w h) = do
  flushCmd da
  writeIORef (daCurrentClip da) (x, y, w, h)

-- | Run @act@ clipped to the intersection with the current clip. Not
-- exception-safe: the frame resets the clip before the next paint anyway.
{-# INLINE withClip #-}
withClip :: DrawArena -> Rect -> IO a -> IO a
withClip da rect act = do
  (ox, oy, ow, oh) <- readIORef (daCurrentClip da)
  let prev = Rect ox oy ow oh
  setClip da (fromMaybe (Rect 0 0 0 0) (rectIntersect prev rect))
  act <* setClip da prev

-- | Bind a texture. The unchanged case is the common one and stays inline; a
-- real switch closes the pending command out of line.
{-# INLINE setTexture #-}
setTexture :: DrawArena -> Int -> IO ()
setTexture da tex = do
  cur <- readIORef (daCurrentTexture da)
  when (cur /= tex) $ switchTexture da tex

{-# NOINLINE switchTexture #-}
switchTexture :: DrawArena -> Int -> IO ()
switchTexture da tex = do
  flushCmd da
  writeIORef (daCurrentTexture da) tex

finishDraw :: DrawArena -> IO DrawData
finishDraw da = do
  flushCmd da
  vFPtr <- readIORef (daVertexFPtr da)
  iFPtr <- readIORef (daIndexFPtr da)
  vCount <- readIORef (daVertexCount da)
  iCount <- readIORef (daIndexCount da)
  count <- readIORef (daCmdCount da)
  arr <- readIORef (daCmdStore da)
  (cmds, slices) <- groupCmdsByLayer arr count
  pure
    DrawData
      { drawVertices = vFPtr
      , drawVertexCount = vCount
      , drawIndices = iFPtr
      , drawIndexCount = iCount
      , drawCommands = cmds
      , drawLayerSlices = slices
      }

-- | Stable counting sort of the recorded commands by layer, plus one slice per
-- layer into the sorted array.
groupCmdsByLayer :: MutablePrimArray RealWorld DrawCmd -> Int -> IO (PrimArray DrawCmd, PrimArray LayerSlice)
groupCmdsByLayer src n = do
  let layers = fromEnum (maxBound :: Layer) + 1
      layerAt i = fromEnum . cmdLayer <$> readPrimArray src i
  counts <- newPrimArray layers
  setPrimArray counts 0 layers (0 :: Int)
  loopIO 0 (n - 1) $ \i -> do
    l <- layerAt i
    readPrimArray counts l >>= writePrimArray counts l . (+ 1)
  cursors <- newPrimArray layers
  slices <- newPrimArray layers
  let offsets !l !off =
        when (l < layers) $ do
          c <- readPrimArray counts l
          writePrimArray cursors l off
          writePrimArray slices l (LayerSlice off c)
          offsets (l + 1) (off + c)
  offsets 0 0
  dest <- newPrimArray n
  loopIO 0 (n - 1) $ \i -> do
    cmd <- readPrimArray src i
    let l = fromEnum (cmdLayer cmd)
    j <- readPrimArray cursors l
    writePrimArray dest j cmd
    writePrimArray cursors l (j + 1)
  (,) <$> unsafeFreezePrimArray dest <*> unsafeFreezePrimArray slices

{-# INLINE unpackColorF #-}
unpackColorF :: Color -> (Float, Float, Float, Float)
unpackColorF (Color w) =
  let !inv255 = 1.0 / 255.0
      !r = fromIntegral ((w `shiftR` 24) .&. 0xFF) * inv255
      !g = fromIntegral ((w `shiftR` 16) .&. 0xFF) * inv255
      !b = fromIntegral ((w `shiftR` 8) .&. 0xFF) * inv255
      !a = fromIntegral (w .&. 0xFF) * inv255
   in (r, g, b, a)

-- Allocate room for a primitive, hand the derived offsets to the body, and
-- commit the vertex/index counts afterwards. INLINE: erased at -O.
{-# INLINE withVerts #-}
withVerts :: DrawArena -> Int -> Int -> (Ptr Word8 -> Ptr Word8 -> Int -> Int -> Word32 -> IO ()) -> IO ()
withVerts da needV needI f = do
  (vp, ip, base, baseIdx) <- ensureAndAlloc da needV needI
  let !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !baseIdxWord = fromIntegral base :: Word32
  f vp ip vOff iOff baseIdxWord
  writeIORef (daVertexCount da) (base + needV)
  writeIORef (daIndexCount da) (baseIdx + needI)

-- Like 'withVerts' but for primitives that index vertices relative to 'base'
-- themselves instead of using one contiguous offset.
{-# INLINE withVertsRaw #-}
withVertsRaw :: DrawArena -> Int -> Int -> (Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()) -> IO ()
withVertsRaw da needV needI f = do
  (vp, ip, base, baseIdx) <- ensureAndAlloc da needV needI
  f vp ip base baseIdx
  writeIORef (daVertexCount da) (base + needV)
  writeIORef (daIndexCount da) (baseIdx + needI)

-- | Reserve room for up to @maxV@ vertices / @maxI@ indices, hand the body a
-- commit action, then record only the counts the body reports. Batches many
-- small quads (text glyphs) into one arena reservation instead of one
-- @withVerts@ closure + capacity check per quad.
{-# INLINE withVertsReserve #-}
withVertsReserve ::
  DrawArena ->
  Int ->
  Int ->
  (Ptr Word8 -> Ptr Word8 -> Int -> Int -> (Int -> Int -> IO ()) -> IO ()) ->
  IO ()
withVertsReserve da maxV maxI f = do
  (vp, ip, base, baseIdx) <- ensureAndAlloc da maxV maxI
  f vp ip base baseIdx $ \nv ni -> do
    writeIORef (daVertexCount da) (base + nv)
    writeIORef (daIndexCount da) (baseIdx + ni)

-- | Strict numeric loop. Replaces @forM_ [lo .. hi]@ on the rounded-geometry
-- hot path, where the intermediate range list was a measurable allocation and
-- prevented the body from fusing into a straight-line loop.
{-# INLINE loopIO #-}
loopIO :: Int -> Int -> (Int -> IO ()) -> IO ()
loopIO !lo !hi f = go lo
  where
    go !i
      | i > hi = pure ()
      | otherwise = f i >> go (i + 1)

{-# INLINE pushQuad #-}
pushQuad :: DrawArena -> Rect -> Float -> Float -> Float -> Float -> Color -> IO ()
pushQuad da (Rect x y w h) u0 v0 u1 v1 col = do
  let !(r, g, b, a) = unpackColorF col
  withVerts da 4 6 $ \vp ip vOff iOff baseIdxWord ->
    pokeQuadSIMD vp vOff ip iOff x y w h u0 v0 u1 v1 r g b a baseIdxWord

{-# INLINE snapRectOrigin #-}
snapRectOrigin :: DrawArena -> Rect -> IO Rect
snapRectOrigin da (Rect x y w h) = do
  s <- readIORef (daSnapScale da)
  pure (Rect (onGrid s x) (onGrid s y) w h)

{-# INLINE pokeQuadIndices #-}
pokeQuadIndices :: Ptr Word8 -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
pokeQuadIndices ip off a b c d = do
  pokeByteOff ip off a
  pokeByteOff ip (off + 4) b
  pokeByteOff ip (off + 8) c
  pokeByteOff ip (off + 12) a
  pokeByteOff ip (off + 16) c
  pokeByteOff ip (off + 20) d

-- | Center of the 4x4 white pixel patch in the 1024x1024 font atlas.
whitePixelU :: Float
whitePixelU = 1.5 / 1024.0

whitePixelV :: Float
whitePixelV = 1.5 / 1024.0
