{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Draw arena lifecycle, command batching and vertex reservation. The shape
-- and text emitters reserve room here and poke vertices straight into the
-- pinned buffers.
module NanoUI.Internal.Draw.Arena
  ( newDrawArena
  , resetDrawArena
  , setDrawSnapScale
  , getDrawSnapScale
  , setDrawSquareGeometry
  , setDrawExternalText
  , beginLayer
  , currentLayer
  , currentClip
  , setClip
  , setClipPieces
  , getClipPieces
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
  ( PrimArray
  , emptyPrimArray
  , indexPrimArray
  , newPrimArray
  , primArrayFromList
  , readPrimArray
  , setPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Word (Word32, Word8)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Draw.Types
import NanoUI.Internal.SIMD (pokeQuadSIMD)
import NanoUI.Internal.Types (Color (..), Rect (..), onGrid, rectIntersect)

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
  daCmdStore <- newIORef =<< UM.unsafeNew cmdInitialCapacity
  daCmdCount <- newIORef 0
  daCmdCapacity <- newIORef cmdInitialCapacity
  daCurrentLayer <- newIORef LayerContent
  daCurrentClip <- newPrimArray 4
  daCurrentTexture <- newIORef glyphAtlasTextureId
  daCmdStartIndex <- newIORef 0
  daSnapScale <- newIORef 0.0
  daSquareGeometry <- newIORef False
  daExternalText <- newIORef False
  daClipPieces <- newIORef emptyPrimArray
  let
    da = DrawArena {..}
  resetDrawArena da
  pure da

resetDrawArena :: DrawArena -> IO ()
resetDrawArena da = do
  writeIORef (daVertexCount da) 0
  writeIORef (daIndexCount da) 0
  writeIORef (daCmdCount da) 0
  writeIORef (daCurrentLayer da) LayerContent
  setClip da (Rect 0 0 1e9 1e9)
  writeIORef (daCurrentTexture da) glyphAtlasTextureId
  writeIORef (daCmdStartIndex da) 0
  writeIORef (daClipPieces da) emptyPrimArray

-- | Cut every command of this frame to each of these disjoint rects, as a
-- copy per rect it meets. A frame whose damage lies in pieces far apart
-- paints under their bounding box, and the pieces keep it from drawing over
-- the pixels between them, which it has not cleared.
setClipPieces :: DrawArena -> [Rect] -> IO ()
setClipPieces da rects =
  writeIORef (daClipPieces da) $
    primArrayFromList (concat [[x, y, x + w, y + h] | Rect x y w h <- rects])

-- | The rects 'setClipPieces' set, as @x0, y0, x1, y1@ runs.
{-# INLINE getClipPieces #-}
getClipPieces :: DrawArena -> IO (PrimArray Float)
getClipPieces da = readIORef (daClipPieces da)

-- | Device pixel scale used to snap primitive origins/endpoints to whole
-- device pixels. A non-positive value disables snapping. The SDL backend keeps
-- this in sync with the window pixel density. Headless contexts and the RGFW backend
-- leave it disabled.
{-# INLINE setDrawSnapScale #-}
setDrawSnapScale :: DrawArena -> Float -> IO ()
setDrawSnapScale da s = writeIORef (daSnapScale da) (if s > 0 then s else 0)

{-# INLINE getDrawSnapScale #-}
getDrawSnapScale :: DrawArena -> IO Float
getDrawSnapScale da = readIORef (daSnapScale da)

-- | Square geometry for hosts that draw flat, axis-aligned fills, such as the
-- RGFW backend. Rounded rects, circles and their strokes lower to
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
  let
    newCap = oldCap * 2
  arr <- readIORef (daCmdStore da)
  newArr <- UM.unsafeGrow arr (newCap - oldCap)
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
    Rect cx cy cw ch <- currentClip da
    tex <- readIORef (daCurrentTexture da)
    layer <- readIORef (daCurrentLayer da)
    n <- readIORef (daCmdCount da)
    arr <- readIORef (daCmdStore da)
    let
      off = fromIntegral start :: Word32
      cnt = fromIntegral (end - start) :: Word32
    extended <-
      if n <= 0
        then pure False
        else do
          prev <- UM.unsafeRead arr (n - 1)
          let
            same =
              cmdClipX prev == cx
                && cmdClipY prev == cy
                && cmdClipW prev == cw
                && cmdClipH prev == ch
                && cmdTextureId prev == tex
                && cmdLayer prev == layer
                && cmdIndexOffset prev + cmdIndexCount prev == off
          when same $
            UM.unsafeWrite arr (n - 1) prev {cmdIndexCount = cmdIndexCount prev + cnt}
          pure same
    unless extended $ do
      cap <- readIORef (daCmdCapacity da)
      when (n >= cap) $ growCmdStore da cap
      arr' <- readIORef (daCmdStore da)
      UM.unsafeWrite arr' n (DrawCmd cx cy cw ch tex off cnt layer)
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
  let clip = daCurrentClip da
  writePrimArray clip 0 x
  writePrimArray clip 1 y
  writePrimArray clip 2 w
  writePrimArray clip 3 h

{-# INLINE currentClip #-}
currentClip :: DrawArena -> IO Rect
currentClip da = do
  let clip = daCurrentClip da
  x <- readPrimArray clip 0
  y <- readPrimArray clip 1
  w <- readPrimArray clip 2
  h <- readPrimArray clip 3
  pure $! Rect x y w h


-- | Run @act@ clipped to the intersection with the current clip. Not
-- exception-safe: the frame resets the clip before the next paint anyway.
{-# INLINE withClip #-}
withClip :: DrawArena -> Rect -> IO a -> IO a
withClip da rect act = do
  prev <- currentClip da
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
  count0 <- readIORef (daCmdCount da)
  arr0 <- readIORef (daCmdStore da)
  pieces <- readIORef (daClipPieces da)
  (arr, count) <-
    if sizeofPrimArray pieces == 0
      then pure (arr0, count0)
      else cutCmdsToPieces pieces arr0 count0
  (cmds, offsets) <- groupCmdsByLayer arr count
  pure
    DrawData
      { drawVertices = vFPtr
      , drawVertexCount = vCount
      , drawIndices = iFPtr
      , drawIndexCount = iCount
      , drawCommands = cmds
      , drawLayerOffsets = offsets
      }

-- | Each command once per piece its clip meets, clipped to that piece, in
-- command order. Pieces are disjoint, so the order among one command's
-- copies does not matter.
cutCmdsToPieces ::
  PrimArray Float -> U.MVector RealWorld DrawCmd -> Int -> IO (U.MVector RealWorld DrawCmd, Int)
cutCmdsToPieces pieces src n = do
  let
    k = sizeofPrimArray pieces `quot` 4
  dest <- UM.unsafeNew (max 1 (n * k))
  let
    go !i !m
      | i >= n = pure m
      | otherwise = do
          cmd <- UM.unsafeRead src i
          let
            cx0 = cmdClipX cmd
            cy0 = cmdClipY cmd
            cx1 = cx0 + cmdClipW cmd
            cy1 = cy0 + cmdClipH cmd
            piece !j !m'
              | j >= k = pure m'
              | otherwise = do
                  let
                    o = j * 4
                    x0 = max cx0 (indexPrimArray pieces o)
                    y0 = max cy0 (indexPrimArray pieces (o + 1))
                    x1 = min cx1 (indexPrimArray pieces (o + 2))
                    y1 = min cy1 (indexPrimArray pieces (o + 3))
                  if x1 <= x0 || y1 <= y0
                    then piece (j + 1) m'
                    else do
                      UM.unsafeWrite dest m' cmd {cmdClipX = x0, cmdClipY = y0, cmdClipW = x1 - x0, cmdClipH = y1 - y0}
                      piece (j + 1) (m' + 1)
          piece 0 m >>= go (i + 1)
  m <- go 0 0
  pure (dest, m)

-- | Stable counting sort by layer, with cumulative offsets into the sorted
-- array. Counts become write cursors after the prefix sum.
groupCmdsByLayer ::
  U.MVector RealWorld DrawCmd -> Int -> IO (U.Vector DrawCmd, PrimArray Int)
groupCmdsByLayer _ 0 = pure (U.empty, emptyLayerOffsets)
groupCmdsByLayer src n = do
  let
    layers = fromEnum (maxBound :: Layer) + 1
    layerAt i = fromEnum . cmdLayer <$> UM.unsafeRead src i
  cursors <- newPrimArray layers
  setPrimArray cursors 0 layers (0 :: Int)
  loopIO 0 (n - 1) $ \i -> do
    l <- layerAt i
    readPrimArray cursors l >>= writePrimArray cursors l . (+ 1)
  offsets <- newPrimArray (layers + 1)
  let
    prefix !l !off = do
      writePrimArray offsets l off
      when (l < layers) $ do
        c <- readPrimArray cursors l
        writePrimArray cursors l off
        prefix (l + 1) (off + c)
  prefix 0 0
  dest <- UM.unsafeNew n
  loopIO 0 (n - 1) $ \i -> do
    cmd <- UM.unsafeRead src i
    let
      l = fromEnum (cmdLayer cmd)
    j <- readPrimArray cursors l
    UM.unsafeWrite dest j cmd
    writePrimArray cursors l (j + 1)
  (,) <$> U.unsafeFreeze dest <*> unsafeFreezePrimArray offsets

-- Empty damage frames share their immutable command index.
emptyLayerOffsets :: PrimArray Int
emptyLayerOffsets = primArrayFromList (replicate (fromEnum (maxBound :: Layer) + 2) 0)

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

-- | Strict numeric loop over inclusive bounds, without allocating a range list.
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
