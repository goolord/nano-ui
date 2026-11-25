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
  , setDrawPixelScale
  , getDrawPixelScale
  , setDrawSquareGeometry
  , setDrawExternalText
  , setDrawClear
  , getDrawClear
  , setDrawRecordOps
  , getDrawRecordOps
  , withRecordOpacity
  , recordOp
  , recordOpOn
  , recordedOps
  , beginLayer
  , currentLayer
  , currentClip
  , setClip
  , setClipPieces
  , getClipPieces
  , withClip
  , setTexture
  , setShader
  , allocShaderCall
  , pushShaderQuad
  , EffectSpanToken (..)
  , beginEffectSpan
  , endEffectSpan
  , currentEffectSpanId
  , finishDraw
  , withVerts
  , withVertsRaw
  , reserveRaw
  , commitRaw
  , withVertsReserve
  , drawnVertexCount
  , pushQuad
  , snapRectOrigin
  , unpackColorF
  , whitePixel
  ) where

import Control.Monad (forM, unless, when)
import Data.Bits (shiftR, (.&.))
import Data.Int (Int32)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray
  ( ByteArray
  , copyByteArray
  , copyMutableByteArray
  , getSizeofMutableByteArray
  , newByteArray
  , sizeofByteArray
  , unsafeFreezeByteArray
  )
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
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Word (Word32, Word8)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr)
import GHC.Exts (RealWorld)
import NanoUI.Internal.Draw.Types
import NanoUI.Internal.Shader (PackedShaderPass (..))
import NanoUI.Internal.SIMD (pokeQuadSIMD)
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..), foldUpTo, forUpTo_, newZeroedPrimArray, onGrid, rectIntersect)
import NanoUI.Internal.Uniforms (alignUp, pokeSysUniforms, sysUniformBytes)

vertexCapacity :: Int
vertexCapacity = 4096

indexCapacity :: Int
indexCapacity = 8192

cmdInitialCapacity :: Int
cmdInitialCapacity = 64

newDrawArena :: IO DrawArena
newDrawArena = do
  vFPtr <- mallocForeignPtrBytes (vertexCapacity * vertexSize)
  iFPtr <- mallocForeignPtrBytes (indexCapacity * indexSize)
  daVertexFPtr <- newIORef vFPtr
  daVertexCap <- newIORef vertexCapacity
  daIndexFPtr <- newIORef iFPtr
  daIndexCap <- newIORef indexCapacity
  daCounts <- newPrimArray countSlots
  daCmdStore <- newIORef =<< UM.unsafeNew cmdInitialCapacity
  daCurrentLayer <- newIORef LayerContent
  daCurrentClip <- newPrimArray 4
  daCurrentTexture <- newIORef glyphAtlasTextureId
  daSnapScale <- newIORef 0.0
  daSquareGeometry <- newIORef False
  daExternalText <- newIORef False
  daClipPieces <- newIORef emptyPrimArray
  daTextSpans <- newIORef ([], [])
  daClear <- newIORef (Color 0)
  daRecordOps <- newIORef False
  daRecordedOps <- newIORef []
  daRecordOpacity <- newIORef 1.0
  daCurrentShader <- newIORef 0
  daNoMergeBefore <- newIORef 0
  daShaderCalls <- newIORef []
  daShaderCallCount <- newIORef 0
  daUniformBuf <- newIORef =<< newByteArray 0
  daUniformLen <- newIORef 0
  daEffectSpans <- newIORef []
  daEffectStack <- newIORef []
  daEffectNextId <- newIORef 1
  daPixelScale <- newIORef 1.0
  let
    da = DrawArena {..}
  resetDrawArena da
  pure da

resetDrawArena :: DrawArena -> IO ()
resetDrawArena da = do
  setPrimArray (daCounts da) 0 countSlots 0
  writeIORef (daCurrentLayer da) LayerContent
  setClip da (Rect 0 0 1e9 1e9)
  writeIORef (daCurrentTexture da) glyphAtlasTextureId
  writeIORef (daCurrentShader da) 0
  writeIORef (daNoMergeBefore da) 0
  writeIORef (daShaderCalls da) []
  writeIORef (daShaderCallCount da) 0
  writeIORef (daUniformLen da) 0
  writeIORef (daEffectSpans da) []
  writeIORef (daEffectStack da) []
  writeIORef (daEffectNextId da) 1
  writeIORef (daClipPieces da) emptyPrimArray
  writeIORef (daClear da) (Color 0)
  -- Only here: a reused frame paints nothing and keeps the last paint's.
  writeIORef (daTextSpans da) ([], [])
  writeIORef (daRecordedOps da) []
  writeIORef (daRecordOpacity da) 1.0

-- 'daCounts' slots.
vertexCountSlot, indexCountSlot, cmdCountSlot, cmdStartSlot, countSlots :: Int
vertexCountSlot = 0
indexCountSlot = 1
cmdCountSlot = 2
cmdStartSlot = 3
countSlots = 4

{-# INLINE getCount #-}
getCount :: DrawArena -> Int -> IO Int
getCount da = readPrimArray (daCounts da)

{-# INLINE setCount #-}
setCount :: DrawArena -> Int -> Int -> IO ()
setCount da = writePrimArray (daCounts da)

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
setDrawSnapScale da s = do
  writeIORef (daSnapScale da) (if s > 0 then s else 0)
  when (s > 0) $ writeIORef (daPixelScale da) s

{-# INLINE getDrawSnapScale #-}
getDrawSnapScale :: DrawArena -> IO Float
getDrawSnapScale da = readIORef (daSnapScale da)

-- | Device pixels per logical layout unit, forwarded in shader system uniforms
-- (@uMeta.x@) without enabling pixel-grid snapping.
{-# INLINE setDrawPixelScale #-}
setDrawPixelScale :: DrawArena -> Float -> IO ()
setDrawPixelScale da s = writeIORef (daPixelScale da) (if s > 0 then s else 1.0)

{-# INLINE getDrawPixelScale #-}
getDrawPixelScale :: DrawArena -> IO Float
getDrawPixelScale da = readIORef (daPixelScale da)

-- | Square geometry for hosts that draw flat, axis-aligned fills, such as the
-- RGFW backend. Rounded rects, circles and their strokes lower to
-- plain rects, and coverage-AA strips lower to solid quads with no
-- transparent fringe vertices. Persists across 'resetDrawArena'.
{-# INLINE setDrawSquareGeometry #-}
setDrawSquareGeometry :: DrawArena -> Bool -> IO ()
setDrawSquareGeometry da = writeIORef (daSquareGeometry da)

-- | The colour the backend clears a full frame to before its commands
-- ('drawClear').
{-# INLINE setDrawClear #-}
setDrawClear :: DrawArena -> Color -> IO ()
setDrawClear da = writeIORef (daClear da)

-- | This paint's clear colour, as 'setDrawClear' left it.
{-# INLINE getDrawClear #-}
getDrawClear :: DrawArena -> IO Color
getDrawClear da = readIORef (daClear da)

-- | Record every solid shape the emitters draw ('recordedOps'), for
-- exporters that redraw a frame as vectors. Off, each emitter pays one flag
-- read. Persists across 'resetDrawArena'.
{-# INLINE setDrawRecordOps #-}
setDrawRecordOps :: DrawArena -> Bool -> IO ()
setDrawRecordOps da = writeIORef (daRecordOps da)

-- | Whether 'setDrawRecordOps' is on.
{-# INLINE getDrawRecordOps #-}
getDrawRecordOps :: DrawArena -> IO Bool
getDrawRecordOps da = readIORef (daRecordOps da)

-- | Record @op@ under the current layer and clip while recording is on.
{-# INLINE recordOp #-}
recordOp :: DrawArena -> DrawOp -> IO ()
recordOp da op = do
  on <- readIORef (daRecordOps da)
  when on (recordOpOn da op)

-- | Multiply 'daRecordOpacity' by @a@ around @act@ when recording vector ops
-- for SVG export.
{-# NOINLINE withRecordOpacity #-}
withRecordOpacity :: DrawArena -> Float -> IO a -> IO a
withRecordOpacity da a act = do
  prev <- readIORef (daRecordOpacity da)
  writeIORef (daRecordOpacity da) $! max 0 (min 1 (prev * a))
  r <- act
  writeIORef (daRecordOpacity da) prev
  pure r

{-# NOINLINE recordOpOn #-}
recordOpOn :: DrawArena -> DrawOp -> IO ()
recordOpOn da op = do
  layer <- currentLayer da
  clip <- currentClip da
  alpha <- readIORef (daRecordOpacity da)
  modifyIORef' (daRecordedOps da) (RecordedOp layer clip alpha op :)

-- | The shapes the last paint recorded, in paint order.
recordedOps :: DrawArena -> IO [RecordedOp]
recordedOps da = reverse <$> readIORef (daRecordedOps da)

-- | External text for hosts that rasterize text themselves from the collected
-- text spans. Text emitters push no quads, so fonts without a glyph atlas do
-- not leave per-character advance boxes in the buffer; drawing text ops are
-- recorded as spans ('daTextSpans') instead. Persists across
-- 'resetDrawArena'.
{-# INLINE setDrawExternalText #-}
setDrawExternalText :: DrawArena -> Bool -> IO ()
setDrawExternalText da = writeIORef (daExternalText da)

-- | Make room for @needV@ vertices after the first @vCount@ and @needI@
-- indices after the first @iCount@, copying each full buffer into one of at
-- least twice its capacity.
{-# NOINLINE growBuffers #-}
growBuffers :: DrawArena -> Int -> Int -> Int -> Int -> IO ()
growBuffers da vCount iCount needV needI = do
  grow (daVertexFPtr da) (daVertexCap da) vertexSize vCount needV
  grow (daIndexFPtr da) (daIndexCap da) indexSize iCount needI
  where
    grow fptrRef capRef elemBytes count needElems = do
      cap <- readIORef capRef
      let required = count + needElems
      when (required > cap) $ do
        oldFPtr <- readIORef fptrRef
        let newCap = max (cap * 2) required
        newFPtr <- mallocForeignPtrBytes (newCap * elemBytes)
        withForeignPtr newFPtr $ \newP ->
          withForeignPtr oldFPtr $ \oldP ->
            copyArray newP oldP (count * elemBytes)
        writeIORef fptrRef newFPtr
        writeIORef capRef newCap

-- | Reserve room for @needV@ vertices and @needI@ indices: the vertex and
-- index buffers and the first free vertex and index. Commit what was
-- written with 'commitRaw'. A primitive with a long body calls these
-- directly rather than passing it to 'withVertsRaw' as a closure, which it
-- would allocate on every call.
{-# INLINE reserveRaw #-}
reserveRaw :: DrawArena -> Int -> Int -> IO (Ptr Word8, Ptr Word8, Int, Int)
reserveRaw da needV needI = do
  vCount <- getCount da vertexCountSlot
  iCount <- getCount da indexCountSlot
  vCap <- readIORef (daVertexCap da)
  iCap <- readIORef (daIndexCap da)
  unless (vCount + needV <= vCap && iCount + needI <= iCap) $
    growBuffers da vCount iCount needV needI
  -- Strict, so a caller that hands the pointers on allocates no thunks.
  !vp <- unsafeForeignPtrToPtr <$> readIORef (daVertexFPtr da)
  !ip <- unsafeForeignPtrToPtr <$> readIORef (daIndexFPtr da)
  pure (vp, ip, vCount, iCount)

-- | Close the pending index run as a command. A run that continues the last
-- command's state and index range extends that command instead, and a run
-- clipped to nothing gets none: a host would still draw it into the pixel
-- the empty clip rounds to. Only reached when the layer, clip or texture
-- changes and from 'finishDraw', so it stays out of the emitters.
{-# NOINLINE flushCmd #-}
flushCmd :: DrawArena -> IO ()
flushCmd da = do
  start <- getCount da cmdStartSlot
  end <- getCount da indexCountSlot
  when (end > start) $ do
    Rect cx cy cw ch <- currentClip da
    when (cw > 0 && ch > 0) $ do
      tex <- readIORef (daCurrentTexture da)
      layer <- readIORef (daCurrentLayer da)
      sh <- readIORef (daCurrentShader da)
      noMerge <- readIORef (daNoMergeBefore da)
      n <- getCount da cmdCountSlot
      arr <- readIORef (daCmdStore da)
      let
        off = fromIntegral start :: Word32
        cnt = fromIntegral (end - start) :: Word32
      extended <-
        if n <= noMerge
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
                  && cmdShader prev == sh
                  && cmdIndexOffset prev + cmdIndexCount prev == off
            when same $
              UM.unsafeWrite arr (n - 1) prev {cmdIndexCount = cmdIndexCount prev + cnt}
            pure same
      unless extended $ do
        arr' <-
          if n < UM.length arr
            then pure arr
            else do
              grown <- UM.unsafeGrow arr (UM.length arr)
              grown <$ writeIORef (daCmdStore da) grown
        UM.unsafeWrite arr' n (DrawCmd cx cy cw ch tex off cnt layer sh)
        setCount da cmdCountSlot (n + 1)
    setCount da cmdStartSlot end

{-# INLINE currentLayer #-}
currentLayer :: DrawArena -> IO Layer
currentLayer = readIORef . daCurrentLayer

beginLayer :: DrawArena -> Layer -> IO ()
beginLayer da layer = do
  cur <- readIORef (daCurrentLayer da)
  when (cur /= layer) $ do
    flushCmd da
    writeIORef (daCurrentLayer da) layer

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

-- | Bind a 1-based shader call index (@0@ for the standard pipeline).
{-# INLINE setShader #-}
setShader :: DrawArena -> Int32 -> IO ()
setShader da sh = do
  cur <- readIORef (daCurrentShader da)
  when (cur /= sh) $ switchShader da sh

{-# NOINLINE switchShader #-}
switchShader :: DrawArena -> Int32 -> IO ()
switchShader da sh = do
  flushCmd da
  writeIORef (daCurrentShader da) sh

-- | Append one shader call's 48-byte system header and user std140 uniform
-- bytes into the draw arena, returning its 1-based 'ShaderCall' index.
{-# NOINLINE allocShaderCall #-}
allocShaderCall ::
  DrawArena ->
  PackedShaderPass ->
  Rect ->
  Rect ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO Int32
allocShaderCall da pass nodeRect uvRect scale radius tw th = do
  let userBytes = pspUniforms pass
      !userLen = sizeofByteArray userBytes
      !totalLen = alignUp 16 (sysUniformBytes + userLen)
  off <- readIORef (daUniformLen da)
  buf0 <- readIORef (daUniformBuf da)
  cap <- getSizeofMutableByteArray buf0
  buf <-
    if off + totalLen <= cap
      then pure buf0
      else do
        let !newCap = max 256 (max (cap * 2) (off + totalLen))
        grown <- newByteArray newCap
        when (off > 0) $ copyMutableByteArray grown 0 buf0 0 off
        grown <$ writeIORef (daUniformBuf da) grown
  pokeSysUniforms nodeRect uvRect scale radius tw th buf off
  when (userLen > 0) $ copyByteArray buf (off + sysUniformBytes) userBytes 0 userLen
  writeIORef (daUniformLen da) (off + totalLen)
  let !call =
        ShaderCall
          { scHash = pspHash pass
          , scProgram = pspProgram pass
          , scUniformOffset = off
          , scUniformLength = totalLen
          }
  idx <- (+ 1) <$> readIORef (daShaderCallCount da)
  writeIORef (daShaderCallCount da) idx
  modifyIORef' (daShaderCalls da) (call :)
  pure (fromIntegral idx)

-- | Emit a Tier 1 leaf surface shader quad with normalized local UVs
-- @(0, 0) .. (1, 1)@ and its packed system + user uniform block.
{-# NOINLINE pushShaderQuad #-}
pushShaderQuad :: DrawArena -> PackedShaderPass -> Rect -> Float -> Color -> IO ()
pushShaderQuad da pass rect@(Rect _ _ w h) radius tint = do
  scale <- readIORef (daPixelScale da)
  callIdx <- allocShaderCall da pass rect (Rect 0 0 1 1) scale radius w h
  setTexture da glyphAtlasTextureId
  setShader da callIdx
  pushQuad da rect 0 0 1 1 tint
  setShader da 0

-- | Token returned by 'beginEffectSpan' and passed to 'endEffectSpan'.
data EffectSpanToken = EffectSpanToken
  { estId :: {-# UNPACK #-} !Int
  , estParent :: {-# UNPACK #-} !Int
  , estLayer :: !Layer
  , estRawStart :: {-# UNPACK #-} !Int
  , estBounds :: !Rect
  , estClip :: !Rect
  , estMargin :: !V2
  }

-- | Current enclosing effect span ID, or @0@ when painting to the root target.
{-# INLINE currentEffectSpanId #-}
currentEffectSpanId :: DrawArena -> IO Int
currentEffectSpanId da = do
  stk <- readIORef (daEffectStack da)
  pure $ case stk of
    (i : _) -> i
    [] -> 0

-- | Open a Tier 2 offscreen effect span bracketing subsequent draw commands
-- in the current layer until the matching 'endEffectSpan'.
{-# NOINLINE beginEffectSpan #-}
beginEffectSpan :: DrawArena -> Rect -> V2 -> IO EffectSpanToken
beginEffectSpan da bounds margin = do
  flushCmd da
  rawStart <- getCount da cmdCountSlot
  writeIORef (daNoMergeBefore da) rawStart
  layer <- readIORef (daCurrentLayer da)
  clip <- currentClip da
  parent <- currentEffectSpanId da
  esid <- readIORef (daEffectNextId da)
  writeIORef (daEffectNextId da) (esid + 1)
  modifyIORef' (daEffectStack da) (esid :)
  pure (EffectSpanToken esid parent layer rawStart bounds clip margin)

-- | Close an open effect span, allocating one 'ShaderCall' per pass in
-- @passes@ and recording the span for 'finishDraw'.
{-# NOINLINE endEffectSpan #-}
endEffectSpan :: DrawArena -> EffectSpanToken -> [PackedShaderPass] -> Maybe Float -> IO ()
endEffectSpan da tok passes svgOpacity = do
  flushCmd da
  rawEnd <- getCount da cmdCountSlot
  writeIORef (daNoMergeBefore da) rawEnd
  modifyIORef' (daEffectStack da) (\case (_ : rest) -> rest; [] -> [])
  scale <- readIORef (daPixelScale da)
  let bounds@(Rect _ _ bw bh) = estBounds tok
      V2 mx my = estMargin tok
      tw = bw + 2 * mx
      th = bh + 2 * my
  calls <- forM passes $ \pass -> do
    callIdx <- allocShaderCall da pass bounds (Rect 0 0 1 1) scale 0 tw th
    -- Read back the ShaderCall just pushed at head of daShaderCalls.
    headCall <- readIORef (daShaderCalls da)
    pure $ case headCall of
      (c : _) -> c
      [] -> ShaderCall (pspHash pass) (pspProgram pass) 0 (fromIntegral callIdx)
  let !pes =
        PendingEffectSpan
          { pesId = estId tok
          , pesParent = estParent tok
          , pesLayer = estLayer tok
          , pesRawStart = estRawStart tok
          , pesRawEnd = rawEnd
          , pesBounds = bounds
          , pesClip = estClip tok
          , pesMargin = estMargin tok
          , pesPasses = smallArrayFromList calls
          , pesSvgOpacity = svgOpacity
          }
  modifyIORef' (daEffectSpans da) (pes :)

finishDraw :: DrawArena -> IO DrawData
finishDraw da = do
  flushCmd da
  vFPtr <- readIORef (daVertexFPtr da)
  iFPtr <- readIORef (daIndexFPtr da)
  vCount <- getCount da vertexCountSlot
  iCount <- getCount da indexCountSlot
  count0 <- getCount da cmdCountSlot
  arr0 <- readIORef (daCmdStore da)
  pieces <- readIORef (daClipPieces da)
  pendingEffects <- readIORef (daEffectSpans da)
  (cmds, offsets, effectSpans) <-
    if null pendingEffects
      then do
        (arr, count) <-
          if sizeofPrimArray pieces == 0
            then pure (arr0, count0)
            else cutCmdsToPieces pieces arr0 count0
        (cs, offs) <- groupCmdsByLayer arr count
        pure (cs, offs, emptyEffectSpans)
      else finishWithEffects pieces arr0 count0 (reverse pendingEffects)
  callCount <- readIORef (daShaderCallCount da)
  (shaderCalls, uniformBytes) <-
    if callCount == 0
      then pure (emptyShaderCalls, emptyUniformBytes)
      else do
        calls <- reverse <$> readIORef (daShaderCalls da)
        uLen <- readIORef (daUniformLen da)
        uBytes <- freezeUniformPrefix da uLen
        pure (smallArrayFromList calls, uBytes)
  clear <- readIORef (daClear da)
  pure
    DrawData
      { drawVertices = vFPtr
      , drawVertexCount = vCount
      , drawIndices = iFPtr
      , drawIndexCount = iCount
      , drawCommands = cmds
      , drawLayerOffsets = offsets
      , drawClear = clear
      , drawShaderCalls = shaderCalls
      , drawUniformBytes = uniformBytes
      , drawEffectSpans = effectSpans
      }

{-# NOINLINE freezeUniformPrefix #-}
freezeUniformPrefix :: DrawArena -> Int -> IO ByteArray
freezeUniformPrefix da uLen
  | uLen <= 0 = pure emptyUniformBytes
  | otherwise = do
      src <- readIORef (daUniformBuf da)
      dst <- newByteArray uLen
      copyMutableByteArray dst 0 src 0 uLen
      unsafeFreezeByteArray dst

-- | Cut commands (when damage pieces are active), group by layer, and map
-- each 'PendingEffectSpan''s raw command boundaries into final 'drawCommands'
-- indices. Only reached on frames that use Tier 2 subtree effects.
{-# NOINLINE finishWithEffects #-}
finishWithEffects ::
  PrimArray Float ->
  U.MVector RealWorld DrawCmd ->
  Int ->
  [PendingEffectSpan] ->
  IO (U.Vector DrawCmd, PrimArray Int, SmallArray EffectSpan)
finishWithEffects pieces arr0 count0 pending = do
  let !layers = fromEnum (maxBound :: Layer) + 1
  before <- newZeroedPrimArray ((count0 + 1) * layers)
  (arr, count) <-
    if sizeofPrimArray pieces == 0
      then do
        forUpTo_ count0 $ \i -> do
          forUpTo_ layers $ \l -> do
            v <- readPrimArray before (i * layers + l)
            writePrimArray before ((i + 1) * layers + l) v
          cmd <- UM.unsafeRead arr0 i
          let !l = fromEnum (cmdLayer cmd)
          v <- readPrimArray before ((i + 1) * layers + l)
          writePrimArray before ((i + 1) * layers + l) (v + 1)
        pure (arr0, count0)
      else do
        let !k = sizeofPrimArray pieces `quot` 4
        dest <- UM.unsafeNew (max 1 (count0 * k))
        let step m i = do
              forUpTo_ layers $ \l -> do
                v <- readPrimArray before (i * layers + l)
                writePrimArray before ((i + 1) * layers + l) v
              cmd <- UM.unsafeRead arr0 i
              let !cx0 = cmdClipX cmd
                  !cy0 = cmdClipY cmd
                  !cx1 = cx0 + cmdClipW cmd
                  !cy1 = cy0 + cmdClipH cmd
                  piece m' j = do
                    let !o = j * 4
                        !x0 = max cx0 (indexPrimArray pieces o)
                        !y0 = max cy0 (indexPrimArray pieces (o + 1))
                        !x1 = min cx1 (indexPrimArray pieces (o + 2))
                        !y1 = min cy1 (indexPrimArray pieces (o + 3))
                    if x1 <= x0 || y1 <= y0
                      then pure m'
                      else do
                        UM.unsafeWrite dest m' cmd {cmdClipX = x0, cmdClipY = y0, cmdClipW = x1 - x0, cmdClipH = y1 - y0}
                        pure (m' + 1)
              mNext <- foldUpTo k piece m
              let !emitted = mNext - m
                  !l = fromEnum (cmdLayer cmd)
              when (emitted > 0) $ do
                v <- readPrimArray before ((i + 1) * layers + l)
                writePrimArray before ((i + 1) * layers + l) (v + emitted)
              pure mNext
        mFinal <- foldUpTo count0 step 0
        pure (dest, mFinal)
  (cmds, offsets) <- groupCmdsByLayer arr count
  spans <- forM pending $ \pes -> do
    let !l = fromEnum (pesLayer pes)
        !layerOff = indexPrimArray offsets l
        !rawS = min count0 (max 0 (pesRawStart pes))
        !rawE = min count0 (max rawS (pesRawEnd pes))
    sOff <- readPrimArray before (rawS * layers + l)
    eOff <- readPrimArray before (rawE * layers + l)
    pure
      EffectSpan
        { esId = pesId pes
        , esParent = pesParent pes
        , esLayer = pesLayer pes
        , esCmdStart = layerOff + sOff
        , esCmdEnd = layerOff + eOff
        , esBounds = pesBounds pes
        , esClip = pesClip pes
        , esMargin = pesMargin pes
        , esPasses = pesPasses pes
        , esSvgOpacity = pesSvgOpacity pes
        }
  pure (cmds, offsets, smallArrayFromList spans)

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
    cutCmd m i = do
      cmd <- UM.unsafeRead src i
      let
        cx0 = cmdClipX cmd
        cy0 = cmdClipY cmd
        cx1 = cx0 + cmdClipW cmd
        cy1 = cy0 + cmdClipH cmd
        piece m' j = do
          let
            o = j * 4
            x0 = max cx0 (indexPrimArray pieces o)
            y0 = max cy0 (indexPrimArray pieces (o + 1))
            x1 = min cx1 (indexPrimArray pieces (o + 2))
            y1 = min cy1 (indexPrimArray pieces (o + 3))
          if x1 <= x0 || y1 <= y0
            then pure m'
            else do
              UM.unsafeWrite dest m' cmd {cmdClipX = x0, cmdClipY = y0, cmdClipW = x1 - x0, cmdClipH = y1 - y0}
              pure (m' + 1)
      foldUpTo k piece m
  m <- foldUpTo n cutCmd 0
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
  cursors <- newZeroedPrimArray layers
  forUpTo_ n $ \i -> do
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
  forUpTo_ n $ \i -> do
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
withVerts da needV needI f =
  withVertsRaw da needV needI $ \vp ip base baseIdx ->
    f vp ip (base * vertexSize) (baseIdx * indexSize) (fromIntegral base)

-- Like 'withVerts' but for primitives that index vertices relative to 'base'
-- themselves instead of using one contiguous offset.
{-# INLINE withVertsRaw #-}
withVertsRaw :: DrawArena -> Int -> Int -> (Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()) -> IO ()
withVertsRaw da needV needI f =
  withVertsReserve da needV needI $ \vp ip base baseIdx commit -> f vp ip base baseIdx >> commit needV needI

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
  (vp, ip, base, baseIdx) <- reserveRaw da maxV maxI
  f vp ip base baseIdx (commitRaw da base baseIdx)

-- | Record @nv@ vertices and @ni@ indices written from a 'reserveRaw' at
-- @base@ and @baseIdx@.
{-# INLINE commitRaw #-}
commitRaw :: DrawArena -> Int -> Int -> Int -> Int -> IO ()
commitRaw da base baseIdx nv ni = do
  setCount da vertexCountSlot (base + nv)
  setCount da indexCountSlot (baseIdx + ni)

-- | Vertices recorded so far this frame, in every layer: 0 before the
-- frame's first geometry.
{-# INLINE drawnVertexCount #-}
drawnVertexCount :: DrawArena -> IO Int
drawnVertexCount da = getCount da vertexCountSlot

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

-- | The u and v at which solid shapes sample glyph atlas page 0
-- ('glyphAtlasTextureId'): three texels in on the SDL host's 2048-texel
-- atlas, inside the 4x4 opaque white patch at its corner.
whitePixel :: Float
whitePixel = 1.5 / 1024.0
