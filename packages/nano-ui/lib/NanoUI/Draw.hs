{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnliftedFFITypes #-}

module NanoUI.Draw
  ( Layer (..)
  , DrawCmd (..)
  , LayerSlice (..)
  , DrawData (..)
  , DrawArena (..)
  , Vertex (..)
  , newDrawArena
  , resetDrawArena
  , beginLayer
  , setClip
  , pushRect
  , pushQuadGradient
  , pushBackdropDim
  , backdropDimTextureId
  , glyphAtlasTextureId
  , pushImage
  , pushRoundedRect
  , pushRoundedStroke
  , pushText
  , pushLine
  , pushStrokeAA
  , pushStroke
  , pushFilledTriangle
  , DrawOp (..)
  , DrawingBuild
  , emitDrawOps
  , drawTextBox
  , shiftDrawOp
  , finishDraw
  , drawCmdCount
  , drawCmdNull
  , drawCmdAt
  , foldDrawCmds
  , drawCmdFilter
  , drawCmdForLayer
  , forDrawCmdsInLayer_
  , drawCmdElemsForLayer
  , drawCmdPartitionByLayer
  , drawCmdElems
  , vertexSize
  , indexSize
  , withClip
  , getCurrentClip
  , currentLayer
  ) where

import Control.Monad (forM_, unless, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , copyMutablePrimArray
  , indexPrimArray
  , newPrimArray
  , readPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts
  ( Addr#
  , Float (F#)
  , Int (I#)
  , Int#
  , MutableByteArray#
  , RealWorld
  , State#
  , (*#)
  , (+#)
  , (-#)
  , (==#)
  , indexFloatOffAddr#
  , indexIntOffAddr#
  , indexWord8Array#
  , indexWord8ArrayAsFloat#
  , indexWord8ArrayAsInt#
  , indexWord8ArrayAsWord32#
  , indexWord8OffAddr#
  , indexWord32OffAddr#
  , isTrue#
  , plusAddr#
  , readFloatOffAddr#
  , readIntOffAddr#
  , readWord8Array#
  , readWord8ArrayAsFloat#
  , readWord8ArrayAsInt#
  , readWord8ArrayAsWord32#
  , readWord8OffAddr#
  , readWord32OffAddr#
  , writeFloatOffAddr#
  , writeIntOffAddr#
  , writeWord8Array#
  , writeWord8ArrayAsFloat#
  , writeWord8ArrayAsInt#
  , writeWord8ArrayAsWord32#
  , writeWord8OffAddr#
  , writeWord32OffAddr#
  )
import GHC.Word (Word8 (W8#), Word32 (W32#))
import NanoUI.Font (FontMetrics (..), GlyphQuad (..), lineWidth)
import NanoUI.Types (Color (..), Rect (..), rectIntersect)
import qualified Data.Text as T
import NanoUI.SIMD
  ( concentricOffsetsSIMD
  , pokeQuadGradientSIMD
  , pokeQuadSIMD
  , pokeVertexSIMD
  , strokeStripNormalsSIMD
  )

data Layer = LayerBackground | LayerContent | LayerOverlay | LayerChrome
  deriving (Eq, Show, Enum, Bounded)

data Vertex = Vertex
  { vtxX :: {-# UNPACK #-} !Float
  , vtxY :: {-# UNPACK #-} !Float
  , vtxR :: {-# UNPACK #-} !Float
  , vtxG :: {-# UNPACK #-} !Float
  , vtxB :: {-# UNPACK #-} !Float
  , vtxA :: {-# UNPACK #-} !Float
  , vtxU :: {-# UNPACK #-} !Float
  , vtxV :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- Immediate vector ops, in widget pixel space. diagrams (and other plotters)
-- flatten into this list; paint emits them after layout.
data DrawOp
  = FillRect !Rect !Color
  | FillTriangle
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  | Stroke
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  | DrawText !Float !Float !Float !Float !T.Text !Color
  -- ^ Pen at (x, y) is the alignment point. ax 0..1 is left..right. ay 0..1 is
  -- bottom..top. ay < 0 means baseline (x is left, y is the baseline). Glyph size
  -- is the host font (`drawTextBox`).
  deriving (Eq, Show)

-- | Pixel box for a 'DrawText' using host advances. diagrams text has no
-- envelope, so plot sizing uses this instead of `fontSizeL`.
drawTextBox :: FontMetrics -> Float -> Float -> Float -> Float -> T.Text -> Rect
drawTextBox fm x y ax ay t =
  let tw = lineWidth fm t
      th = fmLineHeight fm
      px = x - tw * max 0 ax
      py =
        if ay < 0
          then y - fmAscent fm
          else y - th * (1 - ay)
   in Rect px py tw th

-- | Translate every vertex in a 'DrawOp'. Paint reuses ops when only (x, y) moved.
shiftDrawOp :: Float -> Float -> DrawOp -> DrawOp
shiftDrawOp dx dy op =
  case op of
    FillRect (Rect x y w h) c -> FillRect (Rect (x + dx) (y + dy) w h) c
    FillTriangle x0 y0 x1 y1 x2 y2 c ->
      FillTriangle (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) (x2 + dx) (y2 + dy) c
    Stroke x0 y0 x1 y1 t c -> Stroke (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) t c
    DrawText x y ax ay t c -> DrawText (x + dx) (y + dy) ax ay t c

type DrawingBuild = Rect -> Vector DrawOp

data DrawCmd = DrawCmd
  { cmdClipX :: {-# UNPACK #-} !Float
  , cmdClipY :: {-# UNPACK #-} !Float
  , cmdClipW :: {-# UNPACK #-} !Float
  , cmdClipH :: {-# UNPACK #-} !Float
  , cmdTextureId :: {-# UNPACK #-} !Int
  , cmdIndexOffset :: {-# UNPACK #-} !Word32
  , cmdIndexCount :: {-# UNPACK #-} !Word32
  , cmdLayer :: !Layer
  }
  deriving (Eq, Show)

data LayerSlice = LayerSlice
  { sliceOffset :: {-# UNPACK #-} !Int
  , sliceCount :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- Two packed Ints, 16 bytes, 8-byte aligned.
instance Prim LayerSlice where
  sizeOfType# _ = 16#
  alignmentOfType# _ = 8#
  indexByteArray# arr# i# =
    let o# = i# *# 16#
     in LayerSlice
          (I# (indexWord8ArrayAsInt# arr# o#))
          (I# (indexWord8ArrayAsInt# arr# (o# +# 8#)))
  readByteArray# arr# i# s0 =
    let o# = i# *# 16#
     in case readWord8ArrayAsInt# arr# o# s0 of
          (# s1, off# #) ->
            case readWord8ArrayAsInt# arr# (o# +# 8#) s1 of
              (# s2, cnt# #) -> (# s2, LayerSlice (I# off#) (I# cnt#) #)
  writeByteArray# arr# i# (LayerSlice (I# off#) (I# cnt#)) s0 =
    let o# = i# *# 16#
     in writeWord8ArrayAsInt# arr# (o# +# 8#) cnt# (writeWord8ArrayAsInt# arr# o# off# s0)
  setByteArray# = primLoopSetByteArray
  indexOffAddr# addr# i# =
    let a# = addr# `plusAddr#` (i# *# 16#)
     in LayerSlice (I# (indexIntOffAddr# a# 0#)) (I# (indexIntOffAddr# (a# `plusAddr#` 8#) 0#))
  readOffAddr# addr# i# s0 =
    let a# = addr# `plusAddr#` (i# *# 16#)
     in case readIntOffAddr# a# 0# s0 of
          (# s1, off# #) ->
            case readIntOffAddr# (a# `plusAddr#` 8#) 0# s1 of
              (# s2, cnt# #) -> (# s2, LayerSlice (I# off#) (I# cnt#) #)
  writeOffAddr# addr# i# (LayerSlice (I# off#) (I# cnt#)) s0 =
    let a# = addr# `plusAddr#` (i# *# 16#)
     in writeIntOffAddr# (a# `plusAddr#` 8#) 0# cnt# (writeIntOffAddr# a# 0# off# s0)
  setOffAddr# = primLoopSetOffAddr

data DrawData = DrawData
  { drawVertices :: ForeignPtr Word8
  , drawVertexCount :: {-# UNPACK #-} !Int
  , drawIndices :: ForeignPtr Word8
  , drawIndexCount :: {-# UNPACK #-} !Int
  , drawCommands :: !(PrimArray DrawCmd)
  , drawLayerSlices :: !(PrimArray LayerSlice)
  }
  deriving (Eq, Show)

type BufferPool = IORef [(ForeignPtr Word8, Int)]

data DrawArena = DrawArena
  { daVertexFPtr :: !(IORef (ForeignPtr Word8))
  , daVertexPtr :: !(IORef (Ptr Word8))
  , daVertexCap :: !(IORef Int)
  , daVertexCount :: !(IORef Int)
  , daVertexPool :: !BufferPool
  , daIndexFPtr :: !(IORef (ForeignPtr Word8))
  , daIndexPtr :: !(IORef (Ptr Word8))
  , daIndexCap :: !(IORef Int)
  , daIndexCount :: !(IORef Int)
  , daIndexPool :: !BufferPool
  , daCmdStore :: !(IORef (MutablePrimArray RealWorld DrawCmd))
  , daCmdCount :: !(IORef Int)
  , daCmdCapacity :: !(IORef Int)
  , daCurrentLayer :: !(IORef Layer)
  , daCurrentClip :: !(IORef (Float, Float, Float, Float))
  , daCurrentTexture :: !(IORef Int)
  , daCmdStartIndex :: !(IORef Int)
  }

{-# INLINE layerToWord8 #-}
layerToWord8 :: Layer -> Word8
layerToWord8 ly = fromIntegral (fromEnum ly)

{-# INLINE layerFromWord8 #-}
layerFromWord8 :: Word8 -> Layer
layerFromWord8 w = toEnum (fromIntegral w)

-- Clip floats (16) + Int tex (8) + two Word32 (8) + Layer Word8 + pad = 40.
instance Prim DrawCmd where
  sizeOfType# _ = 40#
  alignmentOfType# _ = 8#
  indexByteArray# arr# i# =
    let o# = i# *# 40#
     in DrawCmd
          (F# (indexWord8ArrayAsFloat# arr# o#))
          (F# (indexWord8ArrayAsFloat# arr# (o# +# 4#)))
          (F# (indexWord8ArrayAsFloat# arr# (o# +# 8#)))
          (F# (indexWord8ArrayAsFloat# arr# (o# +# 12#)))
          (I# (indexWord8ArrayAsInt# arr# (o# +# 16#)))
          (W32# (indexWord8ArrayAsWord32# arr# (o# +# 24#)))
          (W32# (indexWord8ArrayAsWord32# arr# (o# +# 28#)))
          (layerFromWord8 (W8# (indexWord8Array# arr# (o# +# 32#))))
  readByteArray# arr# i# s0 =
    let o# = i# *# 40#
     in case readWord8ArrayAsFloat# arr# o# s0 of
          (# s1, x# #) ->
            case readWord8ArrayAsFloat# arr# (o# +# 4#) s1 of
              (# s2, y# #) ->
                case readWord8ArrayAsFloat# arr# (o# +# 8#) s2 of
                  (# s3, w# #) ->
                    case readWord8ArrayAsFloat# arr# (o# +# 12#) s3 of
                      (# s4, h# #) ->
                        case readWord8ArrayAsInt# arr# (o# +# 16#) s4 of
                          (# s5, tex# #) ->
                            case readWord8ArrayAsWord32# arr# (o# +# 24#) s5 of
                              (# s6, off# #) ->
                                case readWord8ArrayAsWord32# arr# (o# +# 28#) s6 of
                                  (# s7, cnt# #) ->
                                    case readWord8Array# arr# (o# +# 32#) s7 of
                                      (# s8, ly# #) ->
                                        (# s8
                                         , DrawCmd
                                            (F# x#)
                                            (F# y#)
                                            (F# w#)
                                            (F# h#)
                                            (I# tex#)
                                            (W32# off#)
                                            (W32# cnt#)
                                            (layerFromWord8 (W8# ly#))
                                         #)
  writeByteArray# arr# i# cmd s0 =
    case cmd of
      DrawCmd (F# x#) (F# y#) (F# w#) (F# h#) (I# tex#) (W32# off#) (W32# cnt#) ly ->
        let o# = i# *# 40#
            !(W8# ly#) = layerToWord8 ly
         in writeWord8Array# arr# (o# +# 32#) ly# $
              writeWord8ArrayAsWord32# arr# (o# +# 28#) cnt# $
                writeWord8ArrayAsWord32# arr# (o# +# 24#) off# $
                  writeWord8ArrayAsInt# arr# (o# +# 16#) tex# $
                    writeWord8ArrayAsFloat# arr# (o# +# 12#) h# $
                      writeWord8ArrayAsFloat# arr# (o# +# 8#) w# $
                        writeWord8ArrayAsFloat# arr# (o# +# 4#) y# $
                          writeWord8ArrayAsFloat# arr# o# x# s0
  setByteArray# = primLoopSetByteArray
  indexOffAddr# addr# i# =
    let a# = addr# `plusAddr#` (i# *# 40#)
     in DrawCmd
          (F# (indexFloatOffAddr# a# 0#))
          (F# (indexFloatOffAddr# (a# `plusAddr#` 4#) 0#))
          (F# (indexFloatOffAddr# (a# `plusAddr#` 8#) 0#))
          (F# (indexFloatOffAddr# (a# `plusAddr#` 12#) 0#))
          (I# (indexIntOffAddr# (a# `plusAddr#` 16#) 0#))
          (W32# (indexWord32OffAddr# (a# `plusAddr#` 24#) 0#))
          (W32# (indexWord32OffAddr# (a# `plusAddr#` 28#) 0#))
          (layerFromWord8 (W8# (indexWord8OffAddr# (a# `plusAddr#` 32#) 0#)))
  readOffAddr# addr# i# s0 =
    let a# = addr# `plusAddr#` (i# *# 40#)
     in case readFloatOffAddr# a# 0# s0 of
          (# s1, x# #) ->
            case readFloatOffAddr# (a# `plusAddr#` 4#) 0# s1 of
              (# s2, y# #) ->
                case readFloatOffAddr# (a# `plusAddr#` 8#) 0# s2 of
                  (# s3, w# #) ->
                    case readFloatOffAddr# (a# `plusAddr#` 12#) 0# s3 of
                      (# s4, h# #) ->
                        case readIntOffAddr# (a# `plusAddr#` 16#) 0# s4 of
                          (# s5, tex# #) ->
                            case readWord32OffAddr# (a# `plusAddr#` 24#) 0# s5 of
                              (# s6, off# #) ->
                                case readWord32OffAddr# (a# `plusAddr#` 28#) 0# s6 of
                                  (# s7, cnt# #) ->
                                    case readWord8OffAddr# (a# `plusAddr#` 32#) 0# s7 of
                                      (# s8, ly# #) ->
                                        (# s8
                                         , DrawCmd
                                            (F# x#)
                                            (F# y#)
                                            (F# w#)
                                            (F# h#)
                                            (I# tex#)
                                            (W32# off#)
                                            (W32# cnt#)
                                            (layerFromWord8 (W8# ly#))
                                         #)
  writeOffAddr# addr# i# cmd s0 =
    case cmd of
      DrawCmd (F# x#) (F# y#) (F# w#) (F# h#) (I# tex#) (W32# off#) (W32# cnt#) ly ->
        let a# = addr# `plusAddr#` (i# *# 40#)
            !(W8# ly#) = layerToWord8 ly
         in writeWord8OffAddr# (a# `plusAddr#` 32#) 0# ly# $
              writeWord32OffAddr# (a# `plusAddr#` 28#) 0# cnt# $
                writeWord32OffAddr# (a# `plusAddr#` 24#) 0# off# $
                  writeIntOffAddr# (a# `plusAddr#` 16#) 0# tex# $
                    writeFloatOffAddr# (a# `plusAddr#` 12#) 0# h# $
                      writeFloatOffAddr# (a# `plusAddr#` 8#) 0# w# $
                        writeFloatOffAddr# (a# `plusAddr#` 4#) 0# y# $
                          writeFloatOffAddr# a# 0# x# s0
  setOffAddr# = primLoopSetOffAddr

primLoopSetByteArray :: Prim a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
primLoopSetByteArray arr# i# n# x s0 = go i# n# s0
  where
    go j# m# s
      | isTrue# (m# ==# 0#) = s
      | otherwise = go (j# +# 1#) (m# -# 1#) (writeByteArray# arr# j# x s)

primLoopSetOffAddr :: Prim a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
primLoopSetOffAddr addr# i# n# x s0 = go i# n# s0
  where
    go j# m# s
      | isTrue# (m# ==# 0#) = s
      | otherwise = go (j# +# 1#) (m# -# 1#) (writeOffAddr# addr# j# x s)

vertexCapacity :: Int
vertexCapacity = 4096

indexCapacity :: Int
indexCapacity = 8192

vertexSize :: Int
vertexSize = 32

indexSize :: Int
indexSize = 4

bufferPoolLimit :: Int
bufferPoolLimit = 4

cmdInitialCapacity :: Int
cmdInitialCapacity = 64

{-# INLINE newDrawArena #-}
newDrawArena :: IO DrawArena
newDrawArena = do
  vFPtr <- mallocForeignPtrBytes (vertexCapacity * vertexSize)
  iFPtr <- mallocForeignPtrBytes (indexCapacity * indexSize)
  let !vRaw = unsafeForeignPtrToPtr vFPtr
      !iRaw = unsafeForeignPtrToPtr iFPtr
  daVertexFPtr <- newIORef vFPtr
  daVertexPtr <- newIORef vRaw
  daVertexCap <- newIORef vertexCapacity
  daVertexCount <- newIORef 0
  daVertexPool <- newIORef []
  daIndexFPtr <- newIORef iFPtr
  daIndexPtr <- newIORef iRaw
  daIndexCap <- newIORef indexCapacity
  daIndexCount <- newIORef 0
  daIndexPool <- newIORef []
  cmdStore <- newPrimArray cmdInitialCapacity
  daCmdStore <- newIORef cmdStore
  daCmdCount <- newIORef 0
  daCmdCapacity <- newIORef cmdInitialCapacity
  daCurrentLayer <- newIORef LayerContent
  daCurrentClip <- newIORef (0, 0, 1e9, 1e9)
  daCurrentTexture <- newIORef glyphAtlasTextureId
  daCmdStartIndex <- newIORef 0
  pure
    DrawArena
      { daVertexFPtr
      , daVertexPtr
      , daVertexCap
      , daVertexCount
      , daVertexPool
      , daIndexFPtr
      , daIndexPtr
      , daIndexCap
      , daIndexCount
      , daIndexPool
      , daCmdStore
      , daCmdCount
      , daCmdCapacity
      , daCurrentLayer
      , daCurrentClip
      , daCurrentTexture
      , daCmdStartIndex
      }

{-# INLINE resetDrawArena #-}
resetDrawArena :: DrawArena -> IO ()
resetDrawArena da = do
  writeIORef (daVertexCount da) 0
  writeIORef (daIndexCount da) 0
  writeIORef (daCmdCount da) 0
  writeIORef (daCurrentLayer da) LayerContent
  writeIORef (daCurrentClip da) (0, 0, 1e9, 1e9)
  writeIORef (daCurrentTexture da) glyphAtlasTextureId
  writeIORef (daCmdStartIndex da) 0

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
  if required <= cap
    then pure ()
    else do
      oldFPtr <- readIORef fptrRef
      let newCap = max (cap * 2) required
          newBytes = newCap * elemBytes
      newFPtr <- poolTake pool newBytes newCap
      let !newRaw = unsafeForeignPtrToPtr newFPtr
      withForeignPtr oldFPtr $ \oldP ->
        copyArray newRaw oldP (count * elemBytes)
      poolGive pool oldFPtr cap
      writeIORef fptrRef newFPtr
      writeIORef ptrRef newRaw
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
  if vCount + needV <= vCap && iCount + needI <= iCap
    then do
      vp <- readIORef (daVertexPtr da)
      ip <- readIORef (daIndexPtr da)
      pure (vp, ip, vCount, iCount)
    else do
      ensureCapacity da needV needI
      vp <- readIORef (daVertexPtr da)
      ip <- readIORef (daIndexPtr da)
      vc <- readIORef (daVertexCount da)
      ic <- readIORef (daIndexCount da)
      pure (vp, ip, vc, ic)

{-# NOINLINE growCmdStore #-}
growCmdStore :: DrawArena -> Int -> IO ()
growCmdStore da oldCap = do
  let newCap = oldCap * 2
  arr <- readIORef (daCmdStore da)
  newArr <- newPrimArray newCap
  copyMutablePrimArray newArr 0 arr 0 oldCap
  writeIORef (daCmdStore da) newArr
  writeIORef (daCmdCapacity da) newCap

{-# INLINE appendCmd #-}
appendCmd :: DrawArena -> DrawCmd -> IO ()
appendCmd da cmd = do
  count <- readIORef (daCmdCount da)
  cap <- readIORef (daCmdCapacity da)
  when (count >= cap) $ growCmdStore da cap
  arr <- readIORef (daCmdStore da)
  writePrimArray arr count cmd
  writeIORef (daCmdCount da) (count + 1)

{-# INLINE currentLayer #-}
currentLayer :: DrawArena -> IO Layer
currentLayer = readIORef . daCurrentLayer

{-# INLINE beginLayer #-}
beginLayer :: DrawArena -> Layer -> IO ()
beginLayer da layer = do
  cur <- readIORef (daCurrentLayer da)
  when (cur /= layer) $ do
    flushCmd da
    writeIORef (daCurrentLayer da) layer
    idx <- readIORef (daIndexCount da)
    writeIORef (daCmdStartIndex da) idx

{-# INLINE flushCmd #-}
flushCmd :: DrawArena -> IO ()
flushCmd da = do
  startIdx <- readIORef (daCmdStartIndex da)
  curIdx <- readIORef (daIndexCount da)
  let count = curIdx - startIdx
  if count > 0
    then do
      (cx, cy, cw, ch) <- readIORef (daCurrentClip da)
      tex <- readIORef (daCurrentTexture da)
      layer <- readIORef (daCurrentLayer da)
      cmdCount <- readIORef (daCmdCount da)
      let newCmd =
            DrawCmd
              { cmdClipX = cx
              , cmdClipY = cy
              , cmdClipW = cw
              , cmdClipH = ch
              , cmdTextureId = tex
              , cmdIndexOffset = fromIntegral startIdx
              , cmdIndexCount = fromIntegral count
              , cmdLayer = layer
              }
      merged <-
        if cmdCount > 0
          then do
            arr <- readIORef (daCmdStore da)
            prev <- readPrimArray arr (cmdCount - 1)
            if sameDrawBatch prev newCmd
              then do
                writePrimArray
                  arr
                  (cmdCount - 1)
                  prev {cmdIndexCount = cmdIndexCount prev + cmdIndexCount newCmd}
                pure True
              else pure False
          else pure False
      unless merged $ appendCmd da newCmd
      writeIORef (daCmdStartIndex da) curIdx
    else pure ()

{-# INLINE sameDrawBatch #-}
sameDrawBatch :: DrawCmd -> DrawCmd -> Bool
sameDrawBatch prev next =
  cmdClipX prev == cmdClipX next
    && cmdClipY prev == cmdClipY next
    && cmdClipW prev == cmdClipW next
    && cmdClipH prev == cmdClipH next
    && cmdTextureId prev == cmdTextureId next
    && cmdLayer prev == cmdLayer next
    && cmdIndexOffset prev + cmdIndexCount prev == cmdIndexOffset next

{-# INLINE setClip #-}
setClip :: DrawArena -> Rect -> IO ()
setClip da (Rect x y w h) = do
  flushCmd da
  writeIORef (daCurrentClip da) (x, y, w, h)

{-# INLINE withClip #-}
withClip :: DrawArena -> Rect -> IO a -> IO a
withClip da rect act = do
  old <- readIORef (daCurrentClip da)
  let (ox, oy, ow, oh) = old
      prev = Rect ox oy ow oh
      clip = maybe (Rect 0 0 0 0) id (rectIntersect prev rect)
  setClip da clip
  r <- act
  setClip da prev
  pure r

{-# INLINE setTexture #-}
setTexture :: DrawArena -> Int -> IO ()
setTexture da tex = do
  cur <- readIORef (daCurrentTexture da)
  when (cur /= tex) $ do
    flushCmd da
    writeIORef (daCurrentTexture da) tex

{-# INLINE unpackColorF #-}
unpackColorF :: Color -> (Float, Float, Float, Float)
unpackColorF (Color w) =
  let !inv255 = 1.0 / 255.0
      !r = fromIntegral ((w `shiftR` 24) .&. 0xFF) * inv255
      !g = fromIntegral ((w `shiftR` 16) .&. 0xFF) * inv255
      !b = fromIntegral ((w `shiftR` 8) .&. 0xFF) * inv255
      !a = fromIntegral (w .&. 0xFF) * inv255
   in (r, g, b, a)

{-# INLINE pokeVertex #-}
pokeVertex :: Ptr Word8 -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
pokeVertex vp off px py r g b a u v =
  pokeVertexSIMD vp off px py r g b a u v

{-# INLINE pushQuad #-}
pushQuad :: DrawArena -> Rect -> Float -> Float -> Float -> Float -> Color -> IO ()
pushQuad da (Rect x y w h) u0 v0 u1 v1 col = do
  (vp, ip, base, baseIdx) <- ensureAndAlloc da 4 6
  let !(r, g, b, a) = unpackColorF col
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !baseIdxWord = fromIntegral base :: Word32
  pokeQuadSIMD vp vOff ip iOff x y w h u0 v0 u1 v1 r g b a baseIdxWord
  writeIORef (daVertexCount da) (base + 4)
  writeIORef (daIndexCount da) (baseIdx + 6)

-- Quad with a color per corner. GPU interpolates across the two triangles.
-- Corners: top-left, top-right, bottom-right, bottom-left.
{-# INLINE pushQuadGradient #-}
pushQuadGradient :: DrawArena -> Rect -> Color -> Color -> Color -> Color -> IO ()
pushQuadGradient da (Rect x y w h) tl tr br bl
  | w <= 0 || h <= 0 = pure ()
  | otherwise = do
      setTexture da glyphAtlasTextureId
      (vp, ip, base, baseIdx) <- ensureAndAlloc da 4 6
      let !c0 = unpackColorF tl
          !c1 = unpackColorF tr
          !c2 = unpackColorF br
          !c3 = unpackColorF bl
          !vOff = base * vertexSize
          !iOff = baseIdx * indexSize
          !baseIdxWord = fromIntegral base :: Word32
      pokeQuadGradientSIMD vp vOff ip iOff x y w h whitePixelU whitePixelV c0 c1 c2 c3 baseIdxWord
      writeIORef (daVertexCount da) (base + 4)
      writeIORef (daIndexCount da) (baseIdx + 6)

-- Reserved texture id. Terminal raster treats these quads as backdrop dim,
-- not a solid fill. Mix comes from the vertex color alpha.
backdropDimTextureId :: Int
backdropDimTextureId = 0x7ffffffe

-- Reserved texture id for the per-glyph SDL_ttf atlas. The renderer binds
-- the glyph atlas SDL_Texture when it sees this id. Glyphs are cached as
-- white-on-alpha so vertex color tints them at draw time.
glyphAtlasTextureId :: Int
glyphAtlasTextureId = 0x7ffffffd

-- | Center of the 4x4 white pixel patch in the 1024x1024 font atlas.
whitePixelU :: Float
whitePixelU = 1.5 / 1024.0

whitePixelV :: Float
whitePixelV = 1.5 / 1024.0

{-# INLINE pushRect #-}
pushRect :: DrawArena -> Rect -> Color -> IO ()
pushRect da rect col = do
  setTexture da glyphAtlasTextureId
  pushQuad da rect whitePixelU whitePixelV whitePixelU whitePixelV col

{-# INLINE pushBackdropDim #-}
pushBackdropDim :: DrawArena -> Rect -> Color -> IO ()
pushBackdropDim da rect col = do
  setTexture da backdropDimTextureId
  pushQuad da rect 0 0 1 1 col

{-# INLINE pushImage #-}
pushImage :: DrawArena -> Rect -> Int -> Float -> Float -> Float -> Float -> Color -> IO ()
pushImage da rect tex u0 v0 u1 v1 col
  | tex <= 0 = pushRect da rect col
  | otherwise = do
      setTexture da tex
      pushQuad da rect u0 v0 u1 v1 col

-- 4 segments per 90° arc. Lookup table in cornerCosSin has 5 points per quadrant.
cornerSegments :: Int
cornerSegments = 4

-- Precomputed unit-circle cos/sin for rounded-rect corners (4 segments per 90° arc).
{-# NOINLINE cornerQuadrant #-}
cornerQuadrant :: Float -> Int
cornerQuadrant a0 =
  if a0 >= pi && a0 < pi * 1.5
    then 0
    else
      if a0 >= pi * 1.5
        then 1
        else if a0 < pi * 0.5 then 2 else 3

{-# NOINLINE cornerCosSin #-}
cornerCosSin :: Int -> Int -> (Float, Float)
cornerCosSin q seg =
  case q * 5 + seg of
    0 -> (-1.0, 0.0)
    1 -> (-0.9238795325, -0.3826834324)
    2 -> (-0.7071067812, -0.7071067812)
    3 -> (-0.3826834324, -0.9238795325)
    4 -> (0.0, -1.0)
    5 -> (0.0, -1.0)
    6 -> (0.3826834324, -0.9238795325)
    7 -> (0.7071067812, -0.7071067812)
    8 -> (0.9238795325, -0.3826834324)
    9 -> (1.0, 0.0)
    10 -> (1.0, 0.0)
    11 -> (0.9238795325, 0.3826834324)
    12 -> (0.7071067812, 0.7071067812)
    13 -> (0.3826834324, 0.9238795325)
    14 -> (0.0, 1.0)
    15 -> (0.0, 1.0)
    16 -> (-0.3826834324, 0.9238795325)
    17 -> (-0.7071067812, 0.7071067812)
    18 -> (-0.9238795325, 0.3826834324)
    19 -> (-1.0, 0.0)
    _ -> (0.0, 0.0)

{-# INLINE getCurrentClip #-}
getCurrentClip :: DrawArena -> IO Rect
getCurrentClip da = do
  (x, y, w, h) <- readIORef (daCurrentClip da)
  pure (Rect x y w h)

pushCornerFan :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushCornerFan da cx cy rad a0 _a1 col = do
  setTexture da glyphAtlasTextureId
  let !segs = cornerSegments
      !ring = segs + 1
      !aa = 1.0
      !needV = 1 + 2 * ring
      !needI = segs * 9
      !q = cornerQuadrant a0
  (vp, ip, base, baseIdx) <- ensureAndAlloc da needV needI
  let !(cr, cg, cb, ca) = unpackColorF col
      !centerIdx = fromIntegral base :: Word32
  pokeVertex vp (base * vertexSize) cx cy cr cg cb ca whitePixelU whitePixelV
  forM_ [0 .. segs] $ \i -> do
    let !(ct, st) = cornerCosSin q i
        !rimI = base + 1 + i
        !outI = base + 1 + ring + i
        !inRad = max 0 (rad - aa)
    pokeVertex vp (rimI * vertexSize) (cx + inRad * ct) (cy + inRad * st) cr cg cb ca whitePixelU whitePixelV
    pokeVertex vp (outI * vertexSize) (cx + rad * ct) (cy + rad * st) cr cg cb 0 whitePixelU whitePixelV
    when (i > 0) $ do
      let !k = i - 1
          !rim0 = fromIntegral (base + i) :: Word32
          !rim1 = fromIntegral (base + 1 + i) :: Word32
          !out0 = fromIntegral (base + 1 + ring + k) :: Word32
          !out1 = fromIntegral (base + 1 + ring + i) :: Word32
          !fillOff = (baseIdx + k * 3) * indexSize
          !fringeOff = (baseIdx + segs * 3 + k * 6) * indexSize
      pokeByteOff ip fillOff centerIdx
      pokeByteOff ip (fillOff + 4) rim0
      pokeByteOff ip (fillOff + 8) rim1
      pokeQuadIndices ip fringeOff rim0 out0 out1 rim1

  writeIORef (daVertexCount da) (base + needV)
  writeIORef (daIndexCount da) (baseIdx + needI)

{-# INLINE pokeQuadIndices #-}
pokeQuadIndices :: Ptr Word8 -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
pokeQuadIndices ip off a b c d = do
  pokeByteOff ip off a
  pokeByteOff ip (off + 4) b
  pokeByteOff ip (off + 8) c
  pokeByteOff ip (off + 12) a
  pokeByteOff ip (off + 16) c
  pokeByteOff ip (off + 20) d

{-# INLINE pushRoundedRect #-}
pushRoundedRect :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRect da rect@(Rect x y w h) radius col
  | w <= 0 || h <= 0 = pure ()
  | radius <= 0.5 = pushRect da rect col
  | otherwise = do
      let !rad = min radius (min (w * 0.5) (h * 0.5))
      if rad <= 0.5
        then pushRect da rect col
        else do
          setTexture da glyphAtlasTextureId
          let !midW = max 0 (w - 2 * rad)
              !midH = max 0 (h - 2 * rad)
          when (midW > 0 && midH > 0) $
            pushQuad da (Rect (x + rad) (y + rad) midW midH) whitePixelU whitePixelV whitePixelU whitePixelV col
          when (midW > 0) $ do
            pushQuad da (Rect (x + rad) y midW rad) whitePixelU whitePixelV whitePixelU whitePixelV col
            pushQuad da (Rect (x + rad) (y + h - rad) midW rad) whitePixelU whitePixelV whitePixelU whitePixelV col
          when (midH > 0) $ do
            pushQuad da (Rect x (y + rad) rad midH) whitePixelU whitePixelV whitePixelU whitePixelV col
            pushQuad da (Rect (x + w - rad) (y + rad) rad midH) whitePixelU whitePixelV whitePixelU whitePixelV col
          pushCornerFan da (x + rad) (y + rad) rad pi (pi * 1.5) col
          pushCornerFan da (x + w - rad) (y + rad) rad (pi * 1.5) (pi * 2) col
          pushCornerFan da (x + w - rad) (y + h - rad) rad 0 (pi * 0.5) col
          pushCornerFan da (x + rad) (y + h - rad) rad (pi * 0.5) pi col

{-# INLINE pushRoundedStroke #-}
pushRoundedStroke :: DrawArena -> Rect -> Float -> Float -> Color -> IO ()
pushRoundedStroke da (Rect x y w h) radius bw col
  | w <= 0 || h <= 0 || bw <= 0 = pure ()
  | otherwise = do
      setTexture da glyphAtlasTextureId
      let !rad = min (max 0 radius) (min (w * 0.5) (h * 0.5))
          !ibw = min bw (min (w * 0.5) (h * 0.5))
      if rad <= 0.5
        then do
          let !t = ibw
              !ox = x + t / 2
              !oy = y + t / 2
              !ow = max 0 (w - t)
              !oh = max 0 (h - t)
          pushStrokeAA da ox oy (ox + ow) oy t col
          pushStrokeAA da ox (oy + oh) (ox + ow) (oy + oh) t col
          when (oh > 0) $ pushStrokeAA da ox oy ox (oy + oh) t col
          when (oh > 0) $ pushStrokeAA da (ox + ow) oy (ox + ow) (oy + oh) t col
        else do
          let !midW = max 0 (w - 2 * rad)
              !midH = max 0 (h - 2 * rad)
              !topY = y + ibw / 2
              !botY = y + h - ibw / 2
              !leftX = x + ibw / 2
              !rightX = x + w - ibw / 2
          when (midW > 0) $ do
            pushStrokeAA da (x + rad) topY (x + rad + midW) topY ibw col
            pushStrokeAA da (x + rad) botY (x + rad + midW) botY ibw col
          when (midH > 0) $ do
            pushStrokeAA da leftX (y + rad) leftX (y + rad + midH) ibw col
            pushStrokeAA da rightX (y + rad) rightX (y + rad + midH) ibw col
          let !cr = max 0.25 (rad - ibw / 2)
          if midW <= 0 && midH <= 0
            then forM_ [0 .. 3] $ \q -> pushCornerArcStroke da (x + w * 0.5) (y + h * 0.5) cr ibw q col
            else do
              pushCornerArcStroke da (x + rad) (y + rad) cr ibw 0 col
              pushCornerArcStroke da (x + w - rad) (y + rad) cr ibw 1 col
              pushCornerArcStroke da (x + w - rad) (y + h - rad) cr ibw 2 col
              pushCornerArcStroke da (x + rad) (y + h - rad) cr ibw 3 col

{-# INLINE pushLine #-}
pushLine :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushLine da x1 y1 x2 y2 thickness col =
  case strokeAxes x1 y1 x2 y2 of
    Nothing -> pure ()
    Just (dx, dy, len) -> do
      setTexture da glyphAtlasTextureId
      let r = thickness / 2
          step = max 0.3 (r * 0.4)
          n = max (1 :: Int) (ceiling (len / step))
      forM_ [0 .. n] $ \i -> do
        let u = fromIntegral i / fromIntegral n
            cx = x1 + dx * u
            cy = y1 + dy * u
        pushRoundedRect da (Rect (cx - r) (cy - r) thickness thickness) r col

-- Coverage-AA strip for a straight segment. Same weight as pushCornerArcStroke,
-- without round caps that blob at rounded-rect corners.
pushStrokeAA :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushStrokeAA da x0 y0 x1 y1 bw col
  | bw <= 0 = pure ()
  | otherwise =
      case strokeAxes x0 y0 x1 y1 of
        Nothing -> pure ()
        Just (dx, dy, len) -> do
          setTexture da glyphAtlasTextureId
          let !nx = (-dy) / len
              !ny = dx / len
              !half = bw * 0.5
              !aa = min 1 half
              !core = half - aa
              !(cr, cg, cb, ca) = unpackColorF col
          (vp, ip, base, baseIdx) <- ensureAndAlloc da 8 18
          let pokeEnd vi px py = do
                let ((p0x, p0y), (p1x, p1y), (p2x, p2y), (p3x, p3y)) =
                      strokeStripNormalsSIMD px py nx ny (-half) (-core) core half
                    !vBase = (base + vi) * vertexSize
                pokeVertex vp vBase p0x p0y cr cg cb 0 whitePixelU whitePixelV
                pokeVertex vp (vBase + 32) p1x p1y cr cg cb ca whitePixelU whitePixelV
                pokeVertex vp (vBase + 64) p2x p2y cr cg cb ca whitePixelU whitePixelV
                pokeVertex vp (vBase + 96) p3x p3y cr cg cb 0 whitePixelU whitePixelV
              !a = fromIntegral base :: Word32
              !b = a + 4
          pokeEnd 0 x0 y0
          pokeEnd 4 x1 y1
          pokeQuadIndices ip (baseIdx * indexSize) a (a + 1) (b + 1) b
          pokeQuadIndices ip ((baseIdx + 6) * indexSize) (a + 1) (a + 2) (b + 2) (b + 1)
          pokeQuadIndices ip ((baseIdx + 12) * indexSize) (a + 2) (a + 3) (b + 3) (b + 2)
          writeIORef (daVertexCount da) (base + 8)
          writeIORef (daIndexCount da) (baseIdx + 18)

strokeAxes :: Float -> Float -> Float -> Float -> Maybe (Float, Float, Float)
strokeAxes x0 y0 x1 y1 =
  let dx = x1 - x0
      dy = y1 - y0
      len = sqrt (dx * dx + dy * dy)
   in if len < 0.001 then Nothing else Just (dx, dy, len)

-- Coverage fringe on inner and outer edges. Quarter-arcs use `cornerCosSin`.
pushCornerArcStroke :: DrawArena -> Float -> Float -> Float -> Float -> Int -> Color -> IO ()
pushCornerArcStroke da cx cy radius bw q col
  | bw <= 0 || radius <= 0 = pure ()
  | otherwise = do
      setTexture da glyphAtlasTextureId
      let !r = max 0.25 radius
          !n = cornerSegments
          !half = bw * 0.5
          !aa = min 1 half
          !innerAA = max 0 (r - half)
          !outerAA = r + half
          !inner = r - half + aa
          !outer = r + half - aa
          !needV = (n + 1) * 4
          !needI = n * 18
      (vp, ip, base, baseIdx) <- ensureAndAlloc da needV needI
      let !(cr, cg, cb, ca) = unpackColorF col
      forM_ [0 .. n] $ \i -> do
        let !(ct, st) = cornerCosSin q i
            !v0 = base + i * 4
            !vBase = v0 * vertexSize
            ((p0x, p0y), (p1x, p1y), (p2x, p2y), (p3x, p3y)) =
              concentricOffsetsSIMD cx cy ct st innerAA inner outer outerAA
        pokeVertex vp vBase p0x p0y cr cg cb 0 whitePixelU whitePixelV
        pokeVertex vp (vBase + 32) p1x p1y cr cg cb ca whitePixelU whitePixelV
        pokeVertex vp (vBase + 64) p2x p2y cr cg cb ca whitePixelU whitePixelV
        pokeVertex vp (vBase + 96) p3x p3y cr cg cb 0 whitePixelU whitePixelV
      forM_ [0 .. n - 1] $ \i -> do
        let !a = fromIntegral (base + i * 4) :: Word32
            !b = a + 4
            !iOff = (baseIdx + i * 18) * indexSize
        pokeQuadIndices ip iOff a (a + 1) (b + 1) b
        pokeQuadIndices ip (iOff + 24) (a + 1) (a + 2) (b + 2) (b + 1)
        pokeQuadIndices ip (iOff + 48) (a + 2) (a + 3) (b + 3) (b + 2)
      writeIORef (daVertexCount da) (base + needV)
      writeIORef (daIndexCount da) (baseIdx + needI)

-- One quad per segment. Plots and diagrams use this; pushLine stamps capsules.
{-# INLINE pushStroke #-}
pushStroke :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushStroke da x1 y1 x2 y2 thickness col
  | thickness <= 0 = pure ()
  | otherwise =
      case strokeAxes x1 y1 x2 y2 of
        Nothing -> pure ()
        Just (dx, dy, len) -> do
          let !invLen = (thickness * 0.5) / len
              !hx = (-dy) * invLen
              !hy = dx * invLen
          setTexture da glyphAtlasTextureId
          (vp, ip, base, baseIdx) <- ensureAndAlloc da 4 6
          let !(r, g, b, a) = unpackColorF col
              !vOff = base * vertexSize
              !iOff = baseIdx * indexSize
              !baseIdxWord = fromIntegral base :: Word32
              poke off px py = pokeVertex vp off px py r g b a whitePixelU whitePixelV
          poke vOff (x1 + hx) (y1 + hy)
          poke (vOff + 32) (x2 + hx) (y2 + hy)
          poke (vOff + 64) (x2 - hx) (y2 - hy)
          poke (vOff + 96) (x1 - hx) (y1 - hy)
          pokeByteOff ip iOff baseIdxWord
          pokeByteOff ip (iOff + 4) (baseIdxWord + 1)
          pokeByteOff ip (iOff + 8) (baseIdxWord + 2)
          pokeByteOff ip (iOff + 12) baseIdxWord
          pokeByteOff ip (iOff + 16) (baseIdxWord + 2)
          pokeByteOff ip (iOff + 20) (baseIdxWord + 3)
          writeIORef (daVertexCount da) (base + 4)
          writeIORef (daIndexCount da) (baseIdx + 6)

{-# INLINE emitDrawOps #-}
emitDrawOps :: DrawArena -> FontMetrics -> Vector DrawOp -> IO ()
emitDrawOps da fm ops = V.mapM_ emitOne ops
  where
    emitOne (FillRect r c) = pushRect da r c
    emitOne (FillTriangle x0 y0 x1 y1 x2 y2 c) = pushFilledTriangle da x0 y0 x1 y1 x2 y2 c
    emitOne (Stroke x0 y0 x1 y1 t c) = pushStroke da x0 y0 x1 y1 t c
    emitOne (DrawText x y ax ay t c) = do
      let Rect px py _ _ = drawTextBox fm x y ax ay t
      pushText da fm px py t c

{-# INLINE pushFilledTriangle #-}
pushFilledTriangle :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushFilledTriangle da x0 y0 x1 y1 x2 y2 col = do
  setTexture da glyphAtlasTextureId
  (vp, ip, base, baseIdx) <- ensureAndAlloc da 3 3
  let !(r, g, b, a) = unpackColorF col
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !baseIdxWord = fromIntegral base :: Word32
  pokeVertex vp vOff x0 y0 r g b a whitePixelU whitePixelV
  pokeVertex vp (vOff + 32) x1 y1 r g b a whitePixelU whitePixelV
  pokeVertex vp (vOff + 64) x2 y2 r g b a whitePixelU whitePixelV
  pokeByteOff ip iOff baseIdxWord
  pokeByteOff ip (iOff + 4) (baseIdxWord + 1)
  pokeByteOff ip (iOff + 8) (baseIdxWord + 2)
  writeIORef (daVertexCount da) (base + 3)
  writeIORef (daIndexCount da) (baseIdx + 3)

{-# INLINE pushText #-}
pushText :: DrawArena -> FontMetrics -> Float -> Float -> T.Text -> Color -> IO ()
pushText da fm x y txt col = go x txt
  where
    go !ox !t =
      case T.uncons t of
        Nothing -> pure ()
        Just (c, rest) -> do
          let !adv = fmAdvance fm c
          case fmGlyph fm c of
            Nothing -> do
              when (adv > 0 && c /= ' ') $
                pushRect da (Rect ox y adv (fmLineHeight fm)) col
              go (ox + adv) rest
            Just gq -> do
              let !gx = ox + gqX gq
                  !gy = y + gqY gq
                  !gw = gqW gq
                  !gh = gqH gq
              setTexture da glyphAtlasTextureId
              pushQuad da (Rect gx gy gw gh) (gqU0 gq) (gqV0 gq) (gqU1 gq) (gqV1 gq) col
              go (ox + adv) rest

{-# INLINE drawCmdCount #-}
drawCmdCount :: DrawData -> Int
drawCmdCount dd = sizeofPrimArray (drawCommands dd)

{-# INLINE drawCmdNull #-}
drawCmdNull :: DrawData -> Bool
drawCmdNull dd = sizeofPrimArray (drawCommands dd) == 0

{-# INLINE drawCmdAt #-}
drawCmdAt :: DrawData -> Int -> DrawCmd
drawCmdAt dd i = indexPrimArray (drawCommands dd) i

{-# INLINE foldDrawCmds #-}
foldDrawCmds :: (a -> DrawCmd -> a) -> a -> DrawData -> a
foldDrawCmds f z dd =
  let cmds = drawCommands dd
      n = sizeofPrimArray cmds
      go !i !acc
        | i >= n = acc
        | otherwise = go (i + 1) (f acc (indexPrimArray cmds i))
   in go 0 z

{-# INLINE drawCmdFilter #-}
drawCmdFilter :: (DrawCmd -> Bool) -> DrawData -> Vector DrawCmd
drawCmdFilter p dd =
  let cmds = drawCommands dd
      n = sizeofPrimArray cmds
   in V.fromList [indexPrimArray cmds i | i <- [0 .. n - 1], p (indexPrimArray cmds i)]

{-# INLINE drawCmdForLayer #-}
drawCmdForLayer :: Layer -> DrawData -> Vector DrawCmd
drawCmdForLayer ly dd =
  let LayerSlice off cnt = layerSliceOf dd ly
      cmds = drawCommands dd
   in V.generate cnt (\i -> indexPrimArray cmds (off + i))

{-# INLINE forDrawCmdsInLayer_ #-}
forDrawCmdsInLayer_ :: Layer -> DrawData -> (DrawCmd -> IO ()) -> IO ()
forDrawCmdsInLayer_ ly dd f =
  let LayerSlice off cnt = layerSliceOf dd ly
      cmds = drawCommands dd
      go !i
        | i >= cnt = pure ()
        | otherwise = f (indexPrimArray cmds (off + i)) >> go (i + 1)
   in go 0

{-# INLINE layerSliceOf #-}
layerSliceOf :: DrawData -> Layer -> LayerSlice
layerSliceOf dd ly = indexPrimArray (drawLayerSlices dd) (fromEnum ly)

drawCmdElemsForLayer :: Layer -> DrawData -> [DrawCmd]
drawCmdElemsForLayer ly dd =
  let LayerSlice off cnt = layerSliceOf dd ly
      cmds = drawCommands dd
   in [indexPrimArray cmds (off + i) | i <- [0 .. cnt - 1]]

-- | Layer buckets as contiguous slices. No per-frame list partition.
drawCmdPartitionByLayer :: DrawData -> ([DrawCmd], [DrawCmd], [DrawCmd], [DrawCmd])
drawCmdPartitionByLayer dd =
  ( drawCmdElemsForLayer LayerBackground dd
  , drawCmdElemsForLayer LayerContent dd
  , drawCmdElemsForLayer LayerOverlay dd
  , drawCmdElemsForLayer LayerChrome dd
  )

drawCmdElems :: DrawData -> [DrawCmd]
drawCmdElems dd =
  let cmds = drawCommands dd
      n = sizeofPrimArray cmds
   in [indexPrimArray cmds i | i <- [0 .. n - 1]]

{-# INLINE finishDraw #-}
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

groupCmdsByLayer ::
  MutablePrimArray RealWorld DrawCmd ->
  Int ->
  IO (PrimArray DrawCmd, PrimArray LayerSlice)
groupCmdsByLayer src n = do
  (nBg, nCt, nOv, nCh) <- countLayers src n 0 0 0 0 0
  let offBg = 0
      offCt = offBg + nBg
      offOv = offCt + nCt
      offCh = offOv + nOv
  dest <- newPrimArray n
  cBg <- newIORef offBg
  cCt <- newIORef offCt
  cOv <- newIORef offOv
  cCh <- newIORef offCh
  let writeSlot ly cmd = do
        slotRef <-
          case ly of
            LayerBackground -> pure cBg
            LayerContent -> pure cCt
            LayerOverlay -> pure cOv
            LayerChrome -> pure cCh
        i <- readIORef slotRef
        writePrimArray dest i cmd
        writeIORef slotRef (i + 1)
      scatter !i
        | i >= n = pure ()
        | otherwise = do
            cmd <- readPrimArray src i
            writeSlot (cmdLayer cmd) cmd
            scatter (i + 1)
  scatter 0
  frozen <- unsafeFreezePrimArray dest
  sliceArr <- newPrimArray 4
  writePrimArray sliceArr 0 (LayerSlice offBg nBg)
  writePrimArray sliceArr 1 (LayerSlice offCt nCt)
  writePrimArray sliceArr 2 (LayerSlice offOv nOv)
  writePrimArray sliceArr 3 (LayerSlice offCh nCh)
  slices <- unsafeFreezePrimArray sliceArr
  pure (frozen, slices)

countLayers ::
  MutablePrimArray RealWorld DrawCmd ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO (Int, Int, Int, Int)
countLayers src n i bg ct ov ch
  | i >= n = pure (bg, ct, ov, ch)
  | otherwise = do
      cmd <- readPrimArray src i
      case cmdLayer cmd of
        LayerBackground -> countLayers src n (i + 1) (bg + 1) ct ov ch
        LayerContent -> countLayers src n (i + 1) bg (ct + 1) ov ch
        LayerOverlay -> countLayers src n (i + 1) bg ct (ov + 1) ch
        LayerChrome -> countLayers src n (i + 1) bg ct ov (ch + 1)
