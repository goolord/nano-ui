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
  , setDrawSnapScale
  , getDrawSnapScale
  , beginLayer
  , setClip
  , pushRect
  , pushQuadGradient
  , pushBackdropDim
  , backdropDimTextureId
  , glyphAtlasTextureId
  , pushImage
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushText
  , pushTextStyled
  , pushLine
  , pushStrokeAA
  , pushStroke
  , pushFilledTriangle
  , snapToPixel
  , DrawOp (..)
  , DrawingBuild
  , emitDrawOps
  , drawTextBox
  , shiftDrawOp
  , finishDraw
  , drawCmdCount
  , drawCmdNull
  , foldDrawCmds
  , forDrawCmdsInLayer_
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
import NanoUI.Font (FontMetrics (..), GlyphQuad (..), RunQuad (..), lineWidth)
import NanoUI.Style
  ( FontStyle (..)
  , FontWeight (..)
  , TextDecoration (..)
  )
import NanoUI.Types (Color (..), Rect (..), onGrid, rectIntersect)
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
  | FillRoundedRect !Rect {-# UNPACK #-} !Float !Color
  | FillTriangle
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  | FillCircle
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
  | StrokeRoundedRect !Rect {-# UNPACK #-} !Float {-# UNPACK #-} !Float !Color
  | StrokeCircle
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  | StrokeLineAA
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Color
  | FillQuadGradient !Rect !Color !Color !Color !Color
  | DrawImageRect
      !Rect
      {-# UNPACK #-} !Int
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
    FillRoundedRect (Rect x y w h) r c -> FillRoundedRect (Rect (x + dx) (y + dy) w h) r c
    FillTriangle x0 y0 x1 y1 x2 y2 c ->
      FillTriangle (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) (x2 + dx) (y2 + dy) c
    FillCircle cx cy r c -> FillCircle (cx + dx) (cy + dy) r c
    Stroke x0 y0 x1 y1 t c -> Stroke (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) t c
    StrokeRoundedRect (Rect x y w h) r bw c -> StrokeRoundedRect (Rect (x + dx) (y + dy) w h) r bw c
    StrokeCircle cx cy r bw c -> StrokeCircle (cx + dx) (cy + dy) r bw c
    StrokeLineAA x0 y0 x1 y1 bw c -> StrokeLineAA (x0 + dx) (y0 + dy) (x1 + dx) (y1 + dy) bw c
    FillQuadGradient (Rect x y w h) c0 c1 c2 c3 -> FillQuadGradient (Rect (x + dx) (y + dy) w h) c0 c1 c2 c3
    DrawImageRect (Rect x y w h) tex u0 v0 u1 v1 c -> DrawImageRect (Rect (x + dx) (y + dy) w h) tex u0 v0 u1 v1 c
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
  , daSnapScale :: !(IORef Float)
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
  daSnapScale <- newIORef 0.0
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
      , daSnapScale
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
      s <- readIORef (daSnapScale da)
      setTexture da glyphAtlasTextureId
      (vp, ip, base, baseIdx) <- ensureAndAlloc da 4 6
      let !px = snapToPixel s x
          !py = snapToPixel s y
          !c0 = unpackColorF tl
          !c1 = unpackColorF tr
          !c2 = unpackColorF br
          !c3 = unpackColorF bl
          !vOff = base * vertexSize
          !iOff = baseIdx * indexSize
          !baseIdxWord = fromIntegral base :: Word32
      pokeQuadGradientSIMD vp vOff ip iOff px py w h whitePixelU whitePixelV c0 c1 c2 c3 baseIdxWord
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

{-# INLINE snapRectOrigin #-}
snapRectOrigin :: DrawArena -> Rect -> IO Rect
snapRectOrigin da (Rect x y w h) = do
  s <- readIORef (daSnapScale da)
  pure (Rect (snapToPixel s x) (snapToPixel s y) w h)

{-# INLINE pushRect #-}
pushRect :: DrawArena -> Rect -> Color -> IO ()
pushRect da rect col = do
  r <- snapRectOrigin da rect
  setTexture da glyphAtlasTextureId
  pushQuad da r whitePixelU whitePixelV whitePixelU whitePixelV col

{-# INLINE pushBackdropDim #-}
pushBackdropDim :: DrawArena -> Rect -> Color -> IO ()
pushBackdropDim da rect col = do
  r <- snapRectOrigin da rect
  setTexture da backdropDimTextureId
  pushQuad da r 0 0 1 1 col

{-# INLINE pushImage #-}
pushImage :: DrawArena -> Rect -> Int -> Float -> Float -> Float -> Float -> Color -> IO ()
pushImage da rect tex u0 v0 u1 v1 col
  | tex <= 0 = pushRect da rect col
  | otherwise = do
      r <- snapRectOrigin da rect
      setTexture da tex
      pushQuad da r u0 v0 u1 v1 col

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
      s <- readIORef (daSnapScale da)
      pushRoundedRectRaw da (Rect (snapToPixel s x) (snapToPixel s y) w h) radius col

-- | Unsnapped variant used when the rect is already anchored to the snapped
-- device pixel grid, e.g. a mark that must stay concentric with a border that
-- has already snapped its own origin. Re-snapping here would round the
-- off-origin inset (delta = (box - mark)/2) away, and since absolute snapping
-- rides on the fractional part of the widget position the mark would drift
-- off-center by up to a pixel as the widget scrolls.
{-# INLINE pushRoundedRectRaw #-}
pushRoundedRectRaw :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRectRaw da (Rect x y w h) radius col
  | w <= 0 || h <= 0 = pure ()
  | radius <= 0.5 = pushRect da (Rect x y w h) col
  | otherwise = do
      let !rad = min radius (min (w * 0.5) (h * 0.5))
      if rad <= 0.5
        then pushRect da (Rect x y w h) col
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
      s <- readIORef (daSnapScale da)
      let !px = snapToPixel s x
          !py = snapToPixel s y
      setTexture da glyphAtlasTextureId
      let !rad = min (max 0 radius) (min (w * 0.5) (h * 0.5))
          !ibw = min bw (min (w * 0.5) (h * 0.5))
      if rad <= 0.5
        then do
          let !t = ibw
              !ox = px + t / 2
              !oy = py + t / 2
              !ow = max 0 (w - t)
              !oh = max 0 (h - t)
          pushStrokeAARaw da ox oy (ox + ow) oy t col
          pushStrokeAARaw da ox (oy + oh) (ox + ow) (oy + oh) t col
          when (oh > 0) $ pushStrokeAARaw da ox oy ox (oy + oh) t col
          when (oh > 0) $ pushStrokeAARaw da (ox + ow) oy (ox + ow) (oy + oh) t col
        else do
          let !midW = max 0 (w - 2 * rad)
              !midH = max 0 (h - 2 * rad)
              !topY = py + ibw / 2
              !botY = py + h - ibw / 2
              !leftX = px + ibw / 2
              !rightX = px + w - ibw / 2
          when (midW > 0) $ do
            pushStrokeAARaw da (px + rad) topY (px + rad + midW) topY ibw col
            pushStrokeAARaw da (px + rad) botY (px + rad + midW) botY ibw col
          when (midH > 0) $ do
            pushStrokeAARaw da leftX (py + rad) leftX (py + rad + midH) ibw col
            pushStrokeAARaw da rightX (py + rad) rightX (py + rad + midH) ibw col
          let !cr = max 0.25 (rad - ibw / 2)
          if midW <= 0 && midH <= 0
            then forM_ [0 .. 3] $ \q -> pushCornerArcStroke da (px + w * 0.5) (py + h * 0.5) cr ibw q col
            else do
              pushCornerArcStroke da (px + rad) (py + rad) cr ibw 0 col
              pushCornerArcStroke da (px + w - rad) (py + rad) cr ibw 1 col
              pushCornerArcStroke da (px + w - rad) (py + h - rad) cr ibw 2 col
              pushCornerArcStroke da (px + rad) (py + h - rad) cr ibw 3 col

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
{-# INLINE pushStrokeAA #-}
pushStrokeAA :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushStrokeAA da x0 y0 x1 y1 bw col
  | bw <= 0 = pure ()
  | otherwise = do
      s <- readIORef (daSnapScale da)
      pushStrokeAARaw da (snapToPixel s x0) (snapToPixel s y0) (snapToPixel s x1) (snapToPixel s y1) bw col

-- | Unsnapped variant used by 'pushRoundedStroke', which already snapped the
-- border rect origin and computes a deliberate half-pixel inset for crisp
-- hairlines. Re-snapping here would round that inset away and make the border
-- drift between the card edge and its neighbour as the card scrolls.
pushStrokeAARaw :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushStrokeAARaw da x0 y0 x1 y1 bw col
  | bw <= 0 = pure ()
  | otherwise =
      case strokeAxes x0 y0 x1 y1 of
        Nothing -> pure ()
        Just (dx, dy, len) -> do
          setTexture da glyphAtlasTextureId
          let !nx = (-dy) / len
              !ny = dx / len
              !half = bw * 0.5
              !core = max 0 (half - 0.5)
              !outer = half + 0.5
              !(cr, cg, cb, ca) = unpackColorF col
          (vp, ip, base, baseIdx) <- ensureAndAlloc da 8 18
          let pokeEnd vi px py = do
                let ((p0x, p0y), (p1x, p1y), (p2x, p2y), (p3x, p3y)) =
                      strokeStripNormalsSIMD px py nx ny (-outer) (-core) core outer
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
          !core = max 0 (half - 0.5)
          !inner = max 0 (r - core)
          !outer = r + core
          !innerAA = max 0 (inner - 1.0)
          !outerAA = outer + 1.0
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
  | otherwise = do
      s <- readIORef (daSnapScale da)
      let !px1 = snapToPixel s x1
          !py1 = snapToPixel s y1
          !px2 = snapToPixel s x2
          !py2 = snapToPixel s y2
      case strokeAxes px1 py1 px2 py2 of
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
          poke vOff (px1 + hx) (py1 + hy)
          poke (vOff + 32) (px2 + hx) (py2 + hy)
          poke (vOff + 64) (px2 - hx) (py2 - hy)
          poke (vOff + 96) (px1 - hx) (py1 - hy)
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
    emitOne (FillRoundedRect r radius c) = pushRoundedRect da r radius c
    emitOne (FillTriangle x0 y0 x1 y1 x2 y2 c) = pushFilledTriangle da x0 y0 x1 y1 x2 y2 c
    emitOne (FillCircle cx cy radius c) =
      pushRoundedRect da (Rect (cx - radius) (cy - radius) (2 * radius) (2 * radius)) radius c
    emitOne (Stroke x0 y0 x1 y1 t c) = pushStroke da x0 y0 x1 y1 t c
    emitOne (StrokeRoundedRect r radius bw c) = pushRoundedStroke da r radius bw c
    emitOne (StrokeCircle cx cy radius bw c) =
      pushRoundedStroke da (Rect (cx - radius) (cy - radius) (2 * radius) (2 * radius)) radius bw c
    emitOne (StrokeLineAA x0 y0 x1 y1 bw c) = pushStrokeAA da x0 y0 x1 y1 bw c
    emitOne (FillQuadGradient r c0 c1 c2 c3) = pushQuadGradient da r c0 c1 c2 c3
    emitOne (DrawImageRect r tex u0 v0 u1 v1 c) = pushImage da r tex u0 v0 u1 v1 c
    emitOne (DrawText x y ax ay t c) = do
      let Rect px py _ _ = drawTextBox fm x y ax ay t
      pushText da fm px py t c

{-# INLINE pushFilledTriangle #-}
pushFilledTriangle :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushFilledTriangle da x0 y0 x1 y1 x2 y2 col = do
  s <- readIORef (daSnapScale da)
  setTexture da glyphAtlasTextureId
  (vp, ip, base, baseIdx) <- ensureAndAlloc da 3 3
  let !px0 = snapToPixel s x0
      !py0 = snapToPixel s y0
      !px1 = snapToPixel s x1
      !py1 = snapToPixel s y1
      !px2 = snapToPixel s x2
      !py2 = snapToPixel s y2
      !(r, g, b, a) = unpackColorF col
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !baseIdxWord = fromIntegral base :: Word32
  pokeVertex vp vOff px0 py0 r g b a whitePixelU whitePixelV
  pokeVertex vp (vOff + 32) px1 py1 r g b a whitePixelU whitePixelV
  pokeVertex vp (vOff + 64) px2 py2 r g b a whitePixelU whitePixelV
  pokeByteOff ip iOff baseIdxWord
  pokeByteOff ip (iOff + 4) (baseIdxWord + 1)
  pokeByteOff ip (iOff + 8) (baseIdxWord + 2)
  writeIORef (daVertexCount da) (base + 3)
  writeIORef (daIndexCount da) (baseIdx + 3)

{-# INLINE snapToPixel #-}
snapToPixel :: Float -> Float -> Float
snapToPixel s v = onGrid s v

{-# INLINE pushText #-}
pushText :: DrawArena -> FontMetrics -> Float -> Float -> T.Text -> Color -> IO ()
pushText _da _fm _x _y txt _col | T.null txt = pure ()
pushText da fm x y txt col =
  -- Snapping the pen to the device pixel grid keeps every glyph quad on a
  -- whole pixel. Advances, bearings, and ink sizes are all integer pixel
  -- counts divided by the snap scale, so snapping the origin alone aligns the
  -- whole line: otherwise fractional layout positions leave glyphs straddling
  -- pixel boundaries, which makes nearest-sampled atlas text blurry and jitter
  -- as scroll position changes.
  let !px = snapToPixel (fmSnapScale fm) x
      !py = snapToPixel (fmSnapScale fm) y
   in case fmRun fm txt of
        Just rq -> drawRunQuad da px py rq col
        Nothing -> go px py Nothing txt
  where
    go !ox !oy !prev !t =
      case T.uncons t of
        Nothing -> pure ()
        Just (c, rest) -> do
          let !adv = advanceAfter prev c
          case fmGlyph fm c of
            Nothing -> do
              when (adv > 0 && c /= ' ') $
                pushRect da (Rect ox oy adv (fmLineHeight fm)) col
              go (ox + adv) oy (Just c) rest
            Just gq -> do
              let !gx = ox + gqX gq
                  !gy = oy + gqY gq
                  !gw = gqW gq
                  !gh = gqH gq
              setTexture da glyphAtlasTextureId
              pushQuad da (Rect gx gy gw gh) (gqU0 gq) (gqV0 gq) (gqU1 gq) (gqV1 gq) col
              go (ox + adv) oy (Just c) rest
    advanceAfter prev c = fmAdvance fm c + maybe 0 (\p -> fmKerning fm p c) prev

drawRunQuad :: DrawArena -> Float -> Float -> RunQuad -> Color -> IO ()
drawRunQuad da x y rq col = do
  let !gx = x + rqX rq
      !gy = y + rqY rq
      !gw = rqW rq
      !gh = rqH rq
  setTexture da glyphAtlasTextureId
  pushQuad da (Rect gx gy gw gh) (rqU0 rq) (rqV0 rq) (rqU1 rq) (rqV1 rq) col

{-# INLINE pushTextStyled #-}
pushTextStyled ::
  DrawArena ->
  FontMetrics ->
  FontWeight ->
  FontStyle ->
  TextDecoration ->
  Float ->
  Float ->
  T.Text ->
  Color ->
  IO ()
pushTextStyled da fm weight fstyle deco x y txt col
  | weight == WeightNormal && fstyle == FontStyleNormal && deco == DecorationNone =
      pushText da fm x y txt col
  | otherwise = do
      let !px = snapToPixel (fmSnapScale fm) x
          !py = snapToPixel (fmSnapScale fm) y
          !lh = fmLineHeight fm
          !bOff = max 1.0 (0.05 * lh)
          !slantMult = case fstyle of
            FontStyleNormal  -> 0.0
            FontStyleItalic  -> 0.18
            FontStyleOblique -> 0.18

      case weight of
        WeightNormal -> go px py txt slantMult
        WeightLight  -> go px py txt slantMult
        WeightMedium -> do
          go px py txt slantMult
          go (px + 0.5 * bOff) py txt slantMult
        WeightSemiBold -> do
          go px py txt slantMult
          go (px + 0.75 * bOff) py txt slantMult
        WeightBold -> do
          go px py txt slantMult
          go (px + bOff) py txt slantMult
        WeightExtraBold -> do
          go px py txt slantMult
          go (px + bOff) py txt slantMult
          go (px + 1.5 * bOff) py txt slantMult
        WeightBlack -> do
          go px py txt slantMult
          go (px + bOff) py txt slantMult
          go (px + 1.5 * bOff) py txt slantMult
          go (px + 2.0 * bOff) py txt slantMult

      case deco of
        DecorationNone -> pure ()
        _ -> do
          let !textW = lineWidth fm txt
              !thick = max 1.0 (0.06 * lh)
          case deco of
            DecorationUnderline -> do
              let !uY = py + fmAscent fm + max 1.0 (0.1 * lh)
              pushRect da (Rect px uY textW thick) col
            DecorationStrikethrough -> do
              let !sY = py + fmAscent fm * 0.65
              pushRect da (Rect px sY textW thick) col
            DecorationUnderlineStrike -> do
              let !uY = py + fmAscent fm + max 1.0 (0.1 * lh)
                  !sY = py + fmAscent fm * 0.65
              pushRect da (Rect px uY textW thick) col
              pushRect da (Rect px sY textW thick) col
  where
    go !ox !oy !t !slantMult
      | slantMult == 0.0 && weight == WeightNormal = pushText da fm ox oy t col
      | slantMult == 0.0 = goNormal ox oy Nothing t
      | otherwise = goSlantedPrev ox oy Nothing t slantMult

    advanceAfter prev c = fmAdvance fm c + maybe 0 (\p -> fmKerning fm p c) prev

    goNormal !ox !oy !prev !t =
      case T.uncons t of
        Nothing -> pure ()
        Just (c, rest) -> do
          let !adv = advanceAfter prev c
          case fmGlyph fm c of
            Nothing -> do
              when (adv > 0 && c /= ' ') $
                pushRect da (Rect ox oy adv (fmLineHeight fm)) col
              goNormal (ox + adv) oy (Just c) rest
            Just gq -> do
              let !gx = ox + gqX gq
                  !gy = oy + gqY gq
                  !gw = gqW gq
                  !gh = gqH gq
              setTexture da glyphAtlasTextureId
              pushQuad da (Rect gx gy gw gh) (gqU0 gq) (gqV0 gq) (gqU1 gq) (gqV1 gq) col
              goNormal (ox + adv) oy (Just c) rest

    goSlantedPrev !ox !oy !prev !t !slantMult =
      case T.uncons t of
        Nothing -> pure ()
        Just (c, rest) -> do
          let !adv = advanceAfter prev c
          case fmGlyph fm c of
            Nothing -> do
              when (adv > 0 && c /= ' ') $
                pushRect da (Rect ox oy adv (fmLineHeight fm)) col
              goSlantedPrev (ox + adv) oy (Just c) rest slantMult
            Just gq -> do
              let !gx = ox + gqX gq
                  !gy = oy + gqY gq
                  !gw = gqW gq
                  !gh = gqH gq
              setTexture da glyphAtlasTextureId
              (vp, ip, base, baseIdx) <- ensureAndAlloc da 4 6
              let !(r, g, b, a) = unpackColorF col
                  !vOff = base * vertexSize
                  !iOff = baseIdx * indexSize
                  !baseIdxWord = fromIntegral base :: Word32
                  -- Synthetic oblique shears around the shared baseline, not
                  -- each glyph's ink box: every glyph gets the same slant so
                  -- stems stay parallel, and descenders lean left below it.
                  !baselineY = oy + fmAscent fm
                  !topDx = slantMult * (baselineY - gy)
                  !botDx = slantMult * (baselineY - (gy + gh))
                  !x0 = gx + topDx
                  !x1 = gx + gw + topDx
                  !x2 = gx + gw + botDx
                  !x3 = gx + botDx
                  !y0 = gy
                  !y1 = gy + gh
              pokeVertex vp vOff x0 y0 r g b a (gqU0 gq) (gqV0 gq)
              pokeVertex vp (vOff + 32) x1 y0 r g b a (gqU1 gq) (gqV0 gq)
              pokeVertex vp (vOff + 64) x2 y1 r g b a (gqU1 gq) (gqV1 gq)
              pokeVertex vp (vOff + 96) x3 y1 r g b a (gqU0 gq) (gqV1 gq)
              pokeQuadIndices ip iOff baseIdxWord (baseIdxWord + 1) (baseIdxWord + 2) (baseIdxWord + 3)
              writeIORef (daVertexCount da) (base + 4)
              writeIORef (daIndexCount da) (baseIdx + 6)
              goSlantedPrev (ox + adv) oy (Just c) rest slantMult

{-# INLINE drawCmdCount #-}
drawCmdCount :: DrawData -> Int
drawCmdCount dd = sizeofPrimArray (drawCommands dd)

{-# INLINE drawCmdNull #-}
drawCmdNull :: DrawData -> Bool
drawCmdNull dd = sizeofPrimArray (drawCommands dd) == 0

{-# INLINE foldDrawCmds #-}
foldDrawCmds :: (a -> DrawCmd -> a) -> a -> DrawData -> a
foldDrawCmds f z dd =
  let cmds = drawCommands dd
      n = sizeofPrimArray cmds
      go !i !acc
        | i >= n = acc
        | otherwise = go (i + 1) (f acc (indexPrimArray cmds i))
   in go 0 z

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
