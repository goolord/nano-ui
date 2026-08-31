{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnliftedFFITypes #-}

module NanoUI.Draw
  ( Layer (..)
  , DrawCmd (..)
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
  , pushFilledTriangle
  , finishDraw
  , drawCmdCount
  , drawCmdNull
  , drawCmdAt
  , foldDrawCmds
  , drawCmdFilter
  , drawCmdForLayer
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
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, readArray, writeArray)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (RealWorld)
import NanoUI.Font (FontMetrics (..), GlyphQuad (..))
import NanoUI.Types (Color (..), Rect (..), rectIntersect)
import qualified Data.Text as T

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

data DrawCmd = DrawCmd
  { cmdClipX :: {-# UNPACK #-} !Float
  , cmdClipY :: {-# UNPACK #-} !Float
  , cmdClipW :: {-# UNPACK #-} !Float
  , cmdClipH :: {-# UNPACK #-} !Float
  , cmdTextureId :: !Int
  , cmdIndexOffset :: {-# UNPACK #-} !Word32
  , cmdIndexCount :: {-# UNPACK #-} !Word32
  , cmdLayer :: !Layer
  }
  deriving (Eq, Show)

data DrawData = DrawData
  { drawVertices :: ForeignPtr Word8
  , drawVertexCount :: Int
  , drawIndices :: ForeignPtr Word8
  , drawIndexCount :: Int
  , drawCommands :: Vector DrawCmd
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
  , daCmdStore :: !(IORef (MutableArray RealWorld DrawCmd))
  , daCmdCount :: !(IORef Int)
  , daCmdCapacity :: !(IORef Int)
  , daCurrentLayer :: !(IORef Layer)
  , daCurrentClip :: !(IORef (Float, Float, Float, Float))
  , daCurrentTexture :: !(IORef Int)
  , daCmdStartIndex :: !(IORef Int)
  }

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
  cmdStore <- newArray cmdInitialCapacity (DrawCmd 0 0 0 0 0 0 0 LayerContent)
  daCmdStore <- newIORef cmdStore
  daCmdCount <- newIORef 0
  daCmdCapacity <- newIORef cmdInitialCapacity
  daCurrentLayer <- newIORef LayerContent
  daCurrentClip <- newIORef (0, 0, 1e9, 1e9)
  daCurrentTexture <- newIORef 0
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
  writeIORef (daCurrentTexture da) 0
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
  newArr <- newArray newCap (DrawCmd 0 0 0 0 0 0 0 LayerContent)
  copyMutableArray newArr 0 arr 0 oldCap
  writeIORef (daCmdStore da) newArr
  writeIORef (daCmdCapacity da) newCap

{-# INLINE appendCmd #-}
appendCmd :: DrawArena -> DrawCmd -> IO ()
appendCmd da cmd = do
  count <- readIORef (daCmdCount da)
  cap <- readIORef (daCmdCapacity da)
  when (count >= cap) $ growCmdStore da cap
  arr <- readIORef (daCmdStore da)
  writeArray arr count cmd
  writeIORef (daCmdCount da) (count + 1)

{-# INLINE currentLayer #-}
currentLayer :: DrawArena -> IO Layer
currentLayer = readIORef . daCurrentLayer

{-# INLINE beginLayer #-}
beginLayer :: DrawArena -> Layer -> IO ()
beginLayer da layer = do
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
            prev <- readArray arr (cmdCount - 1)
            if sameDrawBatch prev newCmd
              then do
                writeArray
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

{-# INLINE pushQuad #-}
pushQuad :: DrawArena -> Rect -> Float -> Float -> Float -> Float -> Color -> IO ()
pushQuad da (Rect x y w h) u0 v0 u1 v1 col = do
  (vp, ip, base, baseIdx) <- ensureAndAlloc da 4 6
  let !(r, g, b, a) = unpackColorF col
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !baseIdxWord = fromIntegral base :: Word32
      !x1 = x + w
      !y1 = y + h
  pokeByteOff vp vOff x
  pokeByteOff vp (vOff + 4) y
  pokeByteOff vp (vOff + 8) r
  pokeByteOff vp (vOff + 12) g
  pokeByteOff vp (vOff + 16) b
  pokeByteOff vp (vOff + 20) a
  pokeByteOff vp (vOff + 24) u0
  pokeByteOff vp (vOff + 28) v0

  pokeByteOff vp (vOff + 32) x1
  pokeByteOff vp (vOff + 36) y
  pokeByteOff vp (vOff + 40) r
  pokeByteOff vp (vOff + 44) g
  pokeByteOff vp (vOff + 48) b
  pokeByteOff vp (vOff + 52) a
  pokeByteOff vp (vOff + 56) u1
  pokeByteOff vp (vOff + 60) v0

  pokeByteOff vp (vOff + 64) x1
  pokeByteOff vp (vOff + 68) y1
  pokeByteOff vp (vOff + 72) r
  pokeByteOff vp (vOff + 76) g
  pokeByteOff vp (vOff + 80) b
  pokeByteOff vp (vOff + 84) a
  pokeByteOff vp (vOff + 88) u1
  pokeByteOff vp (vOff + 92) v1

  pokeByteOff vp (vOff + 96) x
  pokeByteOff vp (vOff + 100) y1
  pokeByteOff vp (vOff + 104) r
  pokeByteOff vp (vOff + 108) g
  pokeByteOff vp (vOff + 112) b
  pokeByteOff vp (vOff + 116) a
  pokeByteOff vp (vOff + 120) u0
  pokeByteOff vp (vOff + 124) v1

  pokeByteOff ip iOff baseIdxWord
  pokeByteOff ip (iOff + 4) (baseIdxWord + 1)
  pokeByteOff ip (iOff + 8) (baseIdxWord + 2)
  pokeByteOff ip (iOff + 12) baseIdxWord
  pokeByteOff ip (iOff + 16) (baseIdxWord + 2)
  pokeByteOff ip (iOff + 20) (baseIdxWord + 3)
  writeIORef (daVertexCount da) (base + 4)
  writeIORef (daIndexCount da) (baseIdx + 6)

-- Quad with a color per corner. GPU interpolates across the two triangles.
-- Corners: top-left, top-right, bottom-right, bottom-left.
{-# INLINE pushQuadGradient #-}
pushQuadGradient :: DrawArena -> Rect -> Color -> Color -> Color -> Color -> IO ()
pushQuadGradient da (Rect x y w h) tl tr br bl
  | w <= 0 || h <= 0 = pure ()
  | otherwise = do
      setTexture da 0
      (vp, ip, base, baseIdx) <- ensureAndAlloc da 4 6
      let !vOff = base * vertexSize
          !iOff = baseIdx * indexSize
          !baseIdxWord = fromIntegral base :: Word32
          !x1 = x + w
          !y1 = y + h
          pokeVert off px py col = do
            let !(r, g, b, a) = unpackColorF col
            pokeByteOff vp off px
            pokeByteOff vp (off + 4) py
            pokeByteOff vp (off + 8) r
            pokeByteOff vp (off + 12) g
            pokeByteOff vp (off + 16) b
            pokeByteOff vp (off + 20) a
            pokeByteOff vp (off + 24) (0 :: Float)
            pokeByteOff vp (off + 28) (0 :: Float)
      pokeVert vOff x y tl
      pokeVert (vOff + 32) x1 y tr
      pokeVert (vOff + 64) x1 y1 br
      pokeVert (vOff + 96) x y1 bl
      pokeByteOff ip iOff baseIdxWord
      pokeByteOff ip (iOff + 4) (baseIdxWord + 1)
      pokeByteOff ip (iOff + 8) (baseIdxWord + 2)
      pokeByteOff ip (iOff + 12) baseIdxWord
      pokeByteOff ip (iOff + 16) (baseIdxWord + 2)
      pokeByteOff ip (iOff + 20) (baseIdxWord + 3)
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

{-# INLINE pushRect #-}
pushRect :: DrawArena -> Rect -> Color -> IO ()
pushRect da rect col = do
  setTexture da 0
  pushQuad da rect 0 0 1 1 col

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
  let !segs = cornerSegments
      !needV = segs + 2
      !needI = segs * 3
      !q = cornerQuadrant a0
  (vp, ip, base, baseIdx) <- ensureAndAlloc da needV needI
  let !(r, g, b, a) = unpackColorF col
      !centerOff = base * vertexSize
      !centerIdx = fromIntegral base :: Word32
  pokeByteOff vp centerOff cx
  pokeByteOff vp (centerOff + 4) cy
  pokeByteOff vp (centerOff + 8) r
  pokeByteOff vp (centerOff + 12) g
  pokeByteOff vp (centerOff + 16) b
  pokeByteOff vp (centerOff + 20) a
  pokeByteOff vp (centerOff + 24) (0 :: Float)
  pokeByteOff vp (centerOff + 28) (0 :: Float)

  forM_ [0 .. segs] $ \i -> do
    let !(ca, sa) = cornerCosSin q i
        !vx = cx + rad * ca
        !vy = cy + rad * sa
        !vOff = (base + 1 + i) * vertexSize
    pokeByteOff vp vOff vx
    pokeByteOff vp (vOff + 4) vy
    pokeByteOff vp (vOff + 8) r
    pokeByteOff vp (vOff + 12) g
    pokeByteOff vp (vOff + 16) b
    pokeByteOff vp (vOff + 20) a
    pokeByteOff vp (vOff + 24) (0 :: Float)
    pokeByteOff vp (vOff + 28) (0 :: Float)
    when (i > 0) $ do
      let !rim0 = fromIntegral (base + i) :: Word32
          !rim1 = fromIntegral (base + 1 + i) :: Word32
          !iOff = (baseIdx + (i - 1) * 3) * indexSize
      pokeByteOff ip iOff centerIdx
      pokeByteOff ip (iOff + 4) rim0
      pokeByteOff ip (iOff + 8) rim1

  writeIORef (daVertexCount da) (base + needV)
  writeIORef (daIndexCount da) (baseIdx + needI)

pushCornerArc :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushCornerArc da cx cy outerR innerR a0 a1 col
  | innerR <= 0.001 = pushCornerFan da cx cy outerR a0 a1 col
  | otherwise = do
      let !segs = cornerSegments
          !needV = (segs + 1) * 2
          !needI = segs * 6
          !q = cornerQuadrant a0
      (vp, ip, base, baseIdx) <- ensureAndAlloc da needV needI
      let !(r, g, b, a) = unpackColorF col
      forM_ [0 .. segs] $ \i -> do
        let !(ca, sa) = cornerCosSin q i
            !ox = cx + outerR * ca
            !oy = cy + outerR * sa
            !ix = cx + innerR * ca
            !iy = cy + innerR * sa
            !oOff = (base + i * 2) * vertexSize
            !iOffV = (base + i * 2 + 1) * vertexSize
        pokeByteOff vp oOff ox
        pokeByteOff vp (oOff + 4) oy
        pokeByteOff vp (oOff + 8) r
        pokeByteOff vp (oOff + 12) g
        pokeByteOff vp (oOff + 16) b
        pokeByteOff vp (oOff + 20) a
        pokeByteOff vp (oOff + 24) (0 :: Float)
        pokeByteOff vp (oOff + 28) (0 :: Float)

        pokeByteOff vp iOffV ix
        pokeByteOff vp (iOffV + 4) iy
        pokeByteOff vp (iOffV + 8) r
        pokeByteOff vp (iOffV + 12) g
        pokeByteOff vp (iOffV + 16) b
        pokeByteOff vp (iOffV + 20) a
        pokeByteOff vp (iOffV + 24) (0 :: Float)
        pokeByteOff vp (iOffV + 28) (0 :: Float)

        when (i > 0) $ do
          let !o0 = fromIntegral (base + (i - 1) * 2) :: Word32
              !i0 = fromIntegral (base + (i - 1) * 2 + 1) :: Word32
              !o1 = fromIntegral (base + i * 2) :: Word32
              !i1 = fromIntegral (base + i * 2 + 1) :: Word32
              !idxOff = (baseIdx + (i - 1) * 6) * indexSize
          pokeByteOff ip idxOff o0
          pokeByteOff ip (idxOff + 4) i0
          pokeByteOff ip (idxOff + 8) i1
          pokeByteOff ip (idxOff + 12) o0
          pokeByteOff ip (idxOff + 16) i1
          pokeByteOff ip (idxOff + 20) o1

      writeIORef (daVertexCount da) (base + needV)
      writeIORef (daIndexCount da) (baseIdx + needI)

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
          setTexture da 0
          let !midW = max 0 (w - 2 * rad)
              !midH = max 0 (h - 2 * rad)
          when (midW > 0 && midH > 0) $
            pushQuad da (Rect (x + rad) (y + rad) midW midH) 0 0 1 1 col
          when (midW > 0) $ do
            pushQuad da (Rect (x + rad) y midW rad) 0 0 1 1 col
            pushQuad da (Rect (x + rad) (y + h - rad) midW rad) 0 0 1 1 col
          when (midH > 0) $ do
            pushQuad da (Rect x (y + rad) rad midH) 0 0 1 1 col
            pushQuad da (Rect (x + w - rad) (y + rad) rad midH) 0 0 1 1 col
          pushCornerFan da (x + rad) (y + rad) rad pi (pi * 1.5) col
          pushCornerFan da (x + w - rad) (y + rad) rad (pi * 1.5) (pi * 2) col
          pushCornerFan da (x + w - rad) (y + h - rad) rad 0 (pi * 0.5) col
          pushCornerFan da (x + rad) (y + h - rad) rad (pi * 0.5) pi col

{-# INLINE pushRoundedStroke #-}
pushRoundedStroke :: DrawArena -> Rect -> Float -> Float -> Color -> IO ()
pushRoundedStroke da (Rect x y w h) radius bw col
  | w <= 0 || h <= 0 || bw <= 0 = pure ()
  | otherwise = do
      setTexture da 0
      let !rad = min (max 0 radius) (min (w * 0.5) (h * 0.5))
          !ibw = min bw (min (w * 0.5) (h * 0.5))
      if rad <= 0.5
        then do
          let !midH = max 0 (h - 2 * ibw)
          pushQuad da (Rect x y w ibw) 0 0 1 1 col
          pushQuad da (Rect x (y + h - ibw) w ibw) 0 0 1 1 col
          when (midH > 0) $ do
            pushQuad da (Rect x (y + ibw) ibw midH) 0 0 1 1 col
            pushQuad da (Rect (x + w - ibw) (y + ibw) ibw midH) 0 0 1 1 col
        else do
          let !midW = max 0 (w - 2 * rad)
              !midH = max 0 (h - 2 * rad)
              !innerR = max 0 (rad - ibw)
          when (midW > 0) $ do
            pushQuad da (Rect (x + rad) y midW ibw) 0 0 1 1 col
            pushQuad da (Rect (x + rad) (y + h - ibw) midW ibw) 0 0 1 1 col
          when (midH > 0) $ do
            pushQuad da (Rect x (y + rad) ibw midH) 0 0 1 1 col
            pushQuad da (Rect (x + w - ibw) (y + rad) ibw midH) 0 0 1 1 col
          pushCornerArc da (x + rad) (y + rad) rad innerR pi (pi * 1.5) col
          pushCornerArc da (x + w - rad) (y + rad) rad innerR (pi * 1.5) (pi * 2) col
          pushCornerArc da (x + w - rad) (y + h - rad) rad innerR 0 (pi * 0.5) col
          pushCornerArc da (x + rad) (y + h - rad) rad innerR (pi * 0.5) pi col

{-# INLINE pushLine #-}
pushLine :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushLine da x1 y1 x2 y2 thickness col = do
  let dx = x2 - x1
      dy = y2 - y1
      len = sqrt (dx * dx + dy * dy)
  if len < 0.001
    then pure ()
    else do
      setTexture da 0
      let r = thickness / 2
          step = max 0.5 (r * 0.65)
          n = max (1 :: Int) (ceiling (len / step))
      forM_ [0 .. n] $ \i -> do
        let u = fromIntegral i / fromIntegral n
            cx = x1 + dx * u
            cy = y1 + dy * u
        pushRoundedRect da (Rect (cx - r) (cy - r) thickness thickness) r col

{-# INLINE pushFilledTriangle #-}
pushFilledTriangle :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushFilledTriangle da x0 y0 x1 y1 x2 y2 col = do
  setTexture da 0
  (vp, ip, base, baseIdx) <- ensureAndAlloc da 3 3
  let !(r, g, b, a) = unpackColorF col
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !baseIdxWord = fromIntegral base :: Word32
  pokeByteOff vp vOff x0
  pokeByteOff vp (vOff + 4) y0
  pokeByteOff vp (vOff + 8) r
  pokeByteOff vp (vOff + 12) g
  pokeByteOff vp (vOff + 16) b
  pokeByteOff vp (vOff + 20) a
  pokeByteOff vp (vOff + 24) (-3 :: Float)
  pokeByteOff vp (vOff + 28) (0 :: Float)

  pokeByteOff vp (vOff + 32) x1
  pokeByteOff vp (vOff + 36) y1
  pokeByteOff vp (vOff + 40) r
  pokeByteOff vp (vOff + 44) g
  pokeByteOff vp (vOff + 48) b
  pokeByteOff vp (vOff + 52) a
  pokeByteOff vp (vOff + 56) (-3 :: Float)
  pokeByteOff vp (vOff + 60) (0 :: Float)

  pokeByteOff vp (vOff + 64) x2
  pokeByteOff vp (vOff + 68) y2
  pokeByteOff vp (vOff + 72) r
  pokeByteOff vp (vOff + 76) g
  pokeByteOff vp (vOff + 80) b
  pokeByteOff vp (vOff + 84) a
  pokeByteOff vp (vOff + 88) (-3 :: Float)
  pokeByteOff vp (vOff + 92) (0 :: Float)

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

readCmdVector :: MutableArray RealWorld DrawCmd -> Int -> IO (Vector DrawCmd)
readCmdVector arr count = V.generateM count (readArray arr)

{-# INLINE drawCmdCount #-}
drawCmdCount :: DrawData -> Int
drawCmdCount dd = V.length (drawCommands dd)

{-# INLINE drawCmdNull #-}
drawCmdNull :: DrawData -> Bool
drawCmdNull dd = V.null (drawCommands dd)

{-# INLINE drawCmdAt #-}
drawCmdAt :: DrawData -> Int -> DrawCmd
drawCmdAt dd i = drawCommands dd V.! i

{-# INLINE foldDrawCmds #-}
foldDrawCmds :: (a -> DrawCmd -> a) -> a -> DrawData -> a
foldDrawCmds f z dd = V.foldl f z (drawCommands dd)

{-# INLINE drawCmdFilter #-}
drawCmdFilter :: (DrawCmd -> Bool) -> DrawData -> Vector DrawCmd
drawCmdFilter p dd = V.filter p (drawCommands dd)

{-# INLINE drawCmdForLayer #-}
drawCmdForLayer :: Layer -> DrawData -> Vector DrawCmd
drawCmdForLayer ly dd = drawCmdFilter ((== ly) . cmdLayer) dd

drawCmdElemsForLayer :: Layer -> DrawData -> [DrawCmd]
drawCmdElemsForLayer ly dd = V.toList (drawCmdForLayer ly dd)

-- | One pass over draw commands, bucketed by layer in paint order.
drawCmdPartitionByLayer :: DrawData -> ([DrawCmd], [DrawCmd], [DrawCmd], [DrawCmd])
drawCmdPartitionByLayer dd =
  let (bg, ct, ov, ch) = foldDrawCmds go ([], [], [], []) dd
   in (reverse bg, reverse ct, reverse ov, reverse ch)
  where
    go (bg, ct, ov, ch) cmd =
      case cmdLayer cmd of
        LayerBackground -> (cmd : bg, ct, ov, ch)
        LayerContent -> (bg, cmd : ct, ov, ch)
        LayerOverlay -> (bg, ct, cmd : ov, ch)
        LayerChrome -> (bg, ct, ov, cmd : ch)

drawCmdElems :: DrawData -> [DrawCmd]
drawCmdElems dd = V.toList (drawCommands dd)

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
  cmds <- readCmdVector arr count
  pure
    DrawData
      { drawVertices = vFPtr
      , drawVertexCount = vCount
      , drawIndices = iFPtr
      , drawIndexCount = iCount
      , drawCommands = cmds
      }
