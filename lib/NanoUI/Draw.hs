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
  , pushImage
  , pushRoundedRect
  , pushText
  , pushLine
  , pushFilledTriangle
  , finishDraw
  , vertexSize
  , indexSize
  , withClip
  ) where

import Control.Monad (forM_, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.Array (MutableArray, newArray, readArray, writeArray)
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (RealWorld)
import NanoUI.Font (FontMetrics (..), GlyphQuad (..))
import NanoUI.Types (Color, Rect (..), colorToWord32, rectIntersect)
import qualified Data.Text as T

data Layer = LayerBackground | LayerContent | LayerOverlay
  deriving (Eq, Show, Enum, Bounded)

data Vertex = Vertex
  { vtxX :: {-# UNPACK #-} !Float
  , vtxY :: {-# UNPACK #-} !Float
  , vtxU :: {-# UNPACK #-} !Float
  , vtxV :: {-# UNPACK #-} !Float
  , vtxRgba :: {-# UNPACK #-} !Word32
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
  , drawCommands :: [DrawCmd]
  }
  deriving (Eq, Show)

type BufferPool = IORef [(ForeignPtr Word8, Int)]

data DrawArena = DrawArena
  { daVertices :: IORef (ForeignPtr Word8, Int)
  , daVertexCount :: IORef Int
  , daVertexPool :: BufferPool
  , daIndices :: IORef (ForeignPtr Word8, Int)
  , daIndexCount :: IORef Int
  , daIndexPool :: BufferPool
  , daCmdStore :: IORef (MutableArray RealWorld DrawCmd)
  , daCmdCount :: IORef Int
  , daCmdCapacity :: IORef Int
  , daCurrentLayer :: IORef Layer
  , daCurrentClip :: IORef (Float, Float, Float, Float)
  , daCurrentTexture :: IORef Int
  , daCmdStartIndex :: IORef Int
  }

vertexCapacity :: Int
vertexCapacity = 4096

indexCapacity :: Int
indexCapacity = 8192

vertexSize :: Int
vertexSize = 20

indexSize :: Int
indexSize = 4

bufferPoolLimit :: Int
bufferPoolLimit = 4

cmdInitialCapacity :: Int
cmdInitialCapacity = 64

{-# INLINE pokeVertex #-}
pokeVertex :: Ptr Word8 -> Int -> Float -> Float -> Float -> Float -> Word32 -> IO ()
pokeVertex p i x y u v rgba = do
  let off = i * vertexSize
  pokeByteOff p off x
  pokeByteOff p (off + 4) y
  pokeByteOff p (off + 8) u
  pokeByteOff p (off + 12) v
  pokeByteOff p (off + 16) rgba

{-# INLINE pokeIndex #-}
pokeIndex :: Ptr Word8 -> Int -> Int -> IO ()
pokeIndex p i idx =
  pokeByteOff p (i * indexSize) (fromIntegral idx :: Word32)

{-# INLINE pokeQuadIndices #-}
pokeQuadIndices :: Ptr Word8 -> Int -> Int -> IO ()
pokeQuadIndices p baseIdx base = do
  pokeIndex p baseIdx base
  pokeIndex p (baseIdx + 1) (base + 1)
  pokeIndex p (baseIdx + 2) (base + 2)
  pokeIndex p (baseIdx + 3) base
  pokeIndex p (baseIdx + 4) (base + 2)
  pokeIndex p (baseIdx + 5) (base + 3)

{-# INLINE newDrawArena #-}
newDrawArena :: IO DrawArena
newDrawArena = do
  vPtr <- mallocForeignPtrBytes (vertexCapacity * vertexSize)
  iPtr <- mallocForeignPtrBytes (indexCapacity * indexSize)
  daVertices <- newIORef (vPtr, vertexCapacity)
  daVertexCount <- newIORef 0
  daVertexPool <- newIORef []
  daIndices <- newIORef (iPtr, indexCapacity)
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
      { daVertices
      , daVertexCount
      , daVertexPool
      , daIndices
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

{-# NOINLINE growBufferWithCount #-}
growBufferWithCount ::
  Int ->
  IORef (ForeignPtr Word8, Int) ->
  BufferPool ->
  Int ->
  Int ->
  IO ()
growBufferWithCount count bufRef pool elemBytes needElems = do
  (ptr, cap) <- readIORef bufRef
  let required = count + needElems
  if required <= cap
    then pure ()
    else do
      let newCap = max (cap * 2) required
          newBytes = newCap * elemBytes
      newPtr <- poolTake pool newBytes newCap
      withForeignPtr ptr $ \oldP ->
        withForeignPtr newPtr $ \newP ->
          copyArray newP oldP (count * elemBytes)
      poolGive pool ptr cap
      writeIORef bufRef (newPtr, newCap)

ensureVerts :: DrawArena -> Int -> IO ()
ensureVerts da needVerts = do
  count <- readIORef (daVertexCount da)
  growBufferWithCount count (daVertices da) (daVertexPool da) vertexSize needVerts

ensureIndices :: DrawArena -> Int -> IO ()
ensureIndices da needIndices = do
  count <- readIORef (daIndexCount da)
  growBufferWithCount count (daIndices da) (daIndexPool da) indexSize needIndices

{-# NOINLINE growCmdStore #-}
growCmdStore :: DrawArena -> Int -> IO ()
growCmdStore da oldCap = do
  let newCap = oldCap * 2
  arr <- readIORef (daCmdStore da)
  newArr <- newArray newCap (DrawCmd 0 0 0 0 0 0 0 LayerContent)
  forM_ [0 .. oldCap - 1] $ \i ->
    readArray arr i >>= writeArray newArr i
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
      appendCmd
        da
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
      writeIORef (daCmdStartIndex da) curIdx
    else pure ()

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

{-# INLINE pushQuad #-}
pushQuad :: DrawArena -> Rect -> Float -> Float -> Float -> Float -> Color -> IO ()
pushQuad da (Rect x y w h) u0 v0 u1 v1 col = do
  ensureVerts da 4
  ensureIndices da 6
  base <- readIORef (daVertexCount da)
  baseIdx <- readIORef (daIndexCount da)
  (vPtr, _) <- readIORef (daVertices da)
  (iPtr, _) <- readIORef (daIndices da)
  let rgba = colorToWord32 col
  withForeignPtr vPtr $ \vp -> do
    pokeVertex vp base x y u0 v0 rgba
    pokeVertex vp (base + 1) (x + w) y u1 v0 rgba
    pokeVertex vp (base + 2) (x + w) (y + h) u1 v1 rgba
    pokeVertex vp (base + 3) x (y + h) u0 v1 rgba
  withForeignPtr iPtr $ \ip ->
    pokeQuadIndices ip baseIdx base
  writeIORef (daVertexCount da) (base + 4)
  writeIORef (daIndexCount da) (baseIdx + 6)

{-# INLINE pushRect #-}
pushRect :: DrawArena -> Rect -> Color -> IO ()
pushRect da rect col = do
  setTexture da 0
  pushQuad da rect 0 0 1 1 col

{-# INLINE pushImage #-}
pushImage :: DrawArena -> Rect -> Int -> Float -> Float -> Float -> Float -> Color -> IO ()
pushImage da rect tex u0 v0 u1 v1 col
  | tex <= 0 = pushRect da rect col
  | otherwise = do
      setTexture da tex
      pushQuad da rect u0 v0 u1 v1 col

-- Rounded fills encode radius in vtxU and use vtxV = -1 (plain rects use v in [0, 1]).
{-# INLINE pushRoundedRect #-}
pushRoundedRect :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRect da (Rect x y w h) radius col = do
  setTexture da 0
  ensureVerts da 4
  ensureIndices da 6
  base <- readIORef (daVertexCount da)
  baseIdx <- readIORef (daIndexCount da)
  (vPtr, _) <- readIORef (daVertices da)
  (iPtr, _) <- readIORef (daIndices da)
  let rgba = colorToWord32 col
      r = max 0 radius
  withForeignPtr vPtr $ \vp -> do
    pokeVertex vp base x y r (-1) rgba
    pokeVertex vp (base + 1) (x + w) y r (-1) rgba
    pokeVertex vp (base + 2) (x + w) (y + h) r (-1) rgba
    pokeVertex vp (base + 3) x (y + h) r (-1) rgba
  withForeignPtr iPtr $ \ip ->
    pokeQuadIndices ip baseIdx base
  writeIORef (daVertexCount da) (base + 4)
  writeIORef (daIndexCount da) (baseIdx + 6)

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
  ensureVerts da 3
  ensureIndices da 3
  base <- readIORef (daVertexCount da)
  baseIdx <- readIORef (daIndexCount da)
  (vPtr, _) <- readIORef (daVertices da)
  (iPtr, _) <- readIORef (daIndices da)
  let rgba = colorToWord32 col
  withForeignPtr vPtr $ \vp -> do
    pokeVertex vp base x0 y0 (-3) 0 rgba
    pokeVertex vp (base + 1) x1 y1 (-3) 0 rgba
    pokeVertex vp (base + 2) x2 y2 (-3) 0 rgba
  withForeignPtr iPtr $ \ip -> do
    pokeIndex ip baseIdx base
    pokeIndex ip (baseIdx + 1) (base + 1)
    pokeIndex ip (baseIdx + 2) (base + 2)
  writeIORef (daVertexCount da) (base + 3)
  writeIORef (daIndexCount da) (baseIdx + 3)

{-# INLINE pushText #-}
pushText :: DrawArena -> FontMetrics -> Float -> Float -> T.Text -> Color -> IO ()
pushText da fm x y txt col = go x (T.unpack txt)
  where
    adv = fmAdvance fm ' '
    go _ [] = pure ()
    go ox (c : rest) =
      case fmGlyph fm c of
        Nothing -> go (ox + adv) rest
        Just gq -> do
          let gx = ox + gqX gq
              gy = y + gqY gq
              gw = gqW gq
              gh = gqH gq
          pushRect da (Rect gx gy gw gh) col
          go (ox + adv) rest

readCmdList :: MutableArray RealWorld DrawCmd -> Int -> IO [DrawCmd]
readCmdList arr count = reverse <$> go 0 []
  where
    go i acc
      | i >= count = pure acc
      | otherwise = readArray arr i >>= \cmd -> go (i + 1) (cmd : acc)

{-# INLINE finishDraw #-}
finishDraw :: DrawArena -> IO DrawData
finishDraw da = do
  flushCmd da
  (vPtr, _) <- readIORef (daVertices da)
  (iPtr, _) <- readIORef (daIndices da)
  vCount <- readIORef (daVertexCount da)
  iCount <- readIORef (daIndexCount da)
  count <- readIORef (daCmdCount da)
  arr <- readIORef (daCmdStore da)
  cmds <- readCmdList arr count
  pure
    DrawData
      { drawVertices = vPtr
      , drawVertexCount = vCount
      , drawIndices = iPtr
      , drawIndexCount = iCount
      , drawCommands = cmds
      }
