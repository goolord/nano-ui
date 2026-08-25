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
  , pushRoundedRect
  , pushText
  , pushLine
  , finishDraw
  , vertexSize
  , indexSize
  , withClip
  ) where

import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Array (copyArray)
import Foreign.Storable (pokeByteOff)
import NanoUI.Font (FontMetrics (..), GlyphQuad (..))
import NanoUI.Types (Color, Rect (..), colorToWord32)
import qualified Data.Text as T

data Layer = LayerBackground | LayerContent | LayerOverlay
  deriving (Eq, Show, Enum, Bounded)

data Vertex = Vertex
  { vtxX :: Float
  , vtxY :: Float
  , vtxU :: Float
  , vtxV :: Float
  , vtxRgba :: Word32
  }
  deriving (Eq, Show)

data DrawCmd = DrawCmd
  { cmdClipX :: Float
  , cmdClipY :: Float
  , cmdClipW :: Float
  , cmdClipH :: Float
  , cmdTextureId :: Int
  , cmdIndexOffset :: Word32
  , cmdIndexCount :: Word32
  , cmdLayer :: Layer
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
  , daCommands :: IORef [DrawCmd]
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
  daCommands <- newIORef []
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
      , daCommands
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
  writeIORef (daCommands da) []
  writeIORef (daCurrentLayer da) LayerContent
  writeIORef (daCurrentClip da) (0, 0, 1e9, 1e9)
  writeIORef (daCurrentTexture da) 0
  writeIORef (daCmdStartIndex da) 0

poolTake :: BufferPool -> Int -> Int -> IO (ForeignPtr Word8)
poolTake pool bytes minCap = do
  entries <- readIORef pool
  case break (\(_, cap) -> cap >= minCap) entries of
    (before, (ptr, _) : after) -> do
      writeIORef pool (before ++ after)
      pure ptr
    _ -> mallocForeignPtrBytes bytes

poolGive :: BufferPool -> ForeignPtr Word8 -> Int -> IO ()
poolGive pool ptr cap = do
  entries <- readIORef pool
  writeIORef pool (take bufferPoolLimit ((ptr, cap) : entries))

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
      cmds <- readIORef (daCommands da)
      let cmd =
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
      writeIORef (daCommands da) (cmd : cmds)
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
  setClip da rect
  r <- act
  let (cx, cy, cw, ch) = old
  setClip da (Rect cx cy cw ch)
  pure r

{-# INLINE pushRect #-}
pushRect :: DrawArena -> Rect -> Color -> IO ()
pushRect da (Rect x y w h) col = do
  let rgba = colorToWord32 col
      verts =
        [ Vertex x y 0 0 rgba
        , Vertex (x + w) y 1 0 rgba
        , Vertex (x + w) (y + h) 1 1 rgba
        , Vertex x (y + h) 0 1 rgba
        ]
  ensureVerts da 4
  ensureIndices da 6
  base <- readIORef (daVertexCount da)
  writeVerts da base verts
  writeIORef (daVertexCount da) (base + 4)
  baseIdx <- readIORef (daIndexCount da)
  writeIndices da baseIdx [base, base + 1, base + 2, base, base + 2, base + 3]
  writeIORef (daIndexCount da) (baseIdx + 6)

-- Rounded fills encode radius in vtxU and use vtxV = -1 (plain rects use v in [0, 1]).
{-# INLINE pushRoundedRect #-}
pushRoundedRect :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRect da (Rect x y w h) radius col = do
  let rgba = colorToWord32 col
      r = max 0 radius
      verts =
        [ Vertex x y r (-1) rgba
        , Vertex (x + w) y r (-1) rgba
        , Vertex (x + w) (y + h) r (-1) rgba
        , Vertex x (y + h) r (-1) rgba
        ]
  ensureVerts da 4
  ensureIndices da 6
  base <- readIORef (daVertexCount da)
  writeVerts da base verts
  writeIORef (daVertexCount da) (base + 4)
  baseIdx <- readIORef (daIndexCount da)
  writeIndices da baseIdx [base, base + 1, base + 2, base, base + 2, base + 3]
  writeIORef (daIndexCount da) (baseIdx + 6)

{-# INLINE pushLine #-}
pushLine :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushLine da x1 y1 x2 y2 thickness col = do
  let dx = x2 - x1
      dy = y2 - y1
      len = sqrt (dx * dx + dy * dy)
  if len < 0.001
    then pure ()
    else
      let nx = -dy / len * thickness / 2
          ny = dx / len * thickness / 2
          rgba = colorToWord32 col
          verts =
            [ Vertex (x1 + nx) (y1 + ny) 0 0 rgba
            , Vertex (x2 + nx) (y2 + ny) 1 0 rgba
            , Vertex (x2 - nx) (y2 - ny) 1 1 rgba
            , Vertex (x1 - nx) (y1 - ny) 0 1 rgba
            ]
       in do
        ensureVerts da 4
        ensureIndices da 6
        base <- readIORef (daVertexCount da)
        writeVerts da base verts
        writeIORef (daVertexCount da) (base + 4)
        baseIdx <- readIORef (daIndexCount da)
        writeIndices da baseIdx [base, base + 1, base + 2, base, base + 2, base + 3]
        writeIORef (daIndexCount da) (baseIdx + 6)

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

writeVerts :: DrawArena -> Int -> [Vertex] -> IO ()
writeVerts da base verts = do
  (ptr, _) <- readIORef (daVertices da)
  withForeignPtr ptr $ \p ->
    forM_ (zip [base ..] verts) $ \(i, Vertex x y u v rgba) -> do
      let off = i * vertexSize
      pokeByteOff p off x
      pokeByteOff p (off + 4) y
      pokeByteOff p (off + 8) u
      pokeByteOff p (off + 12) v
      pokeByteOff p (off + 16) rgba

writeIndices :: DrawArena -> Int -> [Int] -> IO ()
writeIndices da baseIdx indices = do
  (ptr, _) <- readIORef (daIndices da)
  withForeignPtr ptr $ \p ->
    forM_ (zip [baseIdx ..] indices) $ \(i, idx) ->
      pokeByteOff p (i * indexSize) (fromIntegral idx :: Word32)

{-# INLINE finishDraw #-}
finishDraw :: DrawArena -> IO DrawData
finishDraw da = do
  flushCmd da
  (vPtr, _) <- readIORef (daVertices da)
  (iPtr, _) <- readIORef (daIndices da)
  vCount <- readIORef (daVertexCount da)
  iCount <- readIORef (daIndexCount da)
  cmds <- readIORef (daCommands da)
  pure
    DrawData
      { drawVertices = vPtr
      , drawVertexCount = vCount
      , drawIndices = iPtr
      , drawIndexCount = iCount
      , drawCommands = reverse cmds
      }
