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
  , currentLayer
  ) where

import Control.Monad (forM_, unless, when)
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
import NanoUI.Types (Color, Rect (..), colorToWord32, rectIntersect)
import qualified Data.Text as T

data Layer = LayerBackground | LayerContent | LayerOverlay | LayerChrome
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
vertexSize = 20

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

{-# INLINE pushQuad #-}
pushQuad :: DrawArena -> Rect -> Float -> Float -> Float -> Float -> Color -> IO ()
pushQuad da (Rect x y w h) u0 v0 u1 v1 col = do
  vCount <- readIORef (daVertexCount da)
  iCount <- readIORef (daIndexCount da)
  vCap <- readIORef (daVertexCap da)
  iCap <- readIORef (daIndexCap da)
  (vp, ip, base, baseIdx) <-
    if vCount + 4 <= vCap && iCount + 6 <= iCap
      then do
        vp <- readIORef (daVertexPtr da)
        ip <- readIORef (daIndexPtr da)
        pure (vp, ip, vCount, iCount)
      else do
        ensureCapacity da 4 6
        vp <- readIORef (daVertexPtr da)
        ip <- readIORef (daIndexPtr da)
        vc <- readIORef (daVertexCount da)
        ic <- readIORef (daIndexCount da)
        pure (vp, ip, vc, ic)
  let !rgba = colorToWord32 col
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !b = fromIntegral base :: Word32
      !x1 = x + w
      !y1 = y + h
  pokeByteOff vp vOff x
  pokeByteOff vp (vOff + 4) y
  pokeByteOff vp (vOff + 8) u0
  pokeByteOff vp (vOff + 12) v0
  pokeByteOff vp (vOff + 16) rgba
  pokeByteOff vp (vOff + 20) x1
  pokeByteOff vp (vOff + 24) y
  pokeByteOff vp (vOff + 28) u1
  pokeByteOff vp (vOff + 32) v0
  pokeByteOff vp (vOff + 36) rgba
  pokeByteOff vp (vOff + 40) x1
  pokeByteOff vp (vOff + 44) y1
  pokeByteOff vp (vOff + 48) u1
  pokeByteOff vp (vOff + 52) v1
  pokeByteOff vp (vOff + 56) rgba
  pokeByteOff vp (vOff + 60) x
  pokeByteOff vp (vOff + 64) y1
  pokeByteOff vp (vOff + 68) u0
  pokeByteOff vp (vOff + 72) v1
  pokeByteOff vp (vOff + 76) rgba
  pokeByteOff ip iOff b
  pokeByteOff ip (iOff + 4) (b + 1)
  pokeByteOff ip (iOff + 8) (b + 2)
  pokeByteOff ip (iOff + 12) b
  pokeByteOff ip (iOff + 16) (b + 2)
  pokeByteOff ip (iOff + 20) (b + 3)
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

-- Rounded fills: vtxU = radius, vtxV = -1. Strokes: vtxV = -(1 + borderWidth).
{-# INLINE pushRoundedRect #-}
pushRoundedRect :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRect da rect radius col = pushRoundedCoded da rect radius (-1) col

{-# INLINE pushRoundedStroke #-}
pushRoundedStroke :: DrawArena -> Rect -> Float -> Float -> Color -> IO ()
pushRoundedStroke da rect radius bw col =
  pushRoundedCoded da rect radius (-(1 + max 1 bw)) col

{-# INLINE pushRoundedCoded #-}
pushRoundedCoded :: DrawArena -> Rect -> Float -> Float -> Color -> IO ()
pushRoundedCoded da (Rect x y w h) radius v col = do
  setTexture da 0
  vCount <- readIORef (daVertexCount da)
  iCount <- readIORef (daIndexCount da)
  vCap <- readIORef (daVertexCap da)
  iCap <- readIORef (daIndexCap da)
  (vp, ip, base, baseIdx) <-
    if vCount + 4 <= vCap && iCount + 6 <= iCap
      then do
        vp <- readIORef (daVertexPtr da)
        ip <- readIORef (daIndexPtr da)
        pure (vp, ip, vCount, iCount)
      else do
        ensureCapacity da 4 6
        vp <- readIORef (daVertexPtr da)
        ip <- readIORef (daIndexPtr da)
        vc <- readIORef (daVertexCount da)
        ic <- readIORef (daIndexCount da)
        pure (vp, ip, vc, ic)
  let !rgba = colorToWord32 col
      !r = max 0 radius
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !b = fromIntegral base :: Word32
      !x1 = x + w
      !y1 = y + h
  pokeByteOff vp vOff x
  pokeByteOff vp (vOff + 4) y
  pokeByteOff vp (vOff + 8) r
  pokeByteOff vp (vOff + 12) v
  pokeByteOff vp (vOff + 16) rgba
  pokeByteOff vp (vOff + 20) x1
  pokeByteOff vp (vOff + 24) y
  pokeByteOff vp (vOff + 28) r
  pokeByteOff vp (vOff + 32) v
  pokeByteOff vp (vOff + 36) rgba
  pokeByteOff vp (vOff + 40) x1
  pokeByteOff vp (vOff + 44) y1
  pokeByteOff vp (vOff + 48) r
  pokeByteOff vp (vOff + 52) v
  pokeByteOff vp (vOff + 56) rgba
  pokeByteOff vp (vOff + 60) x
  pokeByteOff vp (vOff + 64) y1
  pokeByteOff vp (vOff + 68) r
  pokeByteOff vp (vOff + 72) v
  pokeByteOff vp (vOff + 76) rgba
  pokeByteOff ip iOff b
  pokeByteOff ip (iOff + 4) (b + 1)
  pokeByteOff ip (iOff + 8) (b + 2)
  pokeByteOff ip (iOff + 12) b
  pokeByteOff ip (iOff + 16) (b + 2)
  pokeByteOff ip (iOff + 20) (b + 3)
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
  vCount <- readIORef (daVertexCount da)
  iCount <- readIORef (daIndexCount da)
  vCap <- readIORef (daVertexCap da)
  iCap <- readIORef (daIndexCap da)
  (vp, ip, base, baseIdx) <-
    if vCount + 3 <= vCap && iCount + 3 <= iCap
      then do
        vp <- readIORef (daVertexPtr da)
        ip <- readIORef (daIndexPtr da)
        pure (vp, ip, vCount, iCount)
      else do
        ensureCapacity da 3 3
        vp <- readIORef (daVertexPtr da)
        ip <- readIORef (daIndexPtr da)
        vc <- readIORef (daVertexCount da)
        ic <- readIORef (daIndexCount da)
        pure (vp, ip, vc, ic)
  let !rgba = colorToWord32 col
      !vOff = base * vertexSize
      !iOff = baseIdx * indexSize
      !b = fromIntegral base :: Word32
  pokeByteOff vp vOff x0
  pokeByteOff vp (vOff + 4) y0
  pokeByteOff vp (vOff + 8) (-3 :: Float)
  pokeByteOff vp (vOff + 12) (0 :: Float)
  pokeByteOff vp (vOff + 16) rgba
  pokeByteOff vp (vOff + 20) x1
  pokeByteOff vp (vOff + 24) y1
  pokeByteOff vp (vOff + 28) (-3 :: Float)
  pokeByteOff vp (vOff + 32) (0 :: Float)
  pokeByteOff vp (vOff + 36) rgba
  pokeByteOff vp (vOff + 40) x2
  pokeByteOff vp (vOff + 44) y2
  pokeByteOff vp (vOff + 48) (-3 :: Float)
  pokeByteOff vp (vOff + 52) (0 :: Float)
  pokeByteOff vp (vOff + 56) rgba
  pokeByteOff ip iOff b
  pokeByteOff ip (iOff + 4) (b + 1)
  pokeByteOff ip (iOff + 8) (b + 2)
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
