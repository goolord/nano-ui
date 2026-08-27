module NanoUI.Sdl.Render
  ( renderDrawData
  , renderDrawDataPass
  , setLogicalClipRect
  , clearLogicalClipRect
  , logicalClipKey
  , snapDamage
  ) where

import NanoUI.Sdl.Image (ImageAtlas, lookupImage)

import Control.Monad (void, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (partition, sortBy)
import Data.Ord (comparing)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CFloat (..), CInt)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..), peekByteOff, poke)
import NanoUI (Color (..), Damage (..), DrawCmd (..), DrawData (..), Layer (..), Rect (..), damageIsEmpty, indexSize, rectIntersect, vertexSize)
import NanoUI.Sdl.Shape (fillRoundedRect, fillSolidRect, fillTriangle)
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render
  ( renderClearSafe
  , setRenderClipRectSafe
  , setRenderDrawColorSafe
  )

nullClip :: PtrConst.PtrConst SDL_Rect
nullClip = PtrConst.unsafeFromPtr (nullPtr :: Ptr SDL_Rect)

type Vertex = (Float, Float, Float, Float, Word32)

data ClipState
  = ClipNone
  | ClipRect !Rect
  deriving (Eq)

{-# INLINE snapDamage #-}
snapDamage :: Float -> Damage -> Damage
snapDamage _ DamageFull = DamageFull
snapDamage uiScale (DamageClip r) = DamageClip (snapClipRect uiScale r)

snapClipRect :: Float -> Rect -> Rect
snapClipRect uiScale (Rect x y w h) =
  let s = if uiScale > 0 then uiScale else 1
      x0 = fromIntegral (floor (x * s) :: Int)
      y0 = fromIntegral (floor (y * s) :: Int)
      x1 = fromIntegral (ceiling ((x + w) * s) :: Int)
      y1 = fromIntegral (ceiling ((y + h) * s) :: Int)
   in Rect (x0 / s) (y0 / s) ((x1 - x0) / s) ((y1 - y0) / s)

setLogicalClipRect :: Ptr SDL_Renderer -> Float -> Rect -> IO ()
setLogicalClipRect ren uiScale (Rect x y w h) =
  alloca $ \(r :: Ptr SDL_Rect) -> do
    let lx = x * uiScale
        ly = y * uiScale
        rx = (x + w) * uiScale
        by = (y + h) * uiScale
        x0 = floor lx
        y0 = floor ly
        x1 = ceiling rx
        y1 = ceiling by
    poke
      r
      SDL_Rect
        { x = ci x0
        , y = ci y0
        , w = ci (max (0 :: Int) (x1 - x0))
        , h = ci (max (0 :: Int) (y1 - y0))
        }
    void $ setRenderClipRectSafe ren (PtrConst.unsafeFromPtr r)

clearLogicalClipRect :: Ptr SDL_Renderer -> IO ()
clearLogicalClipRect ren = void $ setRenderClipRectSafe ren nullClip

applyClipState :: IORef ClipState -> Ptr SDL_Renderer -> Float -> ClipState -> IO ()
applyClipState ref ren uiScale next = do
  prev <- readIORef ref
  when (prev /= next) $ do
    writeIORef ref next
    case next of
      ClipNone -> clearLogicalClipRect ren
      ClipRect r -> setLogicalClipRect ren uiScale r

renderDrawData :: Ptr SDL_Renderer -> Float -> Color -> DrawData -> ImageAtlas -> IO ()
renderDrawData ren uiScale clearColor drawData images =
  renderDrawDataPass ren uiScale (Just clearColor) drawData False images DamageFull

renderDrawDataPass :: Ptr SDL_Renderer -> Float -> Maybe Color -> DrawData -> Bool -> ImageAtlas -> Damage -> IO ()
renderDrawDataPass ren uiScale mClear drawData overlayPass images damage = do
  when (not (damageIsEmpty damage)) $ do
    clipRef <- newIORef ClipNone
    clearLogicalClipRect ren
    case (mClear, damage) of
      (Just clearColor, DamageFull) -> do
        let (cr, cg, cb, ca) = unpackColor clearColor
        void $ setRenderDrawColorSafe ren cr cg cb ca
        void $ renderClearSafe ren
      (Just clearColor, DamageClip r) -> do
        let (cr, cg, cb, ca) = unpackColor clearColor
            px = rectX r * uiScale
            py = rectY r * uiScale
            pw = rectW r * uiScale
            ph = rectH r * uiScale
        fillSolidRect ren cr cg cb ca px py pw ph
        applyClipState clipRef ren uiScale (ClipRect r)
      (Nothing, DamageClip r) -> applyClipState clipRef ren uiScale (ClipRect r)
      (Nothing, DamageFull) -> pure ()
    let (overlay, base) = partition ((== LayerOverlay) . cmdLayer) (drawCommands drawData)
        cmds = if overlayPass then overlay else base
        clip = case damage of
          DamageFull -> Nothing
          DamageClip r -> Just r
    withForeignPtr (drawVertices drawData) $ \vp ->
      withForeignPtr (drawIndices drawData) $ \ip ->
        mapM_ (drawCmd ren uiScale vp ip drawData images clip clipRef) (sortBy (comparing layerOrder) cmds)
    applyClipState clipRef ren uiScale ClipNone

layerOrder :: DrawCmd -> Int
layerOrder cmd =
  case cmdLayer cmd of
    LayerBackground -> 0
    LayerContent -> 1
    LayerOverlay -> 2

drawCmd ::
  Ptr SDL_Renderer ->
  Float ->
  Ptr Word8 ->
  Ptr Word8 ->
  DrawData ->
  ImageAtlas ->
  Maybe Rect ->
  IORef ClipState ->
  DrawCmd ->
  IO ()
drawCmd ren uiScale vp ip dd images mDamage clipRef cmd = do
  let count = fromIntegral (cmdIndexCount cmd)
      cmdRect = Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)
      cmdOpen = cmdClipW cmd >= 1e8 || cmdClipH cmd >= 1e8
      live = case (mDamage, cmdOpen) of
        (Nothing, _) -> Just cmdRect
        (Just dmg, True) -> Just dmg
        (Just dmg, False) -> rectIntersect dmg cmdRect
  when (count > 0 && count `mod` 3 == 0) $
    case live of
      Nothing -> pure ()
      Just clip -> do
        if cmdOpen && mDamage == Nothing
          then applyClipState clipRef ren uiScale ClipNone
          else applyClipState clipRef ren uiScale (ClipRect clip)
        let start = fromIntegral (cmdIndexOffset cmd)
            texId = cmdTextureId cmd
        mapM_ (fillQuad ren uiScale vp ip dd images texId mDamage) [start, start + 3 .. start + count - 1]

{-# INLINE fillQuad #-}
fillQuad ::
  Ptr SDL_Renderer ->
  Float ->
  Ptr Word8 ->
  Ptr Word8 ->
  DrawData ->
  ImageAtlas ->
  Int ->
  Maybe Rect ->
  Int ->
  IO ()
fillQuad ren uiScale vp ip dd images texId mDamage i = do
  readTriangleVerts vp ip dd i >>= \case
    Nothing -> pure ()
    Just ((x0, y0, u0, v0c, rgba), (x1, y1, _, _, _), (x2, y2, u1, v1c, _))
      | not (quadHitsDamage mDamage x0 y0 x1 y1 x2 y2) -> pure ()
      | otherwise ->
          let (r, g, b, a) = unpackColor (Color rgba)
           in if texId > 0
                    then
                      let w = x2 - x0
                          h = y2 - y0
                       in when (w > 0 && h > 0) $ do
                            let px = x0 * uiScale
                                py = y0 * uiScale
                                pw = w * uiScale
                                ph = h * uiScale
                            mTex <- lookupImage images texId
                            case mTex of
                              Just tex ->
                                void $
                                  renderTextureDst
                                    ren
                                    tex
                                    (cf px)
                                    (cf py)
                                    (cf pw)
                                    (cf ph)
                                    (cf u0)
                                    (cf v0c)
                                    (cf u1)
                                    (cf v1c)
                                    r
                                    g
                                    b
                                    a
                              Nothing -> fillSolidRect ren r g b a px py pw ph
                    else if u0 <= -1.5
                      then
                        fillTriangle
                          ren
                          r
                          g
                          b
                          a
                          (x0 * uiScale)
                          (y0 * uiScale)
                          (x1 * uiScale)
                          (y1 * uiScale)
                          (x2 * uiScale)
                          (y2 * uiScale)
                      else
                        let w = x2 - x0
                            h = y2 - y0
                         in when (w > 0 && h > 0) $ do
                              let px = x0 * uiScale
                                  py = y0 * uiScale
                                  pw = w * uiScale
                                  ph = h * uiScale
                              if v0c < 0
                                then fillRoundedRect ren r g b a px py pw ph (u0 * uiScale)
                                else fillSolidRect ren r g b a px py pw ph

readTriangleVerts :: Ptr Word8 -> Ptr Word8 -> DrawData -> Int -> IO (Maybe (Vertex, Vertex, Vertex))
readTriangleVerts vp ip dd i
  | i < 0 || i + 2 >= drawIndexCount dd = pure Nothing
  | otherwise = do
      vi0 <- peekIndexPtr ip i
      vi1 <- peekIndexPtr ip (i + 1)
      vi2 <- peekIndexPtr ip (i + 2)
      let vc = drawVertexCount dd
      if vi0 < 0 || vi1 < 0 || vi2 < 0 || vi0 >= vc || vi1 >= vc || vi2 >= vc
        then pure Nothing
        else do
          v0 <- readVertexPtr vp vi0
          v1 <- readVertexPtr vp vi1
          v2 <- readVertexPtr vp vi2
          pure (Just (v0, v1, v2))

{-# INLINE peekIndexPtr #-}
peekIndexPtr :: Ptr Word8 -> Int -> IO Int
peekIndexPtr ip i = fromIntegral <$> peekWord32Ptr ip (i * indexSize)

{-# INLINE readVertexPtr #-}
readVertexPtr :: Ptr Word8 -> Int -> IO Vertex
readVertexPtr vp vi = do
  let off = vi * vertexSize
  x <- peekFloatPtr vp off
  y <- peekFloatPtr vp (off + 4)
  u <- peekFloatPtr vp (off + 8)
  v <- peekFloatPtr vp (off + 12)
  rgba <- peekWord32Ptr vp (off + 16)
  pure (x, y, u, v, rgba)

{-# INLINE peekWord32Ptr #-}
peekWord32Ptr :: Ptr Word8 -> Int -> IO Word32
peekWord32Ptr p off = peekByteOff p off

{-# INLINE peekFloatPtr #-}
peekFloatPtr :: Ptr Word8 -> Int -> IO Float
peekFloatPtr p off = peekByteOff p off

quadHitsDamage :: Maybe Rect -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
quadHitsDamage Nothing _ _ _ _ _ _ = True
quadHitsDamage (Just clip) x0 y0 x1 y1 x2 y2 =
  let minx = min x0 (min x1 x2)
      maxx = max x0 (max x1 x2)
      miny = min y0 (min y1 y2)
      maxy = max y0 (max y1 y2)
   in case rectIntersect clip (Rect minx miny (maxx - minx) (maxy - miny)) of
        Nothing -> False
        Just _ -> True

cf :: Float -> CFloat
cf = realToFrac

foreign import ccall safe "nano_ui_render_texture_dst"
  renderTextureDst ::
    Ptr SDL_Renderer ->
    Ptr () ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    Word8 ->
    Word8 ->
    Word8 ->
    Word8 ->
    IO Bool

ci :: Int -> CInt
ci = fromIntegral

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral $ (w `shiftR` 24) .&. 0xFF
  , fromIntegral $ (w `shiftR` 16) .&. 0xFF
  , fromIntegral $ (w `shiftR` 8) .&. 0xFF
  , fromIntegral $ w .&. 0xFF
  )

{-# INLINE logicalClipKey #-}
logicalClipKey :: Float -> Rect -> (Int, Int, Int, Int)
logicalClipKey uiScale (Rect x y w h) =
  let s = if uiScale > 0 then uiScale else 1
      x0 = floor (x * s) :: Int
      y0 = floor (y * s) :: Int
      x1 = ceiling ((x + w) * s) :: Int
      y1 = ceiling ((y + h) * s) :: Int
   in (x0, y0, max 0 (x1 - x0), max 0 (y1 - y0))
