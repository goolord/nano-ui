module NanoUI.Sdl.Render
  ( renderDrawData
  , renderDrawDataPass
  , setLogicalClipRect
  , clearLogicalClipRect
  , snapDamage
  ) where

import NanoUI.Sdl.Image (ImageAtlas, lookupImage)

import Control.Monad (void, when)
import Data.Bits (shiftR, (.&.))
import Data.List (partition, sortBy)
import Data.Ord (comparing)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CFloat (..), CInt)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
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

-- Align a logical clip to the same integer pixels as setLogicalClipRect.
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

renderDrawData :: Ptr SDL_Renderer -> Float -> Color -> DrawData -> ImageAtlas -> IO ()
renderDrawData ren uiScale clearColor drawData images =
  renderDrawDataPass ren uiScale (Just clearColor) drawData False images DamageFull

renderDrawDataPass :: Ptr SDL_Renderer -> Float -> Maybe Color -> DrawData -> Bool -> ImageAtlas -> Damage -> IO ()
renderDrawDataPass ren uiScale mClear drawData overlayPass images damage = do
  when (not (damageIsEmpty damage)) $ do
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
        setLogicalClipRect ren uiScale r
      (Nothing, DamageClip r) -> setLogicalClipRect ren uiScale r
      (Nothing, DamageFull) -> pure ()
    let (overlay, base) = partition ((== LayerOverlay) . cmdLayer) (drawCommands drawData)
        cmds = if overlayPass then overlay else base
        clip = case damage of
          DamageFull -> Nothing
          DamageClip r -> Just r
    mapM_ (drawCmd ren uiScale drawData images clip) (sortBy (comparing layerOrder) cmds)
    clearLogicalClipRect ren

layerOrder :: DrawCmd -> Int
layerOrder cmd =
  case cmdLayer cmd of
    LayerBackground -> 0
    LayerContent -> 1
    LayerOverlay -> 2

drawCmd :: Ptr SDL_Renderer -> Float -> DrawData -> ImageAtlas -> Maybe Rect -> DrawCmd -> IO ()
drawCmd ren uiScale dd images mDamage cmd = do
  let count = fromIntegral (cmdIndexCount cmd)
      cmdRect = Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)
      cmdOpen = cmdClipW cmd >= 1e8 || cmdClipH cmd >= 1e8
      live = case (mDamage, cmdOpen) of
        (Nothing, True) -> Just cmdRect
        (Nothing, False) -> Just cmdRect
        (Just dmg, True) -> Just dmg
        (Just dmg, False) -> rectIntersect dmg cmdRect
  when (count > 0 && count `mod` 3 == 0) $
    case live of
      Nothing -> pure ()
      Just clip -> do
        if cmdOpen && mDamage == Nothing
          then clearLogicalClipRect ren
          else setLogicalClipRect ren uiScale clip
        let start = fromIntegral (cmdIndexOffset cmd)
            texId = cmdTextureId cmd
        mapM_ (fillQuad ren uiScale dd images texId mDamage) [start, start + 3 .. start + count - 1]

fillQuad :: Ptr SDL_Renderer -> Float -> DrawData -> ImageAtlas -> Int -> Maybe Rect -> Int -> IO ()
fillQuad ren uiScale dd images texId mDamage i = do
  vert0 <- vertexAt dd i
  vert1 <- vertexAt dd (i + 1)
  vert2 <- vertexAt dd (i + 2)
  case (vert0, vert1, vert2) of
    (Just (x0, y0, u0, v0c, rgba), Just (x1, y1, _, _, _), Just (x2, y2, u1, v1c, _))
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
    _ -> pure ()

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

vertexAt :: DrawData -> Int -> IO (Maybe (Float, Float, Float, Float, Word32))
vertexAt dd slot = do
  mVi <- readIndexAt dd slot
  case mVi of
    Nothing -> pure Nothing
    Just vi
      | vi < 0 || vi >= drawVertexCount dd -> pure Nothing
      | otherwise -> do
          x <- peekFloatAt (drawVertices dd) (vi * vertexSize)
          y <- peekFloatAt (drawVertices dd) (vi * vertexSize + 4)
          u <- peekFloatAt (drawVertices dd) (vi * vertexSize + 8)
          v <- peekFloatAt (drawVertices dd) (vi * vertexSize + 12)
          rgba <- peekWord32At (drawVertices dd) (vi * vertexSize + 16)
          pure (Just (x, y, u, v, rgba))

readIndexAt :: DrawData -> Int -> IO (Maybe Int)
readIndexAt dd i
  | i < 0 || i >= drawIndexCount dd = pure Nothing
  | otherwise =
      Just . fromIntegral <$> peekWord32At (drawIndices dd) (i * indexSize)

peekWord32At :: ForeignPtr Word8 -> Int -> IO Word32
peekWord32At fp off = withForeignPtr fp $ \p -> peekByteOff p off

peekFloatAt :: ForeignPtr Word8 -> Int -> IO Float
peekFloatAt fp off = withForeignPtr fp $ \p -> peekByteOff p off

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral $ (w `shiftR` 24) .&. 0xFF
  , fromIntegral $ (w `shiftR` 16) .&. 0xFF
  , fromIntegral $ (w `shiftR` 8) .&. 0xFF
  , fromIntegral $ w .&. 0xFF
  )
