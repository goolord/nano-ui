module NanoUI.Sdl.Render
  ( renderDrawData
  , renderDrawDataPass
  , setLogicalClipRect
  , clearLogicalClipRect
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
import NanoUI (Color (..), DrawCmd (..), DrawData (..), Layer (..), Rect (..), indexSize, vertexSize)
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
  renderDrawDataPass ren uiScale (Just clearColor) drawData False images

renderDrawDataPass :: Ptr SDL_Renderer -> Float -> Maybe Color -> DrawData -> Bool -> ImageAtlas -> IO ()
renderDrawDataPass ren uiScale mClear drawData overlayPass images = do
  clearLogicalClipRect ren
  case mClear of
    Just clearColor -> do
      let (cr, cg, cb, ca) = unpackColor clearColor
      void $ setRenderDrawColorSafe ren cr cg cb ca
      void $ renderClearSafe ren
    Nothing -> pure ()
  let (overlay, base) = partition ((== LayerOverlay) . cmdLayer) (drawCommands drawData)
      cmds = if overlayPass then overlay else base
  mapM_ (drawCmd ren uiScale drawData images) (sortBy (comparing layerOrder) cmds)
  clearLogicalClipRect ren

layerOrder :: DrawCmd -> Int
layerOrder cmd =
  case cmdLayer cmd of
    LayerBackground -> 0
    LayerContent -> 1
    LayerOverlay -> 2

drawCmd :: Ptr SDL_Renderer -> Float -> DrawData -> ImageAtlas -> DrawCmd -> IO ()
drawCmd ren uiScale dd images cmd = do
  let count = fromIntegral (cmdIndexCount cmd)
  when (count > 0 && count `mod` 6 == 0) $ do
    setCmdClip ren uiScale cmd
    let start = fromIntegral (cmdIndexOffset cmd)
        texId = cmdTextureId cmd
    mapM_ (fillQuad ren uiScale dd images texId) [start, start + 6 .. start + count - 1]

setCmdClip :: Ptr SDL_Renderer -> Float -> DrawCmd -> IO ()
setCmdClip ren uiScale cmd
  | cmdClipW cmd >= 1e8 || cmdClipH cmd >= 1e8 =
      clearLogicalClipRect ren
  | otherwise =
      setLogicalClipRect ren uiScale (Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd))

fillQuad :: Ptr SDL_Renderer -> Float -> DrawData -> ImageAtlas -> Int -> Int -> IO ()
fillQuad ren uiScale dd images texId i = do
  vert0 <- vertexAt dd i
  vert1 <- vertexAt dd (i + 1)
  vert2 <- vertexAt dd (i + 2)
  case (vert0, vert1, vert2) of
    (Just (x0, y0, u0, v0c, rgba), Just (x1, y1, _, _, _), Just (x2, y2, u1, v1c, _)) ->
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
