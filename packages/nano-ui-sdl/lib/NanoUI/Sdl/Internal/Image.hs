-- | Upload the core image atlas to SDL textures and manage their native lifetime.
module NanoUI.Sdl.Internal.Image
  ( ImageAtlas
  , newImageAtlas
  , destroyImageAtlas
  , syncImageAtlas
  , lookupImage
  )
where

import Control.Exception (mask_)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import NanoUI.Sdl.Internal.Error (withSdlError)
import NanoUI.Backend (AtlasUpload (..), Context, atlasChanges, atlasTextureId)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Render qualified as Render
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render (createTextureSafe, destroyTexture, setTextureBlendMode, updateTextureSafe)
import System.IO (hPutStrLn, stderr)

-- | The SDL texture of a context's image atlas. A texture and its metadata
-- have one lifetime; publish them together. The second field is the atlas
-- generation whose upload failed (0 for none): that generation is not
-- uploaded again, so a texture SDL will not make is asked for, and
-- reported, once rather than every frame.
data ImageAtlas = ImageAtlas !(IORef (Maybe AtlasTexture)) !(IORef Int)

data AtlasTexture = AtlasTexture
  { atTexture :: !(Ptr SDL_Texture)
  , atGeneration :: !Int
  }

-- | An atlas with no texture yet; the first 'syncImageAtlas' makes one.
newImageAtlas :: IO ImageAtlas
newImageAtlas = ImageAtlas <$> newIORef Nothing <*> newIORef 0

-- | Destroy the texture, if any, and forget a failed upload, so the next
-- 'syncImageAtlas' makes and fills a new texture.
destroyImageAtlas :: ImageAtlas -> IO ()
destroyImageAtlas (ImageAtlas ref failed) = mask_ (publish ref Nothing) >> writeIORef failed 0

-- | Make a texture (or none) the atlas's, and destroy the one it replaces.
publish :: IORef (Maybe AtlasTexture) -> Maybe AtlasTexture -> IO ()
publish ref new = atomicModifyIORef' ref (new,) >>= mapM_ (destroyTexture . atTexture)

-- | Bring the texture up to the context's image atlas. Pixels written in
-- place since the last upload go up as those rects only; a new texture is
-- made and filled when the atlas was resized, or there is none yet. A
-- generation whose upload failed is reported on stderr once and left until
-- the atlas changes again.
syncImageAtlas :: Ptr SDL_Renderer -> ImageAtlas -> Context -> IO ()
syncImageAtlas ren atlas@(ImageAtlas ref failed) ctx = do
  old <- readIORef ref
  changes <- atlasChanges ctx (maybe 0 atGeneration old)
  failedGen <- readIORef failed
  case (changes, old) of
    (Nothing, _) -> pure ()
    (Just (_, _, _, gen, _), _) | gen == failedGen -> pure ()
    (Just (w, h, pixels, gen, AtlasRegions rects), Just at) -> do
      ok <- updateRegions (atTexture at) w pixels rects
      -- A failed update leaves the texture behind, so fill a new one.
      if ok
        then writeIORef ref (Just at {atGeneration = gen})
        else uploadAtlas ren atlas w h pixels gen
    (Just (w, h, pixels, gen, _), _) -> uploadAtlas ren atlas w h pixels gen

-- | Copy the pixel rects @rects@ of an atlas @w@ pixels wide into @tex@.
updateRegions :: Ptr SDL_Texture -> Int -> ForeignPtr Word8 -> [(Int, Int, Int, Int)] -> IO Bool
updateRegions tex w pixels rects =
  withForeignPtr pixels $ \ptr ->
    and <$> mapM (update ptr) rects
  where
    update ptr (x, y, rw, rh) =
      with (SDL_Rect (fromIntegral x) (fromIntegral y) (fromIntegral rw) (fromIntegral rh)) $ \rp ->
        updateTextureSafe
          tex
          (PtrConst.unsafeFromPtr rp)
          (PtrConst.unsafeFromPtr (castPtr (ptr `plusPtr` ((y * w + x) * 4))))
          (fromIntegral (w * 4))

uploadAtlas ::
  Ptr SDL_Renderer -> ImageAtlas -> Int -> Int -> ForeignPtr Word8 -> Int -> IO ()
uploadAtlas ren (ImageAtlas ref failed) w h pixels gen = mask_ $ do
  tex <- createTextureSafe ren Pixels.SDL_PIXELFORMAT_RGBA32 Render.SDL_TEXTUREACCESS_STATIC (fromIntegral w) (fromIntegral h)
  if tex == nullPtr
    then giveUp "SDL_CreateTexture"
    else do
      void $ setTextureBlendMode tex (fromIntegral sDL_BLENDMODE_BLEND)
      uploaded <- updateRegions tex w pixels [(0, 0, w, h)]
      if uploaded
        then publish ref (Just (AtlasTexture tex gen)) >> writeIORef failed 0
        else giveUp "SDL_UpdateTexture" >> destroyTexture tex
  where
    giveUp call = do
      writeIORef failed gen
      msg <- withSdlError ("nano-ui: " ++ call ++ " failed for the " ++ show w ++ "x" ++ show h ++ " image atlas; images will not draw until it changes")
      hPutStrLn stderr msg

-- | The texture for a texture id, or null for one that is not the image
-- atlas's or an atlas not uploaded yet.
lookupImage :: ImageAtlas -> Int -> IO (Ptr SDL_Texture)
lookupImage (ImageAtlas ref _) tid
  | tid == atlasTextureId = maybe nullPtr atTexture <$> readIORef ref
  | otherwise = pure nullPtr
