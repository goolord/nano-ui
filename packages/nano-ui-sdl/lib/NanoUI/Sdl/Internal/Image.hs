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
import Control.Monad (unless, void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import NanoUI.Testing (AtlasUpload (..), Context, atlasChanges, atlasTextureId)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Bindgen.Pixels (data SDL_PIXELFORMAT_RGBA32)
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture, data SDL_TEXTUREACCESS_STATIC)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render (createTextureSafe, destroyTexture, setTextureBlendMode, updateTextureSafe)

-- A texture and its metadata have one lifetime; publish them together.
newtype ImageAtlas = ImageAtlas (IORef (Maybe AtlasTexture))

data AtlasTexture = AtlasTexture
  { atTexture :: !(Ptr SDL_Texture)
  , atGeneration :: !Int
  }

newImageAtlas :: IO ImageAtlas
newImageAtlas = ImageAtlas <$> newIORef Nothing

destroyImageAtlas :: ImageAtlas -> IO ()
destroyImageAtlas (ImageAtlas ref) = mask_ (publish ref Nothing)

-- | Make a texture (or none) the atlas's, and destroy the one it replaces.
publish :: IORef (Maybe AtlasTexture) -> Maybe AtlasTexture -> IO ()
publish ref new = atomicModifyIORef' ref (\old -> (new, old)) >>= mapM_ (destroyTexture . atTexture)

-- | Bring the texture up to the context's image atlas. Pixels written in
-- place since the last upload go up as those rects only; a new texture is
-- made and filled when the atlas was resized, or there is none yet.
syncImageAtlas :: Ptr SDL_Renderer -> ImageAtlas -> Context -> IO ()
syncImageAtlas ren atlas@(ImageAtlas ref) ctx = do
  old <- readIORef ref
  changes <- atlasChanges ctx (maybe 0 atGeneration old)
  case (changes, old) of
    (Nothing, _) -> pure ()
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
uploadAtlas ren (ImageAtlas ref) w h pixels gen = mask_ $ do
  tex <- createTextureSafe ren SDL_PIXELFORMAT_RGBA32 SDL_TEXTUREACCESS_STATIC (fromIntegral w) (fromIntegral h)
  unless (tex == nullPtr) $ do
    void $ setTextureBlendMode tex (fromIntegral sDL_BLENDMODE_BLEND)
    uploaded <- updateRegions tex w pixels [(0, 0, w, h)]
    if uploaded then publish ref (Just (AtlasTexture tex gen)) else destroyTexture tex

-- | The texture for a texture id, or null for one that is not the image
-- atlas's or an atlas not uploaded yet.
lookupImage :: ImageAtlas -> Int -> IO (Ptr SDL_Texture)
lookupImage (ImageAtlas ref) tid
  | tid == atlasTextureId = maybe nullPtr atTexture <$> readIORef ref
  | otherwise = pure nullPtr
