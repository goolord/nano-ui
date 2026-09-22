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
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
destroyImageAtlas (ImageAtlas ref) = mask_ $ do
  old <- readIORef ref
  writeIORef ref Nothing
  mapM_ (destroyTexture . atTexture) old

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
uploadAtlas ren (ImageAtlas ref) w h pixels gen = mask_ $
  withForeignPtr pixels $ \ptr -> do
    tex <- createTextureSafe ren SDL_PIXELFORMAT_RGBA32 SDL_TEXTUREACCESS_STATIC (fromIntegral w) (fromIntegral h)
    ok <-
      if tex == nullPtr
        then pure False
        else do
          _ <- setTextureBlendMode tex (fromIntegral sDL_BLENDMODE_BLEND)
          uploaded <- updateTextureSafe tex (PtrConst.unsafeFromPtr nullPtr) (PtrConst.unsafeFromPtr (castPtr ptr)) (fromIntegral (w * 4))
          if uploaded then pure True else destroyTexture tex >> pure False
    when ok $ do
      old <- readIORef ref
      writeIORef ref (Just (AtlasTexture tex gen))
      mapM_ (destroyTexture . atTexture) old

lookupImage :: ImageAtlas -> Int -> IO (Maybe (Ptr SDL_Texture))
lookupImage (ImageAtlas ref) tid
  | tid == atlasTextureId = fmap atTexture <$> readIORef ref
  | otherwise = pure Nothing
