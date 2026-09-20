-- | Upload the core image atlas to SDL textures and manage their native lifetime.
module NanoUI.Sdl.Image
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
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import NanoUI.Testing (Context, atlasSnapshot, atlasTextureId)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Bindgen.Pixels (data SDL_PIXELFORMAT_RGBA32)
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

syncImageAtlas :: Ptr SDL_Renderer -> ImageAtlas -> Context -> IO ()
syncImageAtlas ren atlas@(ImageAtlas ref) ctx = do
  snap <- atlasSnapshot ctx
  case snap of
    Nothing -> pure ()
    Just (w, h, pixels, gen) -> do
      old <- readIORef ref
      when (Just gen /= fmap atGeneration old) $
        uploadAtlas ren atlas w h pixels gen

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
