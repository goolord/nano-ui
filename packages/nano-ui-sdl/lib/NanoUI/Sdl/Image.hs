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
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import NanoUI.Sdl.Display (destroyTexture)
import NanoUI.Testing (Context, atlasSnapshot, atlasTextureId)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)

-- A texture and its metadata have one lifetime; publish them together.
newtype ImageAtlas = ImageAtlas (IORef (Maybe AtlasTexture))

data AtlasTexture = AtlasTexture
  { atTexture :: !(Ptr ())
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
  withForeignPtr pixels $ \ptr ->
    alloca $ \out -> do
      poke out nullPtr
      ok <-
        createRgbaTexture
          ren
          (castPtr ptr)
          (fromIntegral w)
          (fromIntegral h)
          out
      when ok $ do
        tex <- peek out
        old <- readIORef ref
        writeIORef ref (Just (AtlasTexture tex gen))
        mapM_ (destroyTexture . atTexture) old

lookupImage :: ImageAtlas -> Int -> IO (Maybe (Ptr ()))
lookupImage (ImageAtlas ref) tid
  | tid == atlasTextureId = fmap atTexture <$> readIORef ref
  | otherwise = pure Nothing

foreign import ccall safe "nano_ui_create_rgba_texture"
  createRgbaTexture ::
    Ptr SDL_Renderer
    -> Ptr ()
    -> CInt
    -> CInt
    -> Ptr (Ptr ())
    -> IO Bool
