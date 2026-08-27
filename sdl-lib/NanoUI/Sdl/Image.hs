module NanoUI.Sdl.Image
  ( ImageAtlas
  , newImageAtlas
  , destroyImageAtlas
  , syncImageAtlas
  , lookupImage
  , lookupAtlasTex
  ) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import NanoUI (Context, atlasSnapshot, atlasTextureId)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)

data ImageAtlas = ImageAtlas
  { iaTex :: IORef (Maybe (Ptr ()))
  , iaW :: IORef Int
  , iaH :: IORef Int
  , iaGen :: IORef Int
  }

newImageAtlas :: IO ImageAtlas
newImageAtlas = do
  tex <- newIORef Nothing
  w <- newIORef 0
  h <- newIORef 0
  gen <- newIORef 0
  pure ImageAtlas {iaTex = tex, iaW = w, iaH = h, iaGen = gen}

destroyImageAtlas :: ImageAtlas -> IO ()
destroyImageAtlas atlas = do
  mTex <- readIORef (iaTex atlas)
  mapM_ destroyTexture mTex
  writeIORef (iaTex atlas) Nothing
  writeIORef (iaW atlas) 0
  writeIORef (iaH atlas) 0
  writeIORef (iaGen atlas) 0

syncImageAtlas :: Ptr SDL_Renderer -> ImageAtlas -> Context -> IO ()
syncImageAtlas ren atlas ctx = do
  snap <- atlasSnapshot ctx
  case snap of
    Nothing -> pure ()
    Just (w, h, pixels, gen) -> do
      oldGen <- readIORef (iaGen atlas)
      when (gen /= oldGen) $
        uploadAtlas ren atlas w h pixels gen

uploadAtlas :: Ptr SDL_Renderer -> ImageAtlas -> Int -> Int -> ForeignPtr Word8 -> Int -> IO ()
uploadAtlas ren atlas w h pixels gen =
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
        old <- readIORef (iaTex atlas)
        mapM_ destroyTexture old
        writeIORef (iaTex atlas) (Just tex)
        writeIORef (iaW atlas) w
        writeIORef (iaH atlas) h
        writeIORef (iaGen atlas) gen

lookupImage :: ImageAtlas -> Int -> IO (Maybe (Ptr ()))
lookupImage atlas tid
  | tid == atlasTextureId = readIORef (iaTex atlas)
  | otherwise = pure Nothing

lookupAtlasTex :: ImageAtlas -> Int -> IO (Maybe (Ptr (), Float, Float))
lookupAtlasTex atlas tid
  | tid == atlasTextureId = do
      mTex <- readIORef (iaTex atlas)
      tw <- fromIntegral <$> readIORef (iaW atlas)
      th <- fromIntegral <$> readIORef (iaH atlas)
      pure (fmap (\tex -> (tex, tw, th)) mTex)
  | otherwise = pure Nothing

foreign import ccall safe "nano_ui_create_rgba_texture"
  createRgbaTexture ::
    Ptr SDL_Renderer ->
    Ptr () ->
    CInt ->
    CInt ->
    Ptr (Ptr ()) ->
    IO Bool

foreign import ccall safe "nano_ui_destroy_texture"
  destroyTexture :: Ptr () -> IO ()
