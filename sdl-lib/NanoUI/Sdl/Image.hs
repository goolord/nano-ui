module NanoUI.Sdl.Image
  ( ImageAtlas
  , newImageAtlas
  , destroyImageAtlas
  , syncImageAtlas
  , lookupImage
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import NanoUI (Context, atlasSnapshot, atlasTextureId)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)

data ImageAtlas = ImageAtlas
  { iaTex :: IORef (Maybe (Ptr ()))
  , iaGen :: IORef Int
  }

newImageAtlas :: IO ImageAtlas
newImageAtlas = ImageAtlas <$> newIORef Nothing <*> newIORef 0

destroyImageAtlas :: ImageAtlas -> IO ()
destroyImageAtlas atlas = do
  mTex <- readIORef (iaTex atlas)
  mapM_ destroyTexture mTex
  writeIORef (iaTex atlas) Nothing
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

uploadAtlas :: Ptr SDL_Renderer -> ImageAtlas -> Int -> Int -> ByteString -> Int -> IO ()
uploadAtlas ren atlas w h pixels gen =
  BS.useAsCStringLen pixels $ \(ptr, _) ->
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
        writeIORef (iaGen atlas) gen

lookupImage :: ImageAtlas -> Int -> IO (Maybe (Ptr ()))
lookupImage atlas tid
  | tid == atlasTextureId = readIORef (iaTex atlas)
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
