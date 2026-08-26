module NanoUI.Sdl.Image
  ( ImageAtlas
  , newImageAtlas
  , destroyImageAtlas
  , registerRgbaImage
  , lookupImage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import NanoUI (ImageId (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer)

newtype ImageAtlas = ImageAtlas (IORef (IM.IntMap (Ptr ())))

newImageAtlas :: IO ImageAtlas
newImageAtlas = ImageAtlas <$> newIORef IM.empty

destroyImageAtlas :: ImageAtlas -> IO ()
destroyImageAtlas (ImageAtlas ref) = do
  m <- readIORef ref
  mapM_ destroyTexture (IM.elems m)
  writeIORef ref IM.empty

maxImageDim :: Int
maxImageDim = 8192

registerRgbaImage :: Ptr SDL_Renderer -> ImageAtlas -> ImageId -> Int -> Int -> ByteString -> IO Bool
registerRgbaImage ren (ImageAtlas ref) (ImageId tid) w h pixels
  | tid <= 0 || w <= 0 || h <= 0 = pure False
  | w > maxImageDim || h > maxImageDim = pure False
  | BS.length pixels < w * h * 4 = pure False
  | otherwise =
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
          if not ok
            then pure False
            else do
              tex <- peek out
              m <- readIORef ref
              case IM.lookup tid m of
                Just old -> destroyTexture old
                Nothing -> pure ()
              writeIORef ref (IM.insert tid tex m)
              pure True

lookupImage :: ImageAtlas -> Int -> IO (Maybe (Ptr ()))
lookupImage (ImageAtlas ref) tid = IM.lookup tid <$> readIORef ref

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
