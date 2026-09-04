module RGFW
  ( Window (..)
  , Surface (..)
  , Event (..)
  , initRGFW
  , deinitRGFW
  , createWindow
  , closeWindow
  , pollEvent
  , withEventBuffer
  , createSurface
  , blitSurface
  , freeSurface
  , windowSize
  -- Re-exports
  , module RGFW.Raw
  ) where

import Data.Char (chr)
import Data.Word (Word8, Word32)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt (..), CUChar (..), CUInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import RGFW.Raw

newtype Window = Window (Ptr RGFW_window)
  deriving (Eq, Show)

newtype Surface = Surface (Ptr RGFW_surface)
  deriving (Eq, Show)

data Event
  = EventNone
  | EventKeyPress !Word32 !Word8
  | EventKeyRelease !Word32 !Word8
  | EventKeyChar !Char
  | EventMouseButton !Word8 !Bool -- Button, Pressed
  | EventMouseMotion !Int !Int
  | EventMouseScroll !Float !Float
  | EventWindowResize !Int !Int
  | EventWindowClose
  | EventOther !Word8
  deriving (Eq, Show)

createWindow :: String -> Int -> Int -> Int -> Int -> Word32 -> IO (Maybe Window)
createWindow title x y w h flags =
  withCString title $ \cTitle -> do
    ptr <- c_RGFW_createWindow cTitle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (fromIntegral flags)
    if ptr == nullPtr
      then pure Nothing
      else pure (Just (Window ptr))

closeWindow :: Window -> IO ()
closeWindow (Window ptr) = c_RGFW_window_close ptr

withEventBuffer :: (Ptr RGFW_event -> IO a) -> IO a
withEventBuffer f = do
  sz <- c_rgfw_event_size
  allocaBytes (fromIntegral sz) f

pollEvent :: Window -> Ptr RGFW_event -> IO Event
pollEvent (Window win) evPtr = do
  hasEv <- c_RGFW_window_checkEvent win evPtr
  if hasEv == 0
    then pure EventNone
    else do
      CUChar t <- c_rgfw_event_type evPtr
      case t of
        _ | t == rgfw_keyPressed -> do
            CUInt val <- c_rgfw_event_key_value evPtr
            CUChar m <- c_rgfw_event_key_mod evPtr
            pure (EventKeyPress val m)
          | t == rgfw_keyReleased -> do
            CUInt val <- c_rgfw_event_key_value evPtr
            CUChar m <- c_rgfw_event_key_mod evPtr
            pure (EventKeyRelease val m)
          | t == rgfw_keyChar -> do
            CUInt val <- c_rgfw_event_keyChar_value evPtr
            let !cInt = fromIntegral val :: Int
            if (cInt >= 0 && cInt <= 0x10FFFF) && not (cInt >= 0xD800 && cInt <= 0xDFFF)
              then pure (EventKeyChar (chr cInt))
              else pure EventNone
          | t == rgfw_mouseButtonPressed -> do
            CUChar b <- c_rgfw_event_button_value evPtr
            pure (EventMouseButton b True)
          | t == rgfw_mouseButtonReleased -> do
            CUChar b <- c_rgfw_event_button_value evPtr
            pure (EventMouseButton b False)
          | t == rgfw_mouseMotion -> do
            CInt mx <- c_rgfw_event_mouse_x evPtr
            CInt my <- c_rgfw_event_mouse_y evPtr
            pure (EventMouseMotion (fromIntegral mx) (fromIntegral my))
          | t == rgfw_mouseScroll -> do
            dx <- c_rgfw_event_delta_x evPtr
            dy <- c_rgfw_event_delta_y evPtr
            pure (EventMouseScroll (realToFrac dx) (realToFrac dy))
          | t == rgfw_windowResized -> do
            CInt uw <- c_rgfw_event_update_w evPtr
            CInt uh <- c_rgfw_event_update_h evPtr
            pure (EventWindowResize (fromIntegral uw) (fromIntegral uh))
          | t == rgfw_windowClose ->
            pure EventWindowClose
          | otherwise ->
            pure (EventOther t)

initRGFW :: String -> IO Bool
initRGFW name = withCString name $ \cName -> do
  res <- c_rgfw_init cName
  pure (res == 0)

deinitRGFW :: IO ()
deinitRGFW = c_rgfw_deinit

createSurface :: Window -> Ptr Word8 -> Int -> Int -> Word8 -> IO Surface
createSurface (Window win) ptr w h fmt = do
  s <- c_RGFW_createSurface win (castPtr ptr) (fromIntegral w) (fromIntegral h) (CUChar fmt)
  pure (Surface s)

blitSurface :: Window -> Surface -> IO ()
blitSurface (Window w) (Surface s) = c_RGFW_window_blitSurface w s

freeSurface :: Surface -> IO ()
freeSurface (Surface s) = c_RGFW_surface_free s

windowSize :: Window -> IO (Int, Int)
windowSize (Window w) = do
  CInt width <- c_rgfw_window_w w
  CInt height <- c_rgfw_window_h w
  pure (fromIntegral width, fromIntegral height)
