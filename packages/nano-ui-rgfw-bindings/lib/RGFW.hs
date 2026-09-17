-- | Haskell wrappers over the RGFW C API used by nano-ui-rgfw: OpenGL windows,
-- event polling and waiting, window size and scale, cursors, and the
-- clipboard. Re-exports "RGFW.Raw".
module RGFW
  ( Window (..)
  , Event (..)
  , createWindowGL
  , swapBuffersGL
  , closeWindow
  , pollEvent
  , waitForEvent
  , withEventBuffer
  , windowSize
  , windowScale
  , setMouseStandard
  , setMouseDefault
  , readClipboardText
  , writeClipboardText
  -- Re-exports
  , module RGFW.Raw
  ) where

import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Foreign as TF
import Data.Word (Word8, Word32)
import Foreign.C.String (withCString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUChar (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import RGFW.Raw

newtype Window = Window (Ptr RGFW_window)
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
  | EventScaleUpdate !Float !Float
  | EventWindowClose
  | EventOther !Word8
  deriving (Eq, Show)

-- | Create a window with a core-profile OpenGL context of at least the
-- given major/minor version made current on the calling OS thread. 'Nothing'
-- when the window or the context cannot be created.
createWindowGL :: String -> Int -> Int -> Int -> Int -> Word32 -> Int -> Int -> IO (Maybe Window)
createWindowGL title x y w h flags major minor =
  withCString title $ \cTitle -> do
    ptr <-
      c_rgfw_create_window_gl cTitle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
        (fromIntegral flags) (fromIntegral major) (fromIntegral minor)
    pure (if ptr == nullPtr then Nothing else Just (Window ptr))

swapBuffersGL :: Window -> IO ()
swapBuffersGL (Window w) = c_RGFW_window_swapBuffers_OpenGL w

closeWindow :: Window -> IO ()
closeWindow (Window ptr) = c_RGFW_window_close ptr

-- | Wait for pending events. A negative timeout blocks indefinitely, 0 returns
-- immediately, and a positive timeout polls the display connection for up to
-- that many milliseconds before returning.
waitForEvent :: Int -> IO ()
waitForEvent t = c_RGFW_waitForEvent (fromIntegral t)

withEventBuffer :: (Ptr RGFW_event -> IO a) -> IO a
withEventBuffer f = do
  sz <- c_rgfw_event_size
  allocaBytes (fromIntegral sz) f

-- | Take the next queued event; 'EventNone' only when the queue is empty.
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
            -- An invalid code point is still an event: reporting EventNone
            -- would read as an empty queue and stall the rest of the batch.
            if (cInt >= 0 && cInt <= 0x10FFFF) && not (cInt >= 0xD800 && cInt <= 0xDFFF)
              then pure (EventKeyChar (chr cInt))
              else pure (EventOther t)
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
          | t == rgfw_scaleUpdated -> do
            sx <- c_rgfw_event_scale_x evPtr
            sy <- c_rgfw_event_scale_y evPtr
            pure (EventScaleUpdate (realToFrac sx) (realToFrac sy))
          | t == rgfw_windowClose ->
            pure EventWindowClose
          | otherwise ->
            pure (EventOther t)

windowSize :: Window -> IO (Int, Int)
windowSize (Window w) = do
  CInt width <- c_rgfw_window_w w
  CInt height <- c_rgfw_window_h w
  pure (fromIntegral width, fromIntegral height)

windowScale :: Window -> IO Float
windowScale (Window w) = do
  CFloat s <- c_rgfw_window_scale w
  pure s

setMouseStandard :: Window -> Word8 -> IO Bool
setMouseStandard (Window win) icon = do
  CUChar res <- c_rgfw_window_set_mouse_standard win (CUChar icon)
  pure (res /= 0)

setMouseDefault :: Window -> IO Bool
setMouseDefault (Window win) = do
  CUChar res <- c_rgfw_window_set_mouse_default win
  pure (res /= 0)

-- | The system clipboard's text, if it holds any. Needs an open window; on
-- X11 it waits for the selection owner to convert the data. Invalid UTF-8
-- decodes to replacement characters.
readClipboardText :: IO (Maybe Text)
readClipboardText =
  alloca $ \lenPtr -> do
    ptr <- c_rgfw_read_clipboard_text lenPtr
    CSize len <- peek lenPtr
    if ptr == nullPtr || len == 0
      then pure Nothing
      else do
        txt <- decodeUtf8Lenient <$> unsafePackCStringLen (ptr, fromIntegral len)
        pure (if T.null txt then Nothing else Just txt)

-- | Replace the system clipboard with text (UTF-8); 'False' if refused.
writeClipboardText :: Text -> IO Bool
writeClipboardText txt =
  TF.withCStringLen txt $ \(ptr, len) -> do
    CUChar ok <- c_rgfw_write_clipboard_text ptr (fromIntegral len)
    pure (ok /= 0)
