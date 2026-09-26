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
  , stopWaitForEvent
  , withEventBuffer
  , physicalToMappedKey
  , windowSize
  , windowScale
  , setMouseStandard
  , setMouseDefault
  , readClipboardText
  , writeClipboardText
  , setWindowIcon
  , setWindowMinSize
  , setWindowMaxSize
  , moveWindow
  , centerWindow
  -- Re-exports
  , module RGFW.Raw
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCString)
import Data.Char (chr)
import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Foreign qualified as TF
import Data.Word (Word32, Word8)
import Foreign.C.String (withCString)
import Foreign.C.Types (CSize (..), CUChar (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import RGFW.Raw

-- | Owned native window. Close exactly once with 'closeWindow'; there is no
-- finalizer. Window and OpenGL operations belong on the creating OS thread.
newtype Window = Window (Ptr RGFW_window)
  deriving (Eq, Show)

-- | A copied native event. Key events carry RGFW key codes of physical keys
-- ('physicalToMappedKey' maps them to the layout) and modifier bits;
-- motion/resize coordinates are native window pixels, not nano-ui logical units.
data Event
  = EventNone
  | EventKeyPress !Word32 !Word8
  | EventKeyRepeat !Word32 !Word8
  -- ^ The auto-repeat of a held key.
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
createWindowGL ::
  String -> Int -> Int -> Int -> Int -> Word32 -> Int -> Int -> IO (Maybe Window)
createWindowGL title x y w h flags major minor =
  withCString title $ \cTitle -> do
    ptr <-
      c_rgfw_create_window_gl
        cTitle
        (fromIntegral x)
        (fromIntegral y)
        (fromIntegral w)
        (fromIntegral h)
        (fromIntegral flags)
        (fromIntegral major)
        (fromIntegral minor)
    pure (if ptr == nullPtr then Nothing else Just (Window ptr))

-- | Present the OpenGL back buffer. The window's context must be current.
swapBuffersGL :: Window -> IO ()
swapBuffersGL (Window w) = c_RGFW_window_swapBuffers_OpenGL w

-- | Destroy the window and its context. Invalidates the handle immediately.
closeWindow :: Window -> IO ()
closeWindow (Window ptr) = c_RGFW_window_close ptr

-- | Wait for pending events. A negative timeout blocks indefinitely, 0 returns
-- immediately, and a positive timeout polls the display connection for up to
-- that many milliseconds before returning.
waitForEvent :: Int -> IO ()
waitForEvent t = c_RGFW_waitForEvent (fromIntegral t)

-- | End a 'waitForEvent' that is waiting, or make the next one return at
-- once: how another thread wakes a loop blocked on events. Safe to call from
-- any thread once a window exists.
stopWaitForEvent :: IO ()
stopWaitForEvent = c_RGFW_stopCheckEvents

-- | Allocate native event storage for a callback. The pointer must not escape
-- the callback; reuse it for consecutive 'pollEvent' calls.
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
        _
          | t == rgfw_keyPressed || t == rgfw_keyReleased -> do
              CUInt val <- c_rgfw_event_key_value evPtr
              CUChar m <- c_rgfw_event_key_mod evPtr
              CUChar rep <- c_rgfw_event_key_repeat evPtr
              let ctor
                    | t == rgfw_keyReleased = EventKeyRelease
                    | rep /= 0 = EventKeyRepeat
                    | otherwise = EventKeyPress
              pure (ctor val m)
          | t == rgfw_keyChar -> do
              CUInt val <- c_rgfw_event_keyChar_value evPtr
              let
                !cInt = fromIntegral val :: Int
              -- An invalid code point is still an event: reporting EventNone
              -- would read as an empty queue and stall the rest of the batch.
              if (cInt >= 0 && cInt <= 0x10FFFF) && not (cInt >= 0xD800 && cInt <= 0xDFFF)
                then pure (EventKeyChar (chr cInt))
                else pure (EventOther t)
          | t == rgfw_mouseButtonPressed || t == rgfw_mouseButtonReleased -> do
              CUChar b <- c_rgfw_event_button_value evPtr
              pure (EventMouseButton b (t == rgfw_mouseButtonPressed))
          | t == rgfw_mouseMotion ->
              liftA2
                (EventMouseMotion `on` fromIntegral)
                (c_rgfw_event_mouse_x evPtr)
                (c_rgfw_event_mouse_y evPtr)
          | t == rgfw_mouseScroll ->
              liftA2
                (EventMouseScroll `on` realToFrac)
                (c_rgfw_event_delta_x evPtr)
                (c_rgfw_event_delta_y evPtr)
          | t == rgfw_windowResized ->
              liftA2
                (EventWindowResize `on` fromIntegral)
                (c_rgfw_event_update_w evPtr)
                (c_rgfw_event_update_h evPtr)
          | t == rgfw_scaleUpdated ->
              liftA2
                (EventScaleUpdate `on` realToFrac)
                (c_rgfw_event_scale_x evPtr)
                (c_rgfw_event_scale_y evPtr)
          | t == rgfw_windowClose ->
              pure EventWindowClose
          | otherwise ->
              pure (EventOther t)

-- | The key code a physical key has in the current keyboard layout: on an
-- AZERTY layout the key in QWERTY's Q place is 'rgfw_keyA'. 'Nothing' when
-- the layout gives it none that RGFW names. Needs an open window.
physicalToMappedKey :: Word32 -> IO (Maybe Word32)
physicalToMappedKey key
  | key > 0xFF = pure Nothing
  | otherwise = do
      CUChar mapped <- c_RGFW_physicalToMappedKey (CUChar (fromIntegral key))
      pure (if mapped == 0 then Nothing else Just (fromIntegral mapped))

-- | Current native window width and height in pixels.
windowSize :: Window -> IO (Int, Int)
windowSize (Window w) = liftA2 ((,) `on` fromIntegral) (c_rgfw_window_w w) (c_rgfw_window_h w)

-- | Current display scale reported by RGFW, with 1 meaning unscaled.
windowScale :: Window -> IO Float
windowScale (Window w) = realToFrac <$> c_rgfw_window_scale w

-- | Select an RGFW standard cursor code. Returns 'False' if the request fails.
setMouseStandard :: Window -> Word8 -> IO Bool
setMouseStandard (Window win) icon = (/= 0) <$> c_rgfw_window_set_mouse_standard win (CUChar icon)

-- | Restore the default cursor. Returns 'False' if the request fails.
setMouseDefault :: Window -> IO Bool
setMouseDefault (Window win) = (/= 0) <$> c_rgfw_window_set_mouse_default win

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
  TF.withCStringLen txt $ \(ptr, len) ->
    (/= 0) <$> c_rgfw_write_clipboard_text ptr (fromIntegral len)

-- | Give the window (and its taskbar entry) an icon: @w@ by @h@ pixels of
-- tightly packed RGBA bytes, rows from the top. RGFW copies them. 'False'
-- when the size is not positive, the bytes are too few, or RGFW refuses.
setWindowIcon :: Window -> Int -> Int -> ByteString -> IO Bool
setWindowIcon (Window win) w h pixels
  | w <= 0 || h <= 0 || BS.length pixels < w * h * 4 = pure False
  | otherwise =
      unsafeUseAsCString pixels $ \p ->
        (/= 0) <$> c_RGFW_window_setIcon win (castPtr p) (fromIntegral w) (fromIntegral h) (CUChar rgfw_formatRGBA8)

-- | The smallest size the user may give the window, in native pixels; an
-- axis of zero has no limit.
setWindowMinSize :: Window -> Int -> Int -> IO ()
setWindowMinSize (Window win) w h = c_RGFW_window_setMinSize win (fromIntegral w) (fromIntegral h)

-- | The largest size the user may give the window, in native pixels; an
-- axis of zero has no limit.
setWindowMaxSize :: Window -> Int -> Int -> IO ()
setWindowMaxSize (Window win) w h = c_RGFW_window_setMaxSize win (fromIntegral w) (fromIntegral h)

-- | Move the window's top-left corner to a point on the desktop, in pixels.
moveWindow :: Window -> Int -> Int -> IO ()
moveWindow (Window win) x y = c_RGFW_window_move win (fromIntegral x) (fromIntegral y)

-- | Centre the window on its monitor.
centerWindow :: Window -> IO ()
centerWindow (Window win) = c_RGFW_window_center win
