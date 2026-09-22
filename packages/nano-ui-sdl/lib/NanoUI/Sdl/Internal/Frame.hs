{-# LANGUAGE CPP #-}

-- | How much of the desktop's own decoration a window keeps, and the frame
-- and shadow that go under a window that draws its own title bar.
--
-- A borderless window on Windows is a popup: it has no frame, so the desktop
-- draws no shadow under it, and there is nothing outside its edges to take
-- hold of. Every pixel the pointer can reach is one the view has drawn on,
-- and a window whose edges resize it has to spend some of its own chrome on
-- saying so.
--
-- 'DecorationsFrame' gives both back. The ordinary window frame goes back on
-- and stays outside the client area -- invisible, outside the window you can
-- see, and what the desktop resizes the window by -- and brings the
-- desktop's shadow with it.
--
-- The window procedure the frame needs is in @cbits\/nano_ui_frame.c@ rather
-- than here: Windows calls it from inside @SetWindowPos@, @DefWindowProc@
-- and the modal loop a border drag runs in, and a Haskell callback reached
-- that way re-enters the runtime from a foreign call already running on the
-- thread.
--
-- Elsewhere the compositor decides: the frame and the shadow do nothing, and
-- a window with less than 'DecorationsFull' is a borderless one.
module NanoUI.Sdl.Internal.Frame
  ( WindowDecorations (..)
  , applyDecorations
  , applyWindowShadow
  , nativeFrameOutset
  ) where

import Control.Monad (void, when)
import Data.Bits ((.&.), (.|.))
import Foreign.Ptr (Ptr)
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Video qualified as SDL

#if defined(mingw32_HOST_OS)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import NanoUI.Sdl.Internal.Display (outPair)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (sDL_PROP_WINDOW_WIN32_HWND_POINTER)
import SDL3.Sys.Properties (getPointerProperty)
#endif

-- | How much of the desktop's own decoration a window keeps.
data WindowDecorations
  = DecorationsFull
  -- ^ The desktop's title bar and frame: an ordinary window.
  | DecorationsFrame
  -- ^ The desktop's frame without its title bar, for a view that draws its
  -- own ('NanoUI.Backend.Sdl.windowCaption'). On Windows the frame is never
  -- drawn and takes nothing from the view: it lies outside the window you
  -- can see, it is what the desktop resizes the window by, and it carries
  -- the desktop's drop shadow. The window is made that much larger, so
  -- 'NanoUI.Backend.Sdl.sdlWindowSize' is still the size of the view. A
  -- window that cannot be resized, or is fullscreen, has no use for the
  -- edges and is given only the shadow. Elsewhere this is a borderless
  -- window, and the compositor decides about the shadow.
  | DecorationsNone
  -- ^ Nothing of the desktop's: no title bar, no frame and no shadow.
  -- 'NanoUI.Backend.Sdl.setWindowShadow' puts the desktop's shadow under it
  -- on Windows.
  deriving (Eq, Show)

-- | Give a window the decorations asked for, from whatever it had before.
--
-- Safe calls only: turning the border on and off and putting the frame on
-- and off all run the window's procedure before they return.
applyDecorations :: Ptr SDL_Window -> WindowDecorations -> IO ()
applyDecorations win decorations = do
  -- Take off what an earlier call put on, so that any of these can follow
  -- any other.
  applyNativeFrame win False
  applyWindowShadow win False
  void (SDL.setWindowBorderedSafe win (decorations == DecorationsFull))
  when (decorations == DecorationsFrame) $ do
    flags <- SDL.getWindowFlags win
    let movable =
          flags .&. (SDL.SDL_WINDOW_RESIZABLE .|. SDL.SDL_WINDOW_FULLSCREEN) == SDL.SDL_WINDOW_RESIZABLE
    -- A frame on a window that cannot be resized by it is only a margin
    -- that takes clicks meant for whatever is behind the window; the shadow
    -- is all such a window wants of it. A window with the frame has the
    -- desktop's shadow with it, so there is nothing to ask DWM for; asking
    -- anyway extends the frame over the topmost row of the view, which is a
    -- row the view draws its own border on.
    if movable then applyNativeFrame win True else applyWindowShadow win True

-- | Put the desktop's own frame back under a borderless window, or take it
-- away again.
--
-- The frame stays where the desktop puts it, the same width in on every
-- side, around the outside of the window you can see. Nothing of it is drawn
-- and nothing of the view gives way to it: the window is made that much
-- larger instead, so the view keeps the size it was asked for. What the
-- frame is for is the edges -- the desktop resizes the window by them, from
-- outside the view, the way it does for every window with a title bar of its
-- own.
--
-- It is also what keeps the desktop's rounded corners off the view. The
-- corners the desktop rounds and smooths are the window's, and with the
-- frame between them and the view, the only thing at the view's own corners
-- is the border it draws there itself.
applyNativeFrame :: Ptr SDL_Window -> Bool -> IO ()

-- | Put the desktop's shadow under a window, or take it away again.
applyWindowShadow :: Ptr SDL_Window -> Bool -> IO ()

-- | How much larger than its view the desktop's frame makes a window, across
-- and down, in window coordinates: nothing for a window without the frame,
-- or one maximized or fullscreen. SDL sizes a borderless window as though
-- its client area were the whole of it, so a size asked of SDL adds this.
nativeFrameOutset :: Ptr SDL_Window -> IO (Int, Int)

#if defined(mingw32_HOST_OS)
applyNativeFrame win on =
  withHwnd win () $ \hwnd -> do
    nanoUiSetNativeFrame hwnd (if on then 1 else 0)
    -- The desktop draws a line of its own around a window that has a frame,
    -- and along the top, where the client area reaches the edge of the
    -- window, it falls on the view's first row. The view draws its own
    -- border there, so the desktop's is taken off.
    setWindowWord hwnd dwmwaBorderColor (if on then dwmwaColorNone else dwmwaColorDefault)
    -- And it rounds the window's corners. Those corners are the frame's, not
    -- the view's -- the view is held a frame's width inside them, so its own
    -- corners are square whatever the desktop does there, and a rounding
    -- that only shapes the shadow leaves the shadow round a window that is
    -- not. Square, the shadow follows the window it belongs to, and the
    -- corners are the view's to draw.
    setWindowWord hwnd dwmwaCornerPreference (if on then dwmwcpDoNotRound else dwmwcpDefault)

applyWindowShadow win on =
  withHwnd win () $ \hwnd -> do
    -- DWM hangs the shadow off the non-client area, which a popup window is
    -- not rendered with unless it is asked for. Off goes back to what the
    -- window's style says rather than to never, which would take the shadow
    -- and border from a window that has a frame of its own.
    setWindowWord hwnd dwmwaNcRenderingPolicy (if on then dwmncrpEnabled else dwmncrpUseWindowStyle)
    -- A frame extended into the client area is what makes DWM treat the
    -- window as one that has a frame at all. One pixel along the top is the
    -- least that does, and the view paints over it.
    withArray (if on then [0, 0, 1, 0] else [0, 0, 0, 0]) (void . dwmExtendFrameIntoClientArea hwnd)

nativeFrameOutset win =
  withHwnd win (0, 0) $ \hwnd -> do
    ((), across, down) <- outPair (nanoUiNativeFrameOutset hwnd)
    pure (fromIntegral across, fromIntegral down)

-- | Safe, not unsafe: this one puts the window procedure on and takes it off
-- again, and Windows dispatches messages from inside both.
foreign import ccall safe "nano_ui_set_native_frame"
  nanoUiSetNativeFrame :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "nano_ui_native_frame_outset"
  nanoUiNativeFrameOutset :: Ptr () -> Ptr CInt -> Ptr CInt -> IO ()

-- | Run an action on the window's @HWND@, or answer @none@ for a window that
-- has none.
withHwnd :: Ptr SDL_Window -> a -> (Ptr () -> IO a) -> IO a
withHwnd win none act = do
  props <- SDL.getWindowProperties win
  hwnd <- BS.useAsCString sDL_PROP_WINDOW_WIN32_HWND_POINTER $ \name ->
    getPointerProperty props (PtrConst.unsafeFromPtr name) nullPtr
  if hwnd == nullPtr then pure none else act (castPtr hwnd)

-- | Set one of the desktop's window attributes that takes a word. The border
-- colour and the corner preference are both Windows 11's. An older Windows
-- answers that it has never heard of them and goes on drawing what it would
-- have drawn, which is no worse than what it drew before either existed.
setWindowWord :: Ptr () -> Word32 -> Word32 -> IO ()
setWindowWord hwnd attribute value =
  with value $ \p -> void (dwmSetWindowAttribute hwnd attribute (castPtr p) 4)

-- | @DWMWA_BORDER_COLOR@ and the colour that says to draw no border, and
-- @DWMWA_WINDOW_CORNER_PREFERENCE@ and the two roundings used here.
dwmwaBorderColor, dwmwaColorNone, dwmwaColorDefault :: Word32
dwmwaBorderColor = 34
dwmwaColorNone = 0xFFFFFFFE
dwmwaColorDefault = 0xFFFFFFFF

dwmwaCornerPreference, dwmwcpDefault, dwmwcpDoNotRound :: Word32
dwmwaCornerPreference = 33
dwmwcpDefault = 0
dwmwcpDoNotRound = 1

foreign import ccall unsafe "DwmExtendFrameIntoClientArea"
  dwmExtendFrameIntoClientArea :: Ptr () -> Ptr CInt -> IO Int32

foreign import ccall unsafe "DwmSetWindowAttribute"
  dwmSetWindowAttribute :: Ptr () -> Word32 -> Ptr () -> Word32 -> IO Int32

-- | @DWMWA_NCRENDERING_POLICY@, and the two policies it is set to here.
dwmwaNcRenderingPolicy :: Word32
dwmwaNcRenderingPolicy = 2

dwmncrpUseWindowStyle, dwmncrpEnabled :: Word32
dwmncrpUseWindowStyle = 0
dwmncrpEnabled = 2
#else
applyNativeFrame _ _ = pure ()
applyWindowShadow _ _ = pure ()
nativeFrameOutset _ = pure (0, 0)
#endif
