-- | Support for windows that draw their own title bar. Title, size, mode and
-- maximizing work on any backend and live in the core
-- ('NanoUI.setWindowTitleUi', 'NanoUI.resizeWindowUi',
-- 'NanoUI.toggleMaximizedUi', ...).
--
-- A window without the desktop's title bar ('DecorationsFrame' or
-- 'DecorationsNone') has no native caption buttons or drag bar. 'windowCaption'
-- replaces them: it draws the minimize, maximize, and close buttons, handles
-- minimize and maximize clicks directly, and registers the remaining title bar
-- spans and window edges for native drag and resize hit testing.
--
-- > titleBar :: NanoUI ()
-- > titleBar =
-- >   rowWith (fillW . fixedH captionBarHeight . alignMid) $ do
-- >     menus <- appMenus
-- >     flex
-- >     label "My App"
-- >     flex
-- >     closing <- windowCaption menus
-- >     when closing quitUi
module NanoUI.Sdl.Internal.Chrome
  ( -- * The window
    windowResizable
  , WindowDecorations (..)
  , setWindowDecorations
  , setWindowDecorationsUi
  , setWindowShadow
  , setWindowShadowUi

    -- * A window that draws its own chrome
  , WindowChrome (..)
  , defaultWindowChrome
  , defaultResizeBorder
  , setWindowChrome
  , clearWindowChrome
  , clearWindowChromeUi

    -- * From a view
  , CaptionOptions (..)
  , defaultCaptionOptions
  , windowCaption
  , windowCaptionWith
  , setWindowChromeUi
  ) where

import Control.Monad (void, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isNothing)
-- The constructor under 'SDL_HitTestResult', which the callback returns.
import Foreign.C.Types (CUInt (..))
import Foreign.Ptr (FunPtr, castFunPtr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peekElemOff)
import NanoUI
import NanoUI.Sdl.Internal.Chrome.Types
import NanoUI.Sdl.Internal.Display (outPair)
import NanoUI.Sdl.Internal.Frame
import NanoUI.Sdl.Internal.Window (SdlEnv (..), windowZoom, withSdlEnv)
import SDL3.Sys.Bindgen.Video (SDL_HitTest (..), SDL_HitTestResult (..), SDL_WindowFlags)
import SDL3.Sys.Video qualified as SDL

--------------------------------------------------------------------------------
-- The window
--------------------------------------------------------------------------------

-- Everything here that makes Windows dispatch messages goes through the safe
-- binding rather than the unsafe one, as in "NanoUI.Sdl.Internal.WindowOptions".
-- Changing the decorations or the hit test runs the window procedure
-- synchronously, which calls the hit test below. A callback into the runtime
-- from inside an unsafe call cannot run and wedges the message pump.

-- | Whether the window may be resized at all.
windowResizable :: SdlEnv -> IO Bool
windowResizable env = hasFlag SDL.SDL_WINDOW_RESIZABLE <$> windowFlagsOf env

windowFlagsOf :: SdlEnv -> IO SDL_WindowFlags
windowFlagsOf = SDL.getWindowFlags . sdlWindow

-- | Change how much of the desktop's decoration the window keeps, from
-- whatever 'NanoUI.Backend.Sdl.sdlWindowDecorations' or an earlier call
-- gave it. Going back to 'DecorationsFull' also takes off whatever
-- 'setWindowChrome' put on the window: the desktop's own title bar is the
-- desktop's to answer for.
setWindowDecorations :: SdlEnv -> WindowDecorations -> IO ()
setWindowDecorations env decorations = do
  when (decorations == DecorationsFull) (clearWindowChrome env)
  applyDecorations (sdlWindow env) decorations

-- | Put the desktop's drop shadow under the window, or take it away: for a
-- 'DecorationsNone' window that wants one. A 'DecorationsFrame' window has
-- the desktop's shadow already.
setWindowShadow :: SdlEnv -> Bool -> IO ()
setWindowShadow env = applyWindowShadow (sdlWindow env)

--------------------------------------------------------------------------------
-- A window that draws its own chrome
--------------------------------------------------------------------------------

-- | Register hit-test regions for a borderless window. The first call installs
-- the SDL hit-test callback on the window; subsequent calls only update the
-- stored regions, so a view can call this every frame for the cost of an
-- 'IORef' write.
--
-- The rectangles are in layout coordinates and are scaled here by the window's
-- zoom to match the window coordinates used by the hit-test callback.
setWindowChrome :: SdlEnv -> WindowChrome -> IO ()
setWindowChrome env chrome = do
  zoom <- windowZoom env
  writeIORef
    (chromeRegions st)
    chrome
      { chromeDrag = map (scaleRect zoom) (chromeDrag chrome)
      , chromeResizeBorder = chromeResizeBorder chrome * zoom
      , chromeResizeTop = chromeResizeTop chrome * zoom
      }
  whenM (isNothing <$> readIORef (chromeCallback st)) $ do
    callback <- mkHitTest (hitTest st)
    writeIORef (chromeCallback st) (Just callback)
    void (SDL.setWindowHitTestSafe (sdlWindow env) (SDL_HitTest (castFunPtr callback)) nullPtr)
  where
    st = sdlChromeState env
    scaleRect z (Rect x y w h) = Rect (x * z) (y * z) (w * z) (h * z)

-- | Take the hit test off the window again: every part of it goes back to
-- being the application's.
clearWindowChrome :: SdlEnv -> IO ()
clearWindowChrome env = do
  void (SDL.setWindowHitTestSafe (sdlWindow env) (SDL_HitTest nullFunPtr) nullPtr)
  writeIORef (chromeRegions st) defaultWindowChrome
  clearChromeState st
  where
    st = sdlChromeState env

foreign import ccall "wrapper"
  mkHitTest :: HitTestCallback -> IO (FunPtr HitTestCallback)

-- | What is under the pointer, as far as the desktop is concerned: an edge
-- that resizes the window, a part of it that drags the window, or nothing in
-- particular, which is everything the view itself draws.
--
-- The edges are answered first, so the topmost pixels of a title bar resize
-- the window rather than reaching whatever is drawn there. That is the
-- bargain a window with its own chrome makes, and the reason
-- 'NanoUI.captionBarHeight' leaves room for it.
hitTest :: ChromeState -> HitTestCallback
hitTest st win area _ = do
  x <- fromIntegral <$> peekElemOff area 0
  y <- fromIntegral <$> peekElemOff area 1
  WindowChrome drag border top <- readIORef (chromeRegions st)
  (ok, pw, ph) <- outPair (SDL.getWindowSize (castPtr win))
  let edge
        -- With no edges at all, even a point in the desktop's frame outside
        -- the view is answered as part of it.
        | ok && (border > 0 || top > 0) =
            edgeHit (x < border) (x >= fromIntegral pw - border) (y < top) (y >= fromIntegral ph - border)
        | otherwise = SDL.SDL_HITTEST_NORMAL
      answer
        | edge /= SDL.SDL_HITTEST_NORMAL = edge
        | any (`rectContains` V2 x y) drag = SDL.SDL_HITTEST_DRAGGABLE
        | otherwise = SDL.SDL_HITTEST_NORMAL
  pure answer

-- | The @SDL_HitTestResult@ for an edge or corner, from the sides of the
-- window the point is near.
edgeHit :: Bool -> Bool -> Bool -> Bool -> SDL_HitTestResult
edgeHit left right top bottom
  | top && left = SDL.SDL_HITTEST_RESIZE_TOPLEFT
  | top && right = SDL.SDL_HITTEST_RESIZE_TOPRIGHT
  | top = SDL.SDL_HITTEST_RESIZE_TOP
  | bottom && right = SDL.SDL_HITTEST_RESIZE_BOTTOMRIGHT
  | bottom && left = SDL.SDL_HITTEST_RESIZE_BOTTOMLEFT
  | bottom = SDL.SDL_HITTEST_RESIZE_BOTTOM
  | left = SDL.SDL_HITTEST_RESIZE_LEFT
  | right = SDL.SDL_HITTEST_RESIZE_RIGHT
  | otherwise = SDL.SDL_HITTEST_NORMAL

--------------------------------------------------------------------------------
-- From a view
--------------------------------------------------------------------------------

-- | Draw the minimize, maximize, and close caption buttons and register the
-- remaining title bar area as the window's drag region.
--
-- @taken@ lists the rectangles of other interactive widgets in the title bar
-- (such as menu buttons or tabs); the spans of the bar between them become
-- draggable. The title bar is assumed to span the full window width at the
-- vertical position and height of the caption buttons. On a resizable
-- borderless window, resize hit testing takes precedence along the outer edges;
-- when maximized, fullscreen, or non-resizable, edge resizing is disabled (and
-- dragging is also disabled in fullscreen).
--
-- On a natively decorated window ('DecorationsFull'), only the buttons are
-- drawn and no custom hit-test regions are registered.
--
-- Returns 'True' when the close button is clicked (minimize and maximize are
-- handled directly).
--
-- Outside an SDL session, the buttons still draw and dispatch core window
-- requests, and no drag region is registered.
windowCaption :: [Rect] -> NanoUI Bool
windowCaption = windowCaptionWith defaultCaptionOptions

-- | Options for 'windowCaptionWith': button styling and resize border widths.
data CaptionOptions = CaptionOptions
  { capButtons :: !CaptionConfig
  -- ^ How the three buttons are drawn.
  , capResizeBorder :: !Float
  -- ^ Resize hit-test thickness along the left, right, and bottom edges
  -- ('chromeResizeBorder'). Zero leaves those edges to the native sizing frame
  -- if present.
  , capResizeTop :: !Float
  -- ^ Resize hit-test thickness along the top edge ('chromeResizeTop').
  }

-- | 'defaultCaptionConfig' buttons and 'defaultResizeBorder' edges.
defaultCaptionOptions :: CaptionOptions
defaultCaptionOptions =
  CaptionOptions
    { capButtons = defaultCaptionConfig
    , capResizeBorder = defaultResizeBorder
    , capResizeTop = defaultResizeBorder
    }

-- | 'windowCaption' with custom 'CaptionOptions'.
windowCaptionWith :: CaptionOptions -> [Rect] -> NanoUI Bool
windowCaptionWith opts taken = do
  width <- windowWidth
  -- Maximized and fullscreen come from the state every backend reports; only
  -- handing the drag region over needs the SDL session.
  win <- askWindow
  let maxed = winMaximized win
      fullscreen = winFullscreen win
      -- The close button's corner is the window's, and a window that fills
      -- the screen has square ones.
      cfg
        | maxed || fullscreen = (capButtons opts) {capCornerRadius = 0}
        | otherwise = capButtons opts
  (action, buttons) <- captionButtonsConfigured cfg maxed
  withSdlEnv () $ \env -> do
    flags <- windowFlagsOf env
    let immovable = maxed || fullscreen || not (hasFlag SDL.SDL_WINDOW_RESIZABLE flags)
    when (hasFlag SDL.SDL_WINDOW_BORDERLESS flags) $
      setWindowChrome
        env
        WindowChrome
          { chromeDrag =
              if fullscreen
                then []
                else dragSpans (Rect 0 (rectY buttons) width (rectH buttons)) (buttons : taken)
          , chromeResizeBorder = if immovable then 0 else capResizeBorder opts
          , chromeResizeTop = if immovable then 0 else capResizeTop opts
          }
  case action of
    Just CaptionMinimize -> minimizeWindowUi
    Just CaptionToggleMaximize -> toggleMaximizedUi
    _ -> pure ()
  pure (action == Just CaptionClose)

-- | Update the window's chrome hit-test regions from within a view.
-- 'windowCaption' calls this automatically when drawing standard caption
-- buttons; use this directly for custom title bar layouts.
setWindowChromeUi :: WindowChrome -> NanoUI ()
setWindowChromeUi chrome = withSdlEnv () (`setWindowChrome` chrome)

-- | 'clearWindowChrome' from a view.
clearWindowChromeUi :: NanoUI ()
clearWindowChromeUi = withSdlEnv () clearWindowChrome

-- | 'setWindowDecorations' from a view.
setWindowDecorationsUi :: WindowDecorations -> NanoUI ()
setWindowDecorationsUi d = withSdlEnv () (`setWindowDecorations` d)

-- | 'setWindowShadow' from a view.
setWindowShadowUi :: Bool -> NanoUI ()
setWindowShadowUi on = withSdlEnv () (`setWindowShadow` on)
