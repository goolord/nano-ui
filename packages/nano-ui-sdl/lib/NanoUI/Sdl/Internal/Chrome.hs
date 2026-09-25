-- | Window management the toolkit does not need for itself: the title, the
-- size, and everything a window that draws its own title bar has to do in
-- its place.
--
-- A window without the desktop's title bar ('DecorationsFrame' or
-- 'DecorationsNone') has no buttons and no title of its own. 'windowCaption'
-- is the whole of the replacement: it draws the three buttons where it is
-- called, does what the first two of them say, and tells the desktop which
-- part of the bar drags the window and whether its edges resize it.
--
-- > titleBar :: NanoUI ()
-- > titleBar =
-- >   rowWith (fillW . fixedH captionBarHeight . alignMid) $ do
-- >     menus <- appMenus
-- >     flex
-- >     label "My App"
-- >     flex
-- >     closing <- windowCaption menus
-- >     when closing quit
module NanoUI.Sdl.Internal.Chrome
  ( -- * The window
    setWindowTitle
  , setWindowSize
  , minimizeWindow
  , maximizeWindow
  , restoreWindow
  , toggleMaximized
  , windowMaximized
  , windowResizable
  , WindowDecorations (..)
  , setWindowDecorations
  , setWindowShadow

    -- * A window that draws its own chrome
  , WindowChrome (..)
  , defaultWindowChrome
  , defaultResizeBorder
  , setWindowChrome
  , clearWindowChrome

    -- * From a view
  , CaptionOptions (..)
  , defaultCaptionOptions
  , windowCaption
  , windowCaptionWith
  , setWindowTitleUi
  , setWindowChromeUi
  , minimizeWindowUi
  , toggleMaximizedUi
  , windowMaximizedUi
  ) where

import Control.Monad (void, when)
import Data.Bits (zeroBits, (.&.))
import Data.Foldable (for_, traverse_)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text.Foreign qualified as TextForeign
import Effectful (Eff, type (:>))
-- The constructor under 'SDL_HitTestResult', which the callback returns.
import Foreign.C.Types (CUInt (..))
import Foreign.Ptr (FunPtr, castFunPtr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peekElemOff)
import NanoUI
import NanoUI.Monad (askHost)
import NanoUI.Sdl.Internal.Chrome.Types
import NanoUI.Sdl.Internal.Display (outPair)
import NanoUI.Sdl.Internal.Frame
import NanoUI.Sdl.Internal.Window (SdlEnv (..), windowZoom)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_HitTest (..), SDL_HitTestResult (..), SDL_WindowFlags)
import SDL3.Sys.Video qualified as SDL

--------------------------------------------------------------------------------
-- The window
--------------------------------------------------------------------------------

-- Everything here that makes Windows dispatch messages goes through the safe
-- binding rather than the unsafe one. Maximizing a window, resizing it or
-- putting it away all run the window's procedure before they return, and the
-- procedure reaches the hit test below; a callback into the runtime from
-- inside an unsafe call has nowhere to run and wedges the message pump.

-- | Set the window's title: what the taskbar and the window switcher show,
-- which a borderless window still has even with nowhere to write it.
setWindowTitle :: SdlEnv -> Text -> IO ()
setWindowTitle env title =
  TextForeign.withCString title $
    void . SDL.setWindowTitleSafe (sdlWindow env) . PtrConst.unsafeFromPtr

-- | Resize the view, in window coordinates. A window with the desktop's
-- frame ('DecorationsFrame') is made larger by the frame, as it was when it
-- opened, so the view is the size asked for.
setWindowSize :: SdlEnv -> Size -> IO ()
setWindowSize env (Size w h) = do
  (across, down) <- nativeFrameOutset (sdlWindow env)
  void $
    SDL.setWindowSizeSafe
      (sdlWindow env)
      (round w + fromIntegral across)
      (round h + fromIntegral down)

-- | Put the window away to the taskbar.
minimizeWindow :: SdlEnv -> IO ()
minimizeWindow env = void (SDL.minimizeWindowSafe (sdlWindow env))

-- | Fill the screen with the window.
maximizeWindow :: SdlEnv -> IO ()
maximizeWindow env = void (SDL.maximizeWindowSafe (sdlWindow env))

-- | Give the window back the size it had before it was maximized.
restoreWindow :: SdlEnv -> IO ()
restoreWindow env = void (SDL.restoreWindowSafe (sdlWindow env))

-- | Maximize a restored window, restore a maximized one: what the middle
-- caption button does. Which one it is is read off the window rather than
-- remembered, since the desktop maximizes a window by itself when its title
-- bar is double-clicked or dragged to the top of the screen.
toggleMaximized :: SdlEnv -> IO ()
toggleMaximized env = do
  maxed <- windowMaximized env
  if maxed then restoreWindow env else maximizeWindow env

-- | Whether the desktop has the window filling the screen.
windowMaximized :: SdlEnv -> IO Bool
windowMaximized env = hasFlag SDL.SDL_WINDOW_MAXIMIZED <$> windowFlagsOf env

-- | Whether the window may be resized at all.
windowResizable :: SdlEnv -> IO Bool
windowResizable env = hasFlag SDL.SDL_WINDOW_RESIZABLE <$> windowFlagsOf env

windowFlagsOf :: SdlEnv -> IO SDL_WindowFlags
windowFlagsOf = SDL.getWindowFlags . sdlWindow

hasFlag :: SDL_WindowFlags -> SDL_WindowFlags -> Bool
hasFlag bit flags = flags .&. bit /= zeroBits

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

-- | Say what the parts of a borderless window are for. The first call puts
-- the hit test on the window; later ones only change what it answers, so a
-- view may hand its regions over every frame for the cost of a write.
--
-- The rectangles are in the coordinates a frame is laid out in, and are
-- converted here by the window's zoom, which is what the desktop asks in.
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
  installed <- readIORef (chromeCallback st)
  when (isNothing installed) $ do
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
  pure $
    if edge /= SDL.SDL_HITTEST_NORMAL
      then edge
      else if any (`rectContains` V2 x y) drag then SDL.SDL_HITTEST_DRAGGABLE else SDL.SDL_HITTEST_NORMAL

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

-- | The caption of a window that draws its own: the three buttons, drawn
-- where this is called, and the rest of the title bar handed to the desktop
-- as the strip that drags the window.
--
-- @taken@ is the rectangles of whatever else in the bar takes a click of its
-- own (menu buttons, tabs); what is left of the bar between them drags the
-- window. The bar is taken to be as tall as the buttons and as wide as the
-- window, which is what a title bar is; a window whose edges resize it takes
-- those back at the very edge, since the desktop answers for an edge before
-- it answers for a drag. A maximized or fullscreen window, or one that
-- cannot be resized, has no edges to take hold of, and a fullscreen one has
-- no bar to drag it by either.
--
-- A window that still has the desktop's title bar ('DecorationsFull') gets
-- the buttons and nothing else: the desktop answers for its bar and edges.
--
-- It answers whether the window was asked to close, which is the one of the
-- three the application has to decide: the other two are the window's own
-- and are already done.
--
-- Nothing happens and nothing is drawn differently when the view is not
-- running on a window, so a view under a test context is laid out the same
-- way.
windowCaption :: Ui :> es => [Rect] -> Eff es Bool
windowCaption = windowCaptionWith defaultCaptionOptions

-- | What the caption hands the desktop, beside the buttons themselves.
data CaptionOptions = CaptionOptions
  { capButtons :: !CaptionConfig
  -- ^ How the three buttons are drawn.
  , capResizeBorder :: !Float
  -- ^ How far in from the left, right and bottom edges takes hold of one to
  -- resize the window ('chromeResizeBorder'). Zero leaves those three to the
  -- desktop's frame, if the window has one.
  , capResizeTop :: !Float
  -- ^ How far in from the top edge does ('chromeResizeTop').
  }

-- | 'defaultCaptionConfig' buttons and 'defaultResizeBorder' edges.
defaultCaptionOptions :: CaptionOptions
defaultCaptionOptions =
  CaptionOptions
    { capButtons = defaultCaptionConfig
    , capResizeBorder = defaultResizeBorder
    , capResizeTop = defaultResizeBorder
    }

-- | 'windowCaption' with buttons and edges of your own.
windowCaptionWith :: Ui :> es => CaptionOptions -> [Rect] -> Eff es Bool
windowCaptionWith opts taken = do
  menv <- askHost
  width <- windowWidth
  flags <- maybe (pure zeroBits) (uiIO . windowFlagsOf) menv
  let maxed = hasFlag SDL.SDL_WINDOW_MAXIMIZED flags
      fullscreen = hasFlag SDL.SDL_WINDOW_FULLSCREEN flags
      immovable = maxed || fullscreen || not (hasFlag SDL.SDL_WINDOW_RESIZABLE flags)
      -- The close button's corner is the window's, and a window that fills
      -- the screen has square ones.
      cfg
        | maxed || fullscreen = (capButtons opts) {capCornerRadius = 0}
        | otherwise = capButtons opts
  (action, buttons) <- captionButtonsConfigured cfg maxed
  for_ menv $ \env -> uiIO $ do
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
      Just CaptionMinimize -> minimizeWindow env
      Just CaptionToggleMaximize -> toggleMaximized env
      _ -> pure ()
  pure (action == Just CaptionClose)

-- | Set the window's title from within a view.
setWindowTitleUi :: Ui :> es => Text -> Eff es ()
setWindowTitleUi title = askHost >>= traverse_ (uiIO . (`setWindowTitle` title))

-- | Hand the window's chrome regions over from within a view. 'windowCaption'
-- does this for a view that draws the usual three buttons; this is for one
-- that draws something else.
setWindowChromeUi :: Ui :> es => WindowChrome -> Eff es ()
setWindowChromeUi chrome = askHost >>= traverse_ (uiIO . (`setWindowChrome` chrome))

-- | Put the window away from within a view.
minimizeWindowUi :: Ui :> es => Eff es ()
minimizeWindowUi = askHost >>= traverse_ (uiIO . minimizeWindow)

-- | Maximize or restore the window from within a view.
toggleMaximizedUi :: Ui :> es => Eff es ()
toggleMaximizedUi = askHost >>= traverse_ (uiIO . toggleMaximized)

-- | Whether the window fills the screen, from within a view. A view that is
-- not running on a window is answered 'False'.
windowMaximizedUi :: Ui :> es => Eff es Bool
windowMaximizedUi = askHost >>= maybe (pure False) (uiIO . windowMaximized)
