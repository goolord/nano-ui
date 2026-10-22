-- | Hit-test regions for custom window chrome, shared between the chrome API
-- and the SDL window lifecycle.
module NanoUI.Sdl.Internal.Chrome.Types
  ( WindowChrome (..)
  , defaultWindowChrome
  , defaultResizeBorder
  , HitTestCallback
  , ChromeState (..)
  , newChromeState
  , clearChromeState
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import NanoUI (Rect (..))
import NanoUI.Sdl.Internal.Frame (WindowDecorations)
import SDL3.Sys.Bindgen.Video (SDL_HitTestResult (..))

-- | Hit-test regions for a borderless window. A natively decorated window does
-- not use this, since the window manager handles its title bar and resize
-- borders directly.
data WindowChrome = WindowChrome
  { chromeDrag :: ![Rect]
  -- ^ Client-area rectangles that act as a draggable title bar, in layout
  -- coordinates. Because the window manager handles the drag, these regions
  -- also support aero snap, double-click to maximize, and the system context
  -- menu. 'NanoUI.dragSpans' computes these rectangles from a title bar row
  -- and its interactive child widgets.
  , chromeResizeBorder :: !Float
  -- ^ Resize hit-test thickness along the left, right, and bottom edges.
  -- Set to zero when edge resizing is disabled (including when maximized).
  , chromeResizeTop :: !Float
  -- ^ Resize hit-test thickness along the top edge. Under
  -- 'NanoUI.Backend.Sdl.DecorationsFrame', the native sizing frame extends
  -- outside the left, right, and bottom edges, but not the top (because
  -- Windows paints a top frame inset as a caption). The top resize border must
  -- therefore live inside the client area.
  --
  -- When both 'chromeResizeBorder' and 'chromeResizeTop' are zero, all resize
  -- hit testing is disabled and any outer frame area is treated as normal
  -- client area.
  }
  deriving (Eq, Show)

-- | Empty chrome configuration with no drag regions and no resize borders.
defaultWindowChrome :: WindowChrome
defaultWindowChrome = WindowChrome [] 0 0

-- | Six layout units (wider than a standard 4px native border so it is easier
-- to grab inside the client area).
defaultResizeBorder :: Float
defaultResizeBorder = 6

-- | @SDL_HitTest@: the window, the point being tested (an @SDL_Point@ of two
-- @CInt@s), and the user data pointer registered with the callback.
type HitTestCallback = Ptr () -> Ptr CInt -> Ptr () -> IO SDL_HitTestResult

-- | Active hit-test state read by the SDL hit-test callback. Because the window
-- manager queries hit testing from inside the event pump before pointer events
-- reach the application, each frame writes the current regions here (converted
-- to window coordinates once when set) for the callback to read.
data ChromeState = ChromeState
  { chromeRegions :: !(IORef WindowChrome)
  , chromeCallback :: !(IORef (Maybe (FunPtr HitTestCallback)))
  , chromeFromView :: !(IORef Bool)
  -- ^ Whether a view handed the regions over in the frame being drawn, or
  -- else in the one before: they lapse after a frame whose view did not.
  , chromeDecorations :: !(IORef WindowDecorations)
  -- ^ The decorations the window has, which asking for again does nothing.
  , chromeShadow :: !(IORef (Maybe Bool))
  -- ^ The shadow last asked for, or 'Nothing' since the decorations, which
  -- set it, last changed.
  }

-- | The state of a window opened with the given decorations.
newChromeState :: WindowDecorations -> IO ChromeState
newChromeState decorations =
  ChromeState
    <$> newIORef defaultWindowChrome
    <*> newIORef Nothing
    <*> newIORef False
    <*> newIORef decorations
    <*> newIORef Nothing

-- | Release the marshalled callback. The session acquires this state before
-- the window and so releases it after the window is destroyed: nothing can
-- ask again once this has run.
clearChromeState :: ChromeState -> IO ()
clearChromeState st = atomicModifyIORef' (chromeCallback st) (Nothing,) >>= mapM_ freeHaskellFunPtr
