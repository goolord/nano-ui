-- | What a window that draws its own chrome hands to the desktop, shared
-- between the chrome API and the SDL window lifecycle.
module NanoUI.Sdl.Chrome.Types
  ( WindowChrome (..)
  , defaultWindowChrome
  , defaultResizeBorder
  , HitTestCallback
  , HitTestFunPtr
  , ChromeState (..)
  , newChromeState
  , clearChromeState
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import NanoUI (Rect (..))
import SDL3.Sys.Bindgen.Video (SDL_HitTestResult (..))

-- | What the parts of a borderless window are for. A window with a border of
-- its own needs none of this: the desktop already knows where its title bar
-- and its edges are.
data WindowChrome = WindowChrome
  { chromeDrag :: ![Rect]
  -- ^ The parts of the window that drag it, in the coordinates a frame is
  -- laid out in. Dragging is the desktop's, so these also snap the window to
  -- the sides of the screen, maximize it on a double click, and hang the
  -- window menu off the right button. 'NanoUI.dragSpans' works these out
  -- from a title bar row and the widgets in it.
  , chromeResizeBorder :: !Float
  -- ^ How far in from the left, right and bottom edges takes hold of one.
  -- Zero for a window that should not resize by its edges, which includes
  -- every maximized one.
  , chromeResizeTop :: !Float
  -- ^ How far in from the top edge does, which is on its own because the
  -- top of a window is. A window given the desktop's frame
  -- ('NanoUI.Backend.Sdl.DecorationsFrame') has it outside its sides and
  -- its bottom, where there is room to take hold of the window without
  -- reaching into the view at all; the top has none, since the desktop
  -- paints that strip as a caption, so what is inside the view is all the
  -- top edge has and it has to be deep enough to aim at.
  --
  -- With both at zero the window has no edges at all: the desktop's frame
  -- is asked about through the same test, and answered as part of the view.
  }

-- | No drag region and no resize border: a window that answers for nothing.
defaultWindowChrome :: WindowChrome
defaultWindowChrome = WindowChrome [] 0 0

-- | Six layout units. A bordered window's sizing border is four, which is a
-- thing to aim at rather than a thing to hit.
defaultResizeBorder :: Float
defaultResizeBorder = 6

-- | @SDL_HitTest@: the window, the point being asked about (an @SDL_Point@,
-- which is two ints), and the data the callback was registered with.
type HitTestCallback = Ptr () -> Ptr CInt -> Ptr () -> IO SDL_HitTestResult

type HitTestFunPtr = FunPtr HitTestCallback

-- | A window's chrome as the hit test reads it. The desktop asks what is
-- under the pointer from inside the event pump, before the press reaches
-- anyone, so the answer cannot be passed in at the call: the frame leaves it
-- here and the callback reads it.
--
-- The regions are kept in window coordinates, converted once when they are
-- set rather than on every question.
data ChromeState = ChromeState
  { chromeRegions :: !(IORef WindowChrome)
  , chromeCallback :: !(IORef (Maybe HitTestFunPtr))
  }

newChromeState :: IO ChromeState
newChromeState = ChromeState <$> newIORef defaultWindowChrome <*> newIORef Nothing

-- | Release the marshalled callback. The session acquires this state before
-- the window and so releases it after the window is destroyed: nothing can
-- ask again once this has run.
clearChromeState :: ChromeState -> IO ()
clearChromeState st = do
  cb <- readIORef (chromeCallback st)
  writeIORef (chromeCallback st) Nothing
  maybe (pure ()) freeHaskellFunPtr cb
