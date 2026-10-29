-- | Further native windows, declared from a view as an overlay is: the
-- backend opens one for each declaration a frame makes and closes it when
-- the declarations stop.
module NanoUI.Internal.ChildWindow
  ( nativeWindow
  , ChildWindow (..)
  , childWindows
  , childView
  ) where

import Data.IORef (modifyIORef', readIORef)
import NanoUI.Internal.Context.Types (ChildWindow (..), Context (..))
import NanoUI.Internal.Monad (NanoUI, askFrameInput, nextId, runNanoUI, withContext)
import NanoUI.Internal.NativeWindow.Types (WindowSettings)
import NanoUI.Internal.Var (readVar)
import NanoUI.Internal.Var.Types (Var)

-- | A separate native window running @body@ as its view, open while the
-- 'Var' holds 'True'. Call it unconditionally every frame, as for
-- 'NanoUI.modal': a frame that skips the call closes the window. The desktop's
-- close button, and 'NanoUI.quitUi' from @body@, write 'False'; with
-- 'NanoUI.wsExitOnCloseRequest' off, the close button sets
-- 'NanoUI.winCloseRequested' in @body@ instead.
--
-- The window has its own focus, widget state and positional 'NanoUI.useVar'
-- slots, and runs its own frames. It starts with this view's theme and the
-- settings, which take effect when it opens; set its title or theme later
-- from @body@ ('NanoUI.setWindowTitleUi', 'NanoUI.setUiTheme').
--
-- @body@ runs in the window's own frames. It may close over this view's
-- values: the window runs it again after each frame of this view. A 'Var'
-- either window writes shows in the other on its next frame. In an
-- emitting view ("NanoUI.Emit"), @body@ emits to the session's reducer.
-- There, to keep the open state in the model, pass a 'NanoUI.constVar',
-- turn 'NanoUI.wsExitOnCloseRequest' off, and emit a message from @body@
-- when 'NanoUI.winCloseRequested' is set.
--
-- The SDL and RGFW backends open the window; in tests nothing opens and
-- @body@ does not run.
nativeWindow :: Var Bool -> WindowSettings -> NanoUI () -> NanoUI ()
nativeWindow open settings body = do
  key <- nextId
  isOpen <- readVar open
  if isOpen
    then withContext $ \ctx ->
      modifyIORef' (ctxChildWindows ctx) (ChildWindow key open settings (\c inp -> runNanoUI c inp body) :)
    else pure ()

-- | The native windows the last view run declared open, in call order.
childWindows :: Context -> IO [ChildWindow]
childWindows ctx = reverse <$> readIORef (ctxChildWindows ctx)

-- | A declared window's view, to run as the view of its own frames.
childView :: ChildWindow -> NanoUI ()
childView child = askFrameInput >>= \inp -> withContext (\ctx -> childRun child ctx inp)
