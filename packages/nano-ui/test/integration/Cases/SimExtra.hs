-- | Sim helpers the input, keyboard and focus cases share.
module Cases.SimExtra
  ( above
  , cursorAt
  , pointerCursor
  , focusOn
  , focusId
  , tap
  ) where

import Spec
import GHC.Stack (HasCallStack)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Shortcut (Shortcut)
import NanoUI.Testing.Sim

-- | Check that the value is greater than @lo@.
above :: (HasCallStack, Ord b, Show b) => b -> b -> Sim a ()
above lo x = checkGt x lo

-- | The cursor after a frame hovering the target.
cursorAt :: HasCallStack => Target -> Sim a UiCursorKind
cursorAt t = moveTo t >> pointerCursor

-- | The cursor the last frame left.
pointerCursor :: Sim a UiCursorKind
pointerCursor = do
  ctx <- simContext
  liftIO . uiCursorKind ctx =<< getInput

-- | 'chord' with its key let go after the frame. 'chord' leaves the key
-- held, so the same chord again would be an auto-repeat, not a new press.
tap :: Shortcut -> Sim a a
tap s = chord s <* modifyInput (\i -> i {inputKeysHeld = mempty})

-- | The focused widget.
focusId :: Sim a WidgetId
focusId = simContext >>= liftIO . getFocusId

-- | Give @wid@ keyboard focus directly, as focus retained from earlier does.
focusOn :: WidgetId -> Sim a ()
focusOn wid = simContext >>= \ctx -> liftIO (writeIORef (ctxFocusId ctx) wid)
