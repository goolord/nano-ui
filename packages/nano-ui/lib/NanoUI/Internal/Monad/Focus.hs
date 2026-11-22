
-- | Keyboard focus, the last laid-out rect, the clipboard, frame requests and
-- Escape, from inside a view.
module NanoUI.Internal.Monad.Focus
  ( lastRect
  , holdFocus
  , releaseFocus
  , focusedWidget
  , isFocused
  , requestFocus
  , focusNext
  , focusPrevious
  , clearFocus
  , askFocus
  , getClipboard
  , setClipboard
  , requestFrame
  , takeEscape
  )
where

import Control.Monad (when, unless)
import Data.IORef (writeIORef)
import Data.Text (Text)
import NanoUI.Internal.Context (intKey, Context (ctxClipboardSet, ctxFocusVisible, ctxFocusId, ctxFocusRequest, ctxClipboardGet), FocusRequest (..), getPrevRect, isDisabled, isInert, markDirty, recordLayoutRead, escapeReserved, markEscapeConsumed, markTabConsumed, pointerBlockedByModal, getFocusId)
import NanoUI.Internal.Id (hashWidgetId, WidgetId (..))
import NanoUI.Internal.Input (Key (KeyEscape), Pressable (pressedOnceIn))
import NanoUI.Internal.Monad.Core (NanoUI, withContext, askInput)
import NanoUI.Internal.Types (Rect)

-- | Where a widget was laid out last frame, or 'Nothing' before its first.
-- A widget that works out its own input -- a list that scrolls itself, a
-- text view hit-testing a click -- reads the rect it will be given from here,
-- since this frame's is solved only after the view has run:
--
-- > wid <- nextId
-- > rect <- fromMaybe (Rect 0 0 320 240) <$> lastRect wid
-- > ... work out this frame from rect and the input ...
-- > customWidgetWithId wid spec
--
-- When this frame's layout moves or resizes the widget, another frame
-- follows, so the view gets to read the new rect.
lastRect :: WidgetId -> NanoUI (Maybe Rect)
lastRect wid = withContext $ \ctx -> do
  r <- getPrevRect ctx wid
  recordLayoutRead ctx (intKey wid) ((/= r) <$> getPrevRect ctx wid)
  pure r

-- | Focus a widget immediately without showing the keyboard focus ring. Call
-- this each frame while a condition holds; no-op if already focused.
--
-- Consumes Tab for the current frame so focus does not advance and an editor
-- can use Tab to indent. A click elsewhere still moves focus away until the
-- next frame's call restores it.
--
-- Ignored when called outside an open 'NanoUI.modal'
-- or on a disabled or inert widget.
holdFocus :: WidgetId -> NanoUI ()
holdFocus wid = withContext $ \ctx -> do
  focus <- getFocusId ctx
  inert <- isInert ctx wid
  disabled <- isDisabled ctx wid
  blocked <- pointerBlockedByModal ctx
  unless (inert || disabled || blocked) $ do
    markTabConsumed ctx
    when (focus /= wid) $ do
      writeIORef (ctxFocusId ctx) wid
      writeIORef (ctxFocusVisible ctx) False

-- | Clear keyboard focus from @wid@ immediately if it is currently focused.
-- Unlike 'clearFocus' (which applies at the end of the frame and collapses
-- text selection and menus), this takes effect immediately for subsequent
-- widgets in the same pass and preserves text field selection and menu state.
releaseFocus :: WidgetId -> NanoUI ()
releaseFocus wid = withContext $ \ctx -> do
  focus <- getFocusId ctx
  when (focus == wid) (writeIORef (ctxFocusId ctx) (WidgetId 0))

-- | Currently focused widget ID, or @'WidgetId' 0@ for none.
focusedWidget :: NanoUI WidgetId
focusedWidget = withContext getFocusId

-- | Whether @wid@ currently has keyboard focus.
isFocused :: WidgetId -> NanoUI Bool
isFocused wid = (\focus -> hashWidgetId wid /= 0 && focus == wid) <$> focusedWidget

-- | Request moving keyboard focus to @wid@ at the end of the frame, as Tab
-- would. A text field resumes with its caret where it was (at the end if never
-- edited), the widget shows the focus ring, and Tab continues from it. The
-- previously focused field drops its selection and menu, an open dropdown
-- closes, and on the next frame the prior field observes focus loss (committing
-- a combo box or search field).
--
-- > resp <- searchInput' "Find" query
-- > findPressed <- shortcut (ctrl <> key 'f')
-- > when findPressed (requestFocus (respId resp))
--
-- The request is applied at the end of the frame, against that frame's
-- layout, so it can name a widget declared later, such as the next one via
-- 'currentId':
--
-- > whenM (shortcut (ctrl <> key 'l')) (requestFocus =<< currentId)
-- > textInput address
--
-- Focus moves on the next frame, which the request schedules, and the
-- scrollers around the widget scroll it into view, as for Tab. The last
-- request in a frame wins ('focusNext', 'focusPrevious' and 'clearFocus' are
-- requests too). The request is ignored for a widget Tab would not stop at
-- this frame: one that is disabled, behind an open
-- 'NanoUI.modal', or not declared this frame. A radio
-- group's response names its last option, which is not a Tab stop.
-- Requesting the widget that already has focus is a no-op, so a view can
-- request it every frame. @'WidgetId' 0@ means 'clearFocus'.
requestFocus :: WidgetId -> NanoUI ()
requestFocus wid = askFocus (if hashWidgetId wid == 0 then FocusNowhere else FocusOn wid)

-- | Move focus to the next Tab stop at the end of the frame ('requestFocus').
focusNext :: NanoUI ()
focusNext = askFocus FocusNext

-- | Move focus to the previous Tab stop at the end of the frame
-- ('requestFocus').
focusPrevious :: NanoUI ()
focusPrevious = askFocus FocusPrevious

-- | Drop focus at the end of the frame ('requestFocus'), as a click outside
-- any text field or select does. The focused field drops its selection and
-- menu. 'releaseFocus' instead acts at once on one widget.
clearFocus :: NanoUI ()
clearFocus = askFocus FocusNowhere

askFocus :: FocusRequest -> NanoUI ()
askFocus req = withContext (\ctx -> writeIORef (ctxFocusRequest ctx) (Just req))

-- | The clipboard's text, through whatever clipboard the backend installed.
-- 'Nothing' for an empty clipboard or none at all.
getClipboard :: NanoUI (Maybe Text)
getClipboard = withContext ctxClipboardGet

-- | Put text on the clipboard. 'False' when the backend could not.
setClipboard :: Text -> NanoUI Bool
setClipboard txt = withContext (\ctx -> ctxClipboardSet ctx txt)

-- | Schedule a full-window repaint on the next frame, for external state
-- changes not tracked by 'NanoUI.Var.Var'. Avoid calling unconditionally every
-- frame, as that prevents the event loop from sleeping; use
-- 'NanoUI.wakeAfter' for timed frames.
requestFrame :: NanoUI ()
requestFrame = withContext markDirty

-- | Consume an Escape press this frame and return 'True' if it was available.
-- Returns 'False' if Escape was not pressed, was already consumed earlier in
-- the frame, is reserved by an open text-field menu or dropdown (including a
-- focused combo box), or is blocked by an open modal.
takeEscape :: NanoUI Bool
takeEscape = do
  inp <- askInput
  if not (pressedOnceIn KeyEscape inp)
    then pure False
    else withContext $ \ctx -> do
      reserved <- escapeReserved ctx inp
      blocked <- pointerBlockedByModal ctx
      let ours = not reserved && not blocked
      when ours (markEscapeConsumed ctx)
      pure ours

