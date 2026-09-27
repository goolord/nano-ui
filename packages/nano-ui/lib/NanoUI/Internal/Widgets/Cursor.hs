-- | The pointer shape over part of a view. The frame resolves it after layout
-- ("NanoUI.Internal.Frame.Cursor"), against the nodes the scope declared.
module NanoUI.Internal.Widgets.Cursor
  ( withCursorShape
  ) where

import Control.Monad (when)
import Data.IORef (modifyIORef')
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Input (UiCursorKind)
import NanoUI.Internal.Monad (NanoUI, askContext, withArenaRange)

-- | Show @shape@ while the pointer is over the widgets and containers declared
-- inside. Gaps in the enclosing container keep their own cursor.
--
-- Widgets with their own cursor keep it: a button still shows the hand and a
-- text field the I-beam. A custom widget whose
-- 'NanoUI.Widgets.Custom.widgetCursor' returns 'UiCursorDefault' shows the
-- scope's shape. Disabled widgets have no cursor of their own, so they show
-- the scope's shape too. The innermost scope wins, so a nested
-- @withCursorShape UiCursorDefault@ restores the arrow, and 'UiCursorHidden'
-- hides the pointer.
--
-- > withCursorShape UiCursorCrosshair (drawing (fixedWH 320 200) plot)
-- > withCursorShape UiCursorNotAllowed (disabledWhen locked (button "Delete"))
--
-- The scope takes no widget id and adds no layout node, so wrapping a view in
-- it changes neither ids nor layout. Windows and popups declared inside also
-- get the shape. Only the pointer position matters: a drag that leaves the
-- scope's widgets loses the shape.
withCursorShape :: UiCursorKind -> NanoUI a -> NanoUI a
withCursorShape !shape body = do
  ctx <- askContext
  withArenaRange
    (\start end -> when (end > start) $ modifyIORef' (ctxCursorRegions ctx) ((start, end, shape) :))
    body
