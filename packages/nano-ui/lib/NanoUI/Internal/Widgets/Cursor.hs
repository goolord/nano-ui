-- | The pointer shape over part of a view. The frame resolves it after layout
-- ("NanoUI.Internal.Frame.Cursor"), against the nodes the scope declared.
module NanoUI.Internal.Widgets.Cursor
  ( withCursorShape
  ) where

import Control.Monad (when)
import Data.IORef (modifyIORef')
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Input (UiCursorKind)
import NanoUI.Internal.Layout.Arena (arenaCount)
import NanoUI.Internal.Monad (Ui, askContext, uiIO)

-- | Show @shape@ while the pointer is over the widgets declared inside, and
-- over the empty space in the containers declared inside; the gaps of the
-- container around the scope keep their cursor. A widget with a cursor of its
-- own keeps it, so a button inside still shows the pointer and a text field
-- the I-beam; a custom widget whose 'NanoUI.Widgets.Custom.widgetCursor'
-- answers 'UiCursorDefault' has no opinion, and shows the scope's. The
-- innermost scope around a widget wins, so an inner
-- @withCursorShape UiCursorDefault@ gives its part the arrow back, and
-- 'UiCursorHidden' hides the pointer over its part. Disabled
-- widgets have none of their own, so a scope around a 'NanoUI.disabledWhen'
-- shows its shape over them:
--
-- > withCursorShape UiCursorCrosshair (drawing (fixedWH 320 200) plot)
-- > withCursorShape UiCursorNotAllowed (disabledWhen locked (button "Delete"))
--
-- The scope takes no widget id and adds no layout node, so wrapping part of a
-- view in it moves no ids and changes no layout. Windows and popups declared
-- inside show the shape too. Only the pointer's position counts: a drag that
-- leaves the widgets leaves the shape behind.
withCursorShape :: Ui :> es => UiCursorKind -> Eff es a -> Eff es a
withCursorShape !shape body = do
  ctx <- askContext
  let na = ctxNodeArena ctx
  -- The arena appends nodes in declaration order, so the scope's nodes, and
  -- theirs, are the ones added while the body runs.
  !start <- uiIO (arenaCount na)
  result <- body
  uiIO $ do
    !end <- arenaCount na
    when (end > start) $
      modifyIORef' (ctxCursorRegions ctx) ((start, end, shape) :)
  pure result
