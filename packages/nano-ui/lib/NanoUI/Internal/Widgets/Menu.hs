-- | Menu rows, menu-bar buttons, and context-menu state for application views.
module NanoUI.Internal.Widgets.Menu
  ( contextMenu
  , contextMenuArea
  , useContextMenu
  , menuButton
  , menuButton'
  , menuButtonWith
  , menuButtonWith'
  , menuItem
  , menuItem'
  , menuItemShortcut
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  )
where

import Control.Monad (void, when)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context (..), getStore, intKey, modifyStore)
import NanoUI.Internal.Font (menuItemPadX, menuItemRowH, menuMinW, menuOuterPad, menuSepH, widgetContentInset)
import NanoUI.Internal.Input (inputMousePos, inputMouseReleased)
import NanoUI.Internal.Monad (Ui, askContext, askDefaultLayout, askInput, freshWidget, uiIO)
import NanoUI.Internal.Store (Slot (..), fieldPoint, findSlot, flagSlot, insertSlot, setFlagSlot, slotKey)
import NanoUI.Internal.Style (Layout (..), Padding (..), defaultLayout, fillW, fixedH, fontMuted, gap, minW, padXY, tight)
import NanoUI.Internal.Types (PopupAnchor (..), PopupPlacement (..), V2 (..))
import NanoUI.Internal.WidgetText (buttonFlagMenu, buttonFlagMenuBar)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Layout (columnWith, labelEx, rowWith, separator)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Widgets.Node (HasResponse, Response (..), containerResponse, inertResponse, respClicked, respHovered, respRightClicked)
import NanoUI.Internal.Widgets.Popup (PopupConfig (..), defaultPopupConfig, popup)

-- | A context menu for any widget response, opened by right-clicking it.
-- Returns the menu body's result while the menu is open.
contextMenu ::
  (Ui :> es, HasResponse r) =>
  r ->
  Eff es a ->
  Eff es (Maybe a)
contextMenu target child = do
  menu <- useContextMenu
  runContextMenu menu (respRightClicked target) (const child)

-- | A container whose right-click opens a context menu. The menu body
-- receives the position it was opened at.
contextMenuArea ::
  Ui :> es =>
  (Layout -> Layout) ->
  Eff es a ->
  (V2 -> Eff es b) ->
  Eff es (a, Maybe b)
contextMenuArea f areaContent menuContent = do
  menu <- useContextMenu
  base <- askDefaultLayout
  (areaRes, areaResp) <- containerResponse NodeContainer (f base) areaContent
  (,) areaRes <$> runContextMenu menu (respRightClicked areaResp) menuContent

-- | Open the menu at the pointer on a right click, show it while open, and
-- close it once a row is picked or it is dismissed.
runContextMenu ::
  Ui :> es =>
  (Bool, V2, V2 -> Eff es (), Eff es ()) ->
  Bool ->
  (V2 -> Eff es a) ->
  Eff es (Maybe a)
runContextMenu (isOpen0, pos0, openAt, close) rightClick child = do
  inp <- askInput
  let mouse = inputMousePos inp
      pos = if rightClick then mouse else pos0
      cfg = (defaultPopupConfig (AnchorPoint pos)) {cfgPlacement = PlacementAtCursor, cfgOffset = 0}
  when rightClick (openAt mouse)
  -- A release the menu itself sees, which is one on a row, picks.
  (popupResp, mBody) <-
    popup (isOpen0 || rightClick) cfg $
      (,) . inputMouseReleased <$> askInput <*> columnWith (tight . gap 0) (child pos)
  let picked = respHovered popupResp && maybe False fst mBody
  when (respClicked popupResp || picked) close
  pure (snd <$> mBody)

-- | Open state for a context menu you position yourself: whether it is open,
-- where it was opened, an action to open it at a point, and one to close it.
useContextMenu ::
  Ui :> es =>
  Eff es (Bool, V2, V2 -> Eff es (), Eff es ())
useContextMenu = do
  (wid, ctx) <- freshWidget
  let key = intKey wid
      openK = slotKey SlotMenuOpen key
      posK = slotKey SlotMenuPos key
  store <- uiIO (getStore ctx)
  let (px, py) = findSlot fieldPoint (0, 0) posK store
      openAt (V2 x y) = uiIO (modifyStore ctx (setFlagSlot openK True . insertSlot fieldPoint posK (x, y)))
      close = uiIO (modifyStore ctx (setFlagSlot openK False))
  pure (flagSlot openK store, V2 px py, openAt, close)

-- | Render a menu row, with an optional shortcut hint after the label,
-- returning its full 'Response'. A disabled row is a muted label, not a
-- disabled button: hover tracking does not know a button's enabled flag and
-- would still highlight it. Text nodes ignore padding, so the
-- label sits in a container that reproduces an enabled row's geometry: the
-- 'menuItemRowH' height and 'menuMinW' width, the label inset 'menuItemPadX'
-- plus the button's content inset, and the same total horizontal gutter the
-- solver reserves for menu buttons. Its response never reports interaction.
menuItemWith :: Ui :> es => Text -> Maybe Text -> Bool -> Eff es Response
menuItemWith lbl hint enabled
  | enabled = buttonStyledEx True text 0 menuRowLayout buttonFlagMenu
  | otherwise = do
      ctx <- askContext
      let (ix, _) = widgetContentInset (ctxFontMetrics ctx)
          padLeft = menuItemPadX + ix
          padRight = max 0 (2 * (menuOuterPad + menuItemPadX) - padLeft)
          rowLayout = (minW menuMinW defaultLayout) {layoutPadding = Padding padLeft padRight 0 0}
      (_, resp) <-
        containerResponse NodeContainer rowLayout $
          labelEx (fixedH menuItemRowH . tight . fontMuted $ defaultLayout) text
      pure (inertResponse resp)
  where
    text = maybe lbl (\s -> mconcat [lbl, "  ", s]) hint

-- | Menu row. 'True' on the frame it is clicked.
--
-- > whenM (menuItem "Open...") openFile
{-# INLINE menuItem #-}
menuItem :: Ui :> es => Text -> Eff es Bool
menuItem txt = respClicked <$> menuItem' txt

{-# INLINE menuItem' #-}
-- | 'menuItem' returning its response; activation is in @respClicked@.
menuItem' :: Ui :> es => Text -> Eff es Response
menuItem' txt = menuItemWith txt Nothing True

-- | Menu row with a shortcut hint after the label. The hint is only text;
-- handle the key itself elsewhere.
--
-- > whenM (menuItemShortcut "Save" "Ctrl+S") saveFile
menuItemShortcut :: Ui :> es => Text -> Text -> Eff es Bool
menuItemShortcut txt hint = respClicked <$> menuItemWith txt (Just hint) True

-- | Dimmed menu row that cannot be clicked.
menuItemDisabled :: Ui :> es => Text -> Eff es ()
menuItemDisabled txt = void (menuItemWith txt Nothing False)

-- | Row layout shared by menu items, matching the text-field context menu:
-- 28px rows and a 148px minimum menu width (@menuItemRowH@ and @menuMinW@ in
-- @NanoUI.Internal.Font@).
menuRowLayout :: Layout
menuRowLayout = minW menuMinW . fixedH menuItemRowH . tight . fillW $ defaultLayout

-- | Menu-bar title: a flat, label-sized button. @open@ tints the title while
-- its drop-down is showing. 'True' on the frame it is clicked.
{-# INLINE menuButton #-}
menuButton :: Ui :> es => Text -> Bool -> Eff es Bool
menuButton txt open = respClicked <$> menuButton' txt open

-- | 'menuButton' returning its 'Response', whose rect anchors the drop-down.
menuButton' :: Ui :> es => Text -> Bool -> Eff es Response
menuButton' = menuButtonWith' id

-- | 'menuButton' with modified layout. A menu bar whose row is taller than a
-- label gives its titles 'NanoUI.fillH', so that each one covers the bar it
-- is in and its text sits in the middle of it rather than at the top.
{-# INLINE menuButtonWith #-}
menuButtonWith :: Ui :> es => (Layout -> Layout) -> Text -> Bool -> Eff es Bool
menuButtonWith f txt open = respClicked <$> menuButtonWith' f txt open

-- | 'menuButtonWith' returning its 'Response'.
menuButtonWith' :: Ui :> es => (Layout -> Layout) -> Text -> Bool -> Eff es Response
menuButtonWith' f txt open =
  buttonStyledEx True txt (if open then 1 else 0) (f (tight defaultLayout)) buttonFlagMenuBar

-- | Separator line inside a context menu, matching the text-field context
-- menu painter exactly: a 1px rule inset 'menuItemPadX' from the panel edge
-- (the popup already contributes 'menuOuterPad', the row adds the remainder)
-- centered in a 'menuSepH' band (@lineY = bandY + h\/2@ via 4.5px vertical
-- padding around a zero-height content box). The rule sits in a 'tight'
-- column so it stays horizontal ('separator' adapts to its parent's
-- direction and would grow vertically inside the padded row) and so the
-- default 3px container padding does not inset or stretch it.
menuSeparator :: Ui :> es => Eff es ()
menuSeparator = do
  rowWith (fixedH menuSepH . padXY (menuItemPadX - menuOuterPad) 4.5 . fillW) $
    columnWith (tight . fillW) separator

-- | Header / category title inside a context menu.
menuHeader :: Ui :> es => Text -> Eff es ()
menuHeader txt =
  void (labelEx (padXY 6 2 defaultLayout) txt)
