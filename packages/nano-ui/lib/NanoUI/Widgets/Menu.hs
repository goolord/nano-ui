{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Menu
  ( contextMenu
  , contextMenuArea
  , useContextMenu
  , menuButton
  , menuButton'
  , MenuItem (..)
  , menuItemWith
  , menuItem
  , menuItem'
  , menuItemShortcut
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  )
where

import Control.Monad (void, when)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (getStore, intKey, modifyStore)
import NanoUI.Font (menuItemPadX, menuItemRowH, menuMinW, menuOuterPad, menuSepH)
import NanoUI.Input (inputMousePos, inputMouseReleased)
import NanoUI.Monad (Ui, askContext, askDefaultLayout, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), slotKey, Slot (..))
import NanoUI.Style (Layout (..), defaultLayout, fillW, fixedH, fontMuted, gap, minW, padXY, tight)
import NanoUI.Types (PopupAnchor (..), PopupPlacement (..), V2 (..))
import NanoUI.WidgetText (buttonFlagMenu, buttonFlagMenuBar)
import NanoUI.Widgets.Combinators (buttonStyled)
import NanoUI.Widgets.Layout (columnWith, labelEx, rowWith, separator)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Widgets.Node (HasResponse, Response (..), containerResponse, respClicked, respHovered, respRightClicked)
import NanoUI.Widgets.Popup (PopupConfig (..), popup)

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
      cfg =
        PopupConfig
          { cfgAnchor = AnchorPoint pos
          , cfgPlacement = PlacementAtCursor
          , cfgDismissable = True
          , cfgOffset = 0
          }
  when rightClick (openAt mouse)
  (popupResp, mBody) <- popup (isOpen0 || rightClick) cfg (columnWith (tight . gap 0) (child pos))
  let picked = respHovered popupResp && inputMouseReleased inp
  when (respClicked popupResp || picked) close
  pure mBody

-- | Open state for a context menu you position yourself: whether it is open,
-- where it was opened, an action to open it at a point, and one to close it.
useContextMenu ::
  Ui :> es =>
  Eff es (Bool, V2, V2 -> Eff es (), Eff es ())
useContextMenu = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
      openK = slotKey SlotMenuOpen key
      posK = slotKey SlotMenuPos key
  store <- uiIO (getStore ctx)
  let isOpen = IM.findWithDefault 0 openK (storeInt store) /= 0
      (px, py) = IM.findWithDefault (0, 0) posK (storePoint store)
      openAt (V2 x y) =
        uiIO $
          modifyStore ctx $ \st ->
            st
              { storeInt = IM.insert openK 1 (storeInt st)
              , storePoint = IM.insert posK (x, y) (storePoint st)
              }
      close = uiIO $ modifyStore ctx $ \st -> st {storeInt = IM.delete openK (storeInt st)}
  pure (isOpen, V2 px py, openAt, close)

-- | One context-menu row; the whole row is the button.
data MenuItem = MenuItem
  { menuItemLabel :: !Text
  , menuItemHint :: !(Maybe Text)
    -- ^ Shortcut hint shown after the label, e.g. @Ctrl+S@.
  , menuItemEnabled :: !Bool
    -- ^ Disabled rows are dimmed and cannot be clicked or focused.
  }
  deriving (Eq, Show)

-- | Render a menu row, returning its full 'Response'. A disabled row is a
-- muted label, not a disabled button: hover tracking does not know a button's
-- enabled flag and would still highlight it. Its response never reports
-- interaction.
menuItemWith :: Ui :> es => MenuItem -> Eff es Response
menuItemWith (MenuItem lbl hint enabled)
  | enabled = buttonStyled text 0 menuRowLayout buttonFlagMenu
  | otherwise = do
      resp <- labelEx (tight . fillW . fontMuted $ defaultLayout) text
      pure
        resp
          { rawRespHovered = False
          , rawRespPressed = False
          , rawRespClicked = False
          , rawRespRightPressed = False
          , rawRespRightClicked = False
          }
  where
    text = maybe lbl (\s -> mconcat [lbl, "  ", s]) hint

-- | Menu row. 'True' on the frame it is clicked.
--
-- @
-- whenM (menuItem "Open...") openFile
-- @
{-# INLINE menuItem #-}
menuItem :: Ui :> es => Text -> Eff es Bool
menuItem txt = respClicked <$> menuItem' txt

{-# INLINE menuItem' #-}
menuItem' :: Ui :> es => Text -> Eff es Response
menuItem' txt = menuItemWith (MenuItem txt Nothing True)

-- | Menu row with a shortcut hint after the label. The hint is only text;
-- handle the key itself elsewhere.
--
-- @
-- whenM (menuItemShortcut "Save" "Ctrl+S") saveFile
-- @
menuItemShortcut :: Ui :> es => Text -> Text -> Eff es Bool
menuItemShortcut txt hint = respClicked <$> menuItemWith (MenuItem txt (Just hint) True)

-- | Dimmed menu row that cannot be clicked.
menuItemDisabled :: Ui :> es => Text -> Eff es ()
menuItemDisabled txt = void (menuItemWith (MenuItem txt Nothing False))

-- | Row layout shared by menu items, matching the text-field context menu:
-- 28px rows and a 148px minimum menu width (@menuItemRowH@ and @menuMinW@ in
-- @NanoUI.Font@).
menuRowLayout :: Layout
menuRowLayout = minW menuMinW . fixedH menuItemRowH . tight . fillW $ defaultLayout

-- | Menu-bar title: a flat, label-sized button. @open@ tints the title while
-- its drop-down is showing. 'True' on the frame it is clicked.
{-# INLINE menuButton #-}
menuButton :: Ui :> es => Text -> Bool -> Eff es Bool
menuButton txt open = respClicked <$> menuButton' txt open

-- | 'menuButton' returning its 'Response', whose rect anchors the drop-down.
menuButton' :: Ui :> es => Text -> Bool -> Eff es Response
menuButton' txt open =
  buttonStyled txt (if open then 1 else 0) menuBarTitleLayout buttonFlagMenuBar

menuBarTitleLayout :: Layout
menuBarTitleLayout = tight $ defaultLayout

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
