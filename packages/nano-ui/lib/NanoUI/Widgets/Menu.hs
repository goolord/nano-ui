{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Menu
  ( contextMenu
  , contextMenuArea
  , useContextMenu
  , menuButton
  , MenuItem (..)
  , menuItemWith
  , menuItem
  , menuItemWithShortcut
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  )
where

import Control.Monad (void, when)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (getStore, intKey, setStore)
import NanoUI.Font (menuItemPadX, menuItemRowH, menuMinW, menuOuterPad, menuSepH)
import NanoUI.Input (inputMousePos, inputMouseReleased)
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), slotKey, slotMenuOpen, slotMenuPos)
import NanoUI.Style (Layout (..), defaultLayout, fillW, fixedH, fontMuted, gap, minW, padXY, tight)
import NanoUI.Types (PopupAnchor (..), PopupPlacement (..), V2 (..))
import NanoUI.WidgetText (buttonFlagMenu, buttonFlagMenuBar)
import NanoUI.Widgets.Combinators (buttonStyled)
import NanoUI.Widgets.Layout (columnWith, labelEx, rowWith, sep)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Widgets.Node (HasResponse, Response (..), containerResponse, respClicked, respHovered, respRightClicked)
import NanoUI.Widgets.Popup (PopupConfig (..), popup)

-- | Attach a context menu to any target response, opened on right-click.
contextMenu ::
  (Ui :> es, HasResponse r) =>
  r ->
  Eff es a ->
  Eff es (Maybe a)
contextMenu target child = do
  (isOpen0, pos0, openAt, close) <- useContextMenu
  inp <- askInput
  let rightClick = respRightClicked target
      mouse = inputMousePos inp
  when rightClick (openAt mouse)
  openMenuPopup (isOpen0 || rightClick) (if rightClick then mouse else pos0) child close

-- | Attach a context menu to an area/container.
contextMenuArea ::
  Ui :> es =>
  Layout ->
  Eff es a ->
  (V2 -> Eff es b) ->
  Eff es (a, Maybe b)
contextMenuArea layout areaContent menuContent = do
  (isOpen0, pos0, openAt, close) <- useContextMenu
  (areaRes, areaResp) <- containerResponse NodeContainer layout areaContent
  inp <- askInput
  let rightClick = respRightClicked areaResp
      mouse = inputMousePos inp
  when rightClick (openAt mouse)
  mBody <-
    openMenuPopup
      (isOpen0 || rightClick)
      (if rightClick then mouse else pos0)
      (menuContent (if rightClick then mouse else pos0))
      close
  pure (areaRes, mBody)

openMenuPopup ::
  Ui :> es =>
  Bool ->
  V2 ->
  Eff es a ->
  Eff es () ->
  Eff es (Maybe a)
openMenuPopup isOpen pos child close = do
  let cfg =
        PopupConfig
          { cfgAnchor = AnchorPoint pos
          , cfgPlacement = PlacementAtCursor
          , cfgDismissable = True
          , cfgOffset = 0
          }
  (popupResp, mBody) <- popup isOpen cfg (columnWith (tight . gap 0) child)
  inp <- askInput
  let picked = respHovered popupResp && inputMouseReleased inp
  when (respClicked popupResp || picked) close
  pure mBody

-- | Stateful hook for programmatic context menu control.
useContextMenu ::
  Ui :> es =>
  Eff es (Bool, V2, V2 -> Eff es (), Eff es ())
useContextMenu = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
      openK = slotKey slotMenuOpen key
      posK = slotKey slotMenuPos key
  store <- uiIO (getStore ctx)
  let isOpen = IM.findWithDefault 0 openK (storeInt store) /= 0
      (px, py) = IM.findWithDefault (0, 0) posK (storePoint store)
      openAt (V2 x y) =
        uiIO $
          getStore ctx >>= \st -> setStore ctx $
            st
              { storeInt = IM.insert openK 1 (storeInt st)
              , storePoint = IM.insert posK (x, y) (storePoint st)
              }
      close = uiIO $ getStore ctx >>= \st -> setStore ctx $ st {storeInt = IM.delete openK (storeInt st)}
  pure (isOpen, V2 px py, openAt, close)

-- | One context-menu row; the whole row is the button.
data MenuItem = MenuItem
  { menuItemLabel :: !Text
  , menuItemShortcut :: !(Maybe Text)
    -- ^ Keyboard shortcut hint shown after the label, e.g. @Ctrl+S@.
  , menuItemEnabled :: !Bool
    -- ^ Disabled rows are dimmed and cannot be clicked or focused.
  }
  deriving (Eq, Show)

-- | Render a menu row, returning its full 'Response'. A disabled row is a
-- muted label, not a disabled button: hover tracking does not know a button's
-- enabled flag and would still highlight it. Its response never reports
-- interaction.
menuItemWith :: Ui :> es => MenuItem -> Eff es Response
menuItemWith (MenuItem lbl shortcut enabled)
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
    text = maybe lbl (\s -> lbl <> "  " <> s) shortcut

-- | Standard context menu item. Returns 'True' if clicked this frame.
--
-- Example:
--
-- @
-- whenM (menuItem "Open...") openFile
-- @
menuItem :: Ui :> es => Text -> Eff es Bool
menuItem txt = respClicked <$> menuItemWith (MenuItem txt Nothing True)

-- | Menu item with a keyboard shortcut hint. Returns 'True' if clicked this frame.
--
-- Example:
--
-- @
-- whenM (menuItemWithShortcut "Save" "Ctrl+S") saveFile
-- @
menuItemWithShortcut :: Ui :> es => Text -> Text -> Eff es Bool
menuItemWithShortcut txt shortcut = respClicked <$> menuItemWith (MenuItem txt (Just shortcut) True)

-- | Disabled menu item (dimmed, non-interactive).
menuItemDisabled :: Ui :> es => Text -> Eff es ()
menuItemDisabled txt = void (menuItemWith (MenuItem txt Nothing False))

-- | Row layout shared by menu items, matching the text-field context menu
-- exactly: 28px rows and the same 148px minimum menu width on pixel hosts
-- (@textEditMenuItemH@ / @textEditMenuMinW@ in "NanoUI.Frame.TextEdit"); cell
-- hosts keep tight auto-sizing.
menuRowLayout :: Layout
menuRowLayout = minW menuMinW . fixedH menuItemRowH . tight . fillW $ defaultLayout

-- | Menu-bar title: a flat, label-sized button. @open@ tints the title while
-- its drop-down is showing, so the active menu reads at a glance.
menuButton :: Ui :> es => Text -> Bool -> Eff es Response
menuButton txt open =
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
    columnWith (tight . fillW) (void sep)

-- | Header / category title inside a context menu.
menuHeader :: Ui :> es => Text -> Eff es ()
menuHeader txt =
  void (labelEx (padXY 6 2 defaultLayout) txt)
