{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.Menu
  ( contextMenu
  , withContextMenu
  , contextMenuArea
  , useContextMenu
  , menuButton
  , menuItem
  , menuItemWithShortcut
  , menuItemWithIcon
  , menuItemDisabled
  , menuSeparator
  , menuHeader
  )
where

import Control.Monad (void, when)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), getStore, intKey, markDirty, setStore)
import NanoUI.Font (menuItemPadX, menuItemRowH, menuMinW, menuOuterPad, menuSepH)
import NanoUI.Input (inputMousePos, inputMouseReleased)
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), slotKey, slotMenuOpen, slotMenuPos)
import NanoUI.Style (Layout (..), defaultLayout, fillW, fixedH, fontMuted, gap, minW, padXY, tight)
import NanoUI.Types (PopupAnchor (..), PopupPlacement (..), V2 (..), isCellHost)
import NanoUI.WidgetText (buttonFlagMenu, buttonFlagMenuBar)
import NanoUI.Widgets.Combinators (buttonStyled)
import NanoUI.Widgets.Layout (columnWith, labelEx, rowWith, sep)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Widgets.Node (Responding (..), Response (..), containerResponse)
import NanoUI.Widgets.Popup (PopupConfig (..), popup)

-- | Attach a context menu to any target response, opened on right-click.
contextMenu ::
  (Ui :> es, Responding r) =>
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

-- | Alias for 'contextMenu'.
withContextMenu ::
  (Ui :> es, Responding r) =>
  r ->
  Eff es a ->
  Eff es (Maybe a)
withContextMenu = contextMenu

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
      openAt (V2 x y) = uiIO $ do
        st <- getStore ctx
        setStore
          ctx
          ( st
              { storeInt = IM.insert openK 1 (storeInt st)
              , storePoint = IM.insert posK (x, y) (storePoint st)
              }
          )
        markDirty ctx
      close = uiIO $ do
        st <- getStore ctx
        setStore ctx (st {storeInt = IM.delete openK (storeInt st)})
        markDirty ctx
  pure (isOpen, V2 px py, openAt, close)

-- | Standard context menu item.
menuItem :: Ui :> es => Text -> Eff es Response
menuItem txt = menuRowLayout >>= \lay -> buttonStyled txt 0 lay buttonFlagMenu

-- | Menu item with keyboard shortcut hint. Whole row is the button.
menuItemWithShortcut :: Ui :> es => Text -> Text -> Eff es Response
menuItemWithShortcut txt shortcut =
  menuRowLayout >>= \lay -> buttonStyled (txt <> "  " <> shortcut) 0 lay buttonFlagMenu

-- | Menu item with leading icon name. Whole row is the button.
menuItemWithIcon :: Ui :> es => Text -> Text -> Eff es Response
menuItemWithIcon iconName txt =
  menuRowLayout >>= \lay -> buttonStyled (iconName <> " " <> txt) 0 lay buttonFlagMenu

-- | Row layout shared by menu items, matching the text-field context menu
-- exactly: 28px rows and the same 148px minimum menu width on pixel hosts
-- (@textEditMenuItemH@ / @textEditMenuMinW@ in "NanoUI.Frame.TextEdit"); cell
-- hosts keep tight auto-sizing.
menuRowLayout :: Ui :> es => Eff es Layout
menuRowLayout = do
  ctx <- askContext
  let host = ctxHostProfile ctx
  pure $
    if isCellHost host
      then tight . fillW $ defaultLayout
      else minW menuMinW . fixedH (menuItemRowH host) . tight . fillW $ defaultLayout

-- | Menu-bar title: a flat, label-sized button. @open@ tints the title while
-- its drop-down is showing, so the active menu reads at a glance.
menuButton :: Ui :> es => Text -> Bool -> Eff es Response
menuButton txt open =
  buttonStyled txt (if open then 1 else 0) menuBarTitleLayout buttonFlagMenuBar

menuBarTitleLayout :: Layout
menuBarTitleLayout = tight $ defaultLayout

-- | Disabled menu item (dimmed, non-interactive).
menuItemDisabled :: Ui :> es => Text -> Eff es ()
menuItemDisabled txt =
  void (labelEx (tight . fillW . fontMuted $ defaultLayout) txt)

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
  ctx <- askContext
  let host = ctxHostProfile ctx
  if isCellHost host
    then void sep
    else
      rowWith (fixedH (menuSepH host) . padXY (menuItemPadX - menuOuterPad) 4.5 . fillW) $
        columnWith (tight . fillW) (void sep)

-- | Header / category title inside a context menu.
menuHeader :: Ui :> es => Text -> Eff es ()
menuHeader txt =
  void (labelEx (padXY 6 2 defaultLayout) txt)
