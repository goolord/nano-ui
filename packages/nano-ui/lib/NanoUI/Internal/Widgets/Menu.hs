-- | Menu rows, menu-bar buttons, and context-menu state for application views.
module NanoUI.Internal.Widgets.Menu
  ( contextMenu
  , contextMenuArea
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
import Data.IORef (readIORef)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import NanoUI.Internal.Context (Context (..), getStore, intKey, modifyStore)
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Font (menuItemPadX, menuItemRowH, menuMinW, menuOuterPad, menuSepH, widgetContentInset)
import NanoUI.Internal.Input (MouseButton (..), Pressable (..), inputMousePos, inputWindowSize)
import NanoUI.Internal.Monad (NanoUI, askContext, askDefaultLayout, askInput, freshWidget, liftIO, (<&&>))
import NanoUI.Internal.Store (Slot (..), fieldPoint, findSlot, flagSlot, insertSlot, setFlagSlot, slotKey)
import NanoUI.Internal.Style (Layout (..), Padding (..), defaultLayout, fillW, fixedH, fontMuted, gap, grow, maxH, minW, padXY, tight, windowMargin)
import NanoUI.Internal.Types (PopupAnchor (..), PopupPlacement (..), Size (..), V2 (..))
import NanoUI.Internal.WidgetText (buttonFlagMenu, buttonFlagMenuBar, hasFlag)
import NanoUI.Internal.Widgets.Combinators (buttonStyledEx)
import NanoUI.Internal.Widgets.Layout (columnWith, labelEx, rowWith, scrollWith, separator)
import NanoUI.Internal.Layout.Arena (NodeType (..), getNodeType, getStyleIdx)
import NanoUI.Internal.Widgets.Node (HasResponse, Response (..), containerResponse, inertResponse, respClicked, respHovered, respRightClicked)
import NanoUI.Internal.Var.Types (constVar)
import NanoUI.Internal.Widgets.Popup (PopupConfig (..), defaultPopupConfig, popupWith')
import NanoUI.Internal.Widgets.Shortcut (shortcut)
import NanoUI.Internal.Shortcut (Shortcut, shortcutLabel)

-- | A context menu for any widget response, opened by right-clicking it.
-- Returns the menu body's result while the menu is open. A click on a row
-- closes it, and a menu taller than the window scrolls.
contextMenu ::
  HasResponse r =>
  r ->
  NanoUI a ->
  NanoUI (Maybe a)
contextMenu target child = do
  menu <- freshWidget
  runContextMenu menu (respRightClicked target) (const child)

-- | A container whose right-click opens a context menu. The menu body
-- receives the position it was opened at.
contextMenuArea ::
  (Layout -> Layout) ->
  NanoUI a ->
  (V2 -> NanoUI b) ->
  NanoUI (a, Maybe b)
contextMenuArea f areaContent menuContent = do
  menu <- freshWidget
  base <- askDefaultLayout
  (areaRes, areaResp) <- containerResponse NodeContainer (f base) areaContent
  (,) areaRes <$> runContextMenu menu (respRightClicked areaResp) menuContent

-- | Open the menu at the pointer on a right click, show it while open, and
-- close it once a row is picked or it is dismissed. Whether it is open, and
-- where, is kept in the store under the menu's id.
runContextMenu ::
  (WidgetId, Context) ->
  Bool ->
  (V2 -> NanoUI a) ->
  NanoUI (Maybe a)
runContextMenu (wid, ctx) rightClick child = do
  inp <- askInput
  store <- liftIO (getStore ctx)
  let openK = slotKey SlotMenuOpen (intKey wid)
      posK = slotKey SlotMenuPos (intKey wid)
      mouse@(V2 mx my) = inputMousePos inp
      pos = if rightClick then mouse else uncurry V2 (findSlot fieldPoint (0, 0) posK store)
      cfg = (defaultPopupConfig (AnchorPoint pos)) {cfgPlacement = PlacementAtCursor, cfgOffset = 0}
      -- A menu taller than the window is held inside it, as a modal is, and
      -- its rows scroll.
      Size _ winH = inputWindowSize inp
      fitWindow = maxH (max 1 (winH - 2 * windowMargin))
  when rightClick $ liftIO (modifyStore ctx (setFlagSlot openK True . insertSlot fieldPoint posK (mx, my)))
  -- A left-button release inside the menu popup picks the row the press went
  -- down on. One on a disabled row, a header, a separator or the padding
  -- picks nothing.
  (popupResp, mBody) <-
    popupWith' (constVar (flagSlot openK store || rightClick)) cfg fitWindow $ do
      r <- scrollWith (tight . gap 0 . grow) (child pos)
      onRow <- (releasedIn MouseLeft <$> askInput) <&&> liftIO (pressedMenuRow ctx)
      pure (onRow, r)
  let picked = respHovered popupResp && maybe False fst mBody
  when (respClicked popupResp || picked) $ liftIO (modifyStore ctx (setFlagSlot openK False))
  pure (snd <$> mBody)

-- | Whether the left button went down on a menu row, added this frame.
pressedMenuRow :: Context -> IO Bool
pressedMenuRow ctx = do
  let na = ctxNodeArena ctx
  presses <- readIORef (ctxPressPos ctx)
  case M.lookup MouseLeft presses of
    Just (_, Just row) ->
      withWidgetNode ctx row False $ \idx ->
        ((== NodeButton) <$> getNodeType na idx) <&&> (hasFlag buttonFlagMenu <$> getStyleIdx na idx)
    _ -> pure False

-- | A menu row with an optional shortcut hint after the label. A disabled
-- row is a muted label, not a button (hover would still light a disabled
-- button), padded to match an enabled row. It reports no interaction.
menuItemWith :: Text -> Maybe Text -> Bool -> NanoUI Response
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
menuItem :: Text -> NanoUI Bool
menuItem txt = respClicked <$> menuItem' txt

{-# INLINE menuItem' #-}
-- | 'menuItem' returning its response; activation is in @respClicked@.
menuItem' :: Text -> NanoUI Response
menuItem' txt = menuItemWith txt Nothing True

-- | Menu row bound to a shortcut, showing the chord's 'shortcutLabel' after
-- the label. 'True' on the frame it is clicked, or when its chord is pressed
-- while the menu is open ('shortcut'); a click closes the menu, the chord
-- leaves it open. For the chord to work while the menu is closed, also bind
-- it with 'shortcut' outside the menu.
--
-- > whenM (menuItemShortcut "Save" (ctrl <> key 's')) saveFile
menuItemShortcut :: Text -> Shortcut -> NanoUI Bool
menuItemShortcut txt chord = do
  clicked <- respClicked <$> menuItemWith txt (Just (shortcutLabel chord)) True
  pressed <- shortcut chord
  pure (clicked || pressed)

-- | Dimmed menu row that cannot be clicked.
menuItemDisabled :: Text -> NanoUI ()
menuItemDisabled txt = void (menuItemWith txt Nothing False)

-- | Row layout shared by menu items, matching the text-field context menu.
menuRowLayout :: Layout
menuRowLayout = minW menuMinW . fixedH menuItemRowH . tight . fillW $ defaultLayout

-- | Menu-bar title: a flat, label-sized button. @open@ tints the title while
-- its drop-down is showing. 'True' on the frame it is clicked.
{-# INLINE menuButton #-}
menuButton :: Text -> Bool -> NanoUI Bool
menuButton txt open = respClicked <$> menuButton' txt open

-- | 'menuButton' returning its 'Response', whose rect anchors the drop-down.
menuButton' :: Text -> Bool -> NanoUI Response
menuButton' = menuButtonWith' id

-- | 'menuButton' with modified layout. A menu bar whose row is taller than a
-- label gives its titles 'NanoUI.fillH', so that each one covers the bar it
-- is in and its text sits in the middle of it rather than at the top.
{-# INLINE menuButtonWith #-}
menuButtonWith :: (Layout -> Layout) -> Text -> Bool -> NanoUI Bool
menuButtonWith f txt open = respClicked <$> menuButtonWith' f txt open

-- | 'menuButtonWith' returning its 'Response'.
menuButtonWith' :: (Layout -> Layout) -> Text -> Bool -> NanoUI Response
menuButtonWith' f txt open =
  buttonStyledEx True txt (if open then 1 else 0) (f (tight defaultLayout)) buttonFlagMenuBar

-- | Separator line inside a context menu, matching the text-field context
-- menu: a 1px rule inset 'menuItemPadX' from the panel edge, centred in a
-- 'menuSepH' band. The tight column keeps the rule horizontal.
menuSeparator :: NanoUI ()
menuSeparator = do
  rowWith (fixedH menuSepH . padXY (menuItemPadX - menuOuterPad) 4.5 . fillW) $
    columnWith (tight . fillW) separator

-- | Header / category title inside a context menu.
menuHeader :: Text -> NanoUI ()
menuHeader txt =
  void (labelEx (padXY 6 2 defaultLayout) txt)
