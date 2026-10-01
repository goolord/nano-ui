-- | Overlays: menu bars, context menus, tooltips, popovers, modal dialogs,
-- floating windows, and asking before the window closes.
--
-- Every overlay is controlled like an input: you pass whether it is open,
-- and it hands back a 'Response' whose 'respClicked' means "please close"
-- (Escape, a click outside, or its close button). Store False when you see
-- it; the overlay never closes itself. Its body is an ordinary view that runs
-- only while it is open, inside its own id scope, so opening it shifts no
-- other widget's state.
--
-- The menu bar is 'menuButton'' plus a 'popup' anchored under it, in a small
-- helper below. The Filter button opens the same kind of popup holding
-- checkboxes: clicks inside it are its own, a click outside closes it. Rows
-- open a 'contextMenu' on right-click. 'tooltip' labels a widget after the
-- pointer rests on it; 'withTooltip' does the same for any group of widgets,
-- with any widgets as the tip.
--
-- A 'modal' blocks the page behind it until answered. Keep one open at a
-- time: here a close request first puts away the delete dialog. A 'window'
-- floats over the page but leaves it usable, and can be dragged and resized.
--
-- With 'wsExitOnCloseRequest' off, closing the native window only sets
-- 'winCloseRequested'; the app asks in a modal and calls 'quitUi' if the user
-- agrees. Escape closes the open menu or dialog here, so it never quits. See
-- Keyboard.hs for menu shortcuts.
--
-- Run it with @cabal run nano-ui-example-overlays@.
module Main (main) where

import Control.Monad (forM_, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

data Item = Item {itemId :: !Int, itemName :: !Text, itemFruit :: !Bool, itemPicked :: !Bool}
  deriving (Eq)

-- | The items, and the id the next one gets.
data Model = Model ![Item] !Int
  deriving (Eq)

newtype App = App {appModel :: StateCell Model}

newApp :: IO App
newApp =
  App <$> newState (Model (zipWith3 (\i (n, f) p -> Item i n f p) [0 ..] produce (cycle [True, False])) 6)
  where
    produce = [("Apple", True), ("Carrot", False), ("Cherry", True), ("Leek", False), ("Pear", True), ("Potato", False)]

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings {wsTitle = "Overlays", wsSize = Size 760 560, wsExitOnCloseRequest = False}
      }
    (overlaysApp app)

overlaysApp :: App -> NanoUI ()
overlaysApp app = do
  (Model items fresh, _) <- useState (appModel app)
  (openMenu, setOpenMenu) <- useText ""
  (filterOpen, setFilterOpen) <- useFlag False
  (showFruit, setShowFruit) <- useFlag True
  (showVeg, setShowVeg) <- useFlag True
  (confirmDelete, setConfirmDelete) <- useFlag False
  (inspectorOpen, setInspectorOpen) <- useFlag False
  (asking, setAsking) <- useFlag False
  (status, setStatus) <- useText "Right-click a row for its menu."
  let editItems f = modifyState (appModel app) (\(Model xs n) -> Model (f xs) n)
      picked = length (filter itemPicked items)
      shown = filter (\i -> if itemFruit i then showFruit else showVeg) items
      -- A menu row that closes its menu when picked.
      item menuRow action = whenM menuRow (setOpenMenu "" >> action)
      askDelete = setConfirmDelete True
  columnWith (grow . gap 0) $ do
    menuBar openMenu setOpenMenu
      [ ( "File"
        , do
            item (menuItem "New item") $
              modifyState (appModel app) (\(Model xs n) -> Model (xs <> [Item n ("Item " <> T.pack (show n)) True False]) (n + 1))
            menuSeparator
            item (menuItem "Quit...") (setAsking True)
        )
      , ( "Edit"
        , do
            item (menuItem "Pick all") (editItems (map (\i -> i {itemPicked = True})))
            item (menuItem "Pick none") (editItems (map (\i -> i {itemPicked = False})))
            menuSeparator
            -- The enabled and disabled rows swap, so they share a scope.
            scope $ if picked > 0 then item (menuItem "Delete picked...") askDelete else menuItemDisabled "Delete picked..."
        )
      , ("Help", item (menuItem (if inspectorOpen then "Hide inspector" else "Show inspector")) (setInspectorOpen (not inspectorOpen)))
      ]
    separator
    columnWith (padAll 16 . gap 12 . fillW . fillH) $ do
      rowWith (tight . gap 8 . alignMid . fillW) $ do
        filterR <- button' "Filter"
        tooltip filterR "Choose which kinds of item to show"
        whenM (popupToggled filterR) (setFilterOpen (not filterOpen))
        let below = (defaultPopupConfig (AnchorRect (respRect filterR))) {cfgPlacement = PlacementBelow}
        (filterPopR, _) <- popup filterOpen below $ do
          menuHeader "Show"
          setShowFruit =<< checkbox "Fruit" showFruit
          setShowVeg =<< checkbox "Vegetables" showVeg
        when (respClicked filterPopR) (setFilterOpen False)
        disabledWhen (picked == 0) $ whenM (styled destructive (button "Delete picked...")) askDelete
        setInspectorOpen =<< toggleButton Accent "Inspector" inspectorOpen
        flex
        void $
          withTooltip (labelWith (tight . alignMid . fontMuted) (plural picked "item" <> " picked")) $ do
            label "Picked items are deleted together."
            muted "Edit > Pick all picks every row."
      scrollWith (grow . fillW) $
        columnWith (tight . gap 4 . fillW) $
          forM_ shown $ \i -> withKey (itemId i) (itemRow editItems setStatus i)
      muted status

  -- Overlays float over the page wherever they are declared; keeping them
  -- together at the end reads best. Each takes its ids whether open or not.
  (winR, _) <- window inspectorOpen "Inspector" $ do
    kv "Items" (T.pack (show (length items)))
    kv "Showing" (T.pack (show (length shown)))
    kv "Picked" (T.pack (show picked))
    kv "Next id" (T.pack (show fresh))
  when (respClicked winR) (setInspectorOpen False)

  (deleteR, _) <- modal confirmDelete ("Delete " <> plural picked "item" <> "?") $ do
    muted "Picked rows hidden by the filter are deleted too."
    rowWith (tight . gap 8 . fillW) $ do
      flex
      whenM (button "Cancel") (setConfirmDelete False)
      whenM (styled destructive (button "Delete")) $ do
        editItems (filter (not . itemPicked))
        setStatus ("Deleted " <> plural picked "item" <> ".")
        setConfirmDelete False
  when (respClicked deleteR) (setConfirmDelete False)

  -- The close button only asks while wsExitOnCloseRequest is off.
  closing <- winCloseRequested <$> askWindow
  when closing (setConfirmDelete False >> setAsking True)
  (quitR, _) <- modal asking "Quit?" $ do
    label "Your list is not saved anywhere."
    rowWith (tight . gap 8 . fillW) $ do
      flex
      whenM (button "Keep working") (setAsking False)
      whenM (styled destructive (button "Quit")) quitUi
  when (respClicked quitR) (setAsking False)

-- | One row: a checkbox across the row, with a context menu on it. Hooks
-- inside 'contextMenu' live under the row's key.
itemRow :: (([Item] -> [Item]) -> NanoUI ()) -> (Text -> NanoUI ()) -> Item -> NanoUI ()
itemRow editItems setStatus i = do
  let iid = itemId i
  (rowR, isPicked) <- checkboxWith' fillW (itemName i <> if itemFruit i then "  (fruit)" else "  (vegetable)") (itemPicked i)
  when (isPicked /= itemPicked i) $
    editItems (map (\x -> if itemId x == iid then x {itemPicked = isPicked} else x))
  void $ contextMenu rowR $ do
    menuHeader (itemName i)
    whenM (menuItem "Pick only this") (editItems (map (\x -> x {itemPicked = itemId x == iid})))
    whenM (menuItem "Move to top") (editItems (\xs -> filter ((== iid) . itemId) xs <> filter ((/= iid) . itemId) xs))
    menuSeparator
    whenM (menuItem "Delete") $ do
      editItems (filter ((/= iid) . itemId))
      setStatus ("Deleted " <> itemName i <> ".")

-- | A row of menu titles, each with a drop-down. @openMenu@ names the open
-- one, or is empty. While one is open, pointing at another title switches
-- to it, as desktop menu bars do.
menuBar :: Text -> (Text -> NanoUI ()) -> [(Text, NanoUI ())] -> NanoUI ()
menuBar openMenu setOpen entries =
  rowWith (tight . fillW . padXY 4 2) $
    forM_ entries $ \(title, body) -> do
      let isOpen = openMenu == title
      btn <- menuButton' title isOpen
      whenM (popupToggled btn) (setOpen (if isOpen then "" else title))
      when (not isOpen && not (T.null openMenu) && respHovered btn) (setOpen title)
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
      (popR, _) <- popup isOpen cfg (columnWith (tight . gap 0) body)
      when (respClicked popR) (setOpen "")

-- | Whether a button that opens a popup should flip it this frame. It acts
-- on the press: the popup closes on any press outside it, this button's
-- included, so flipping on the click (the release) would reopen it at once.
-- Enter or Space on the focused button still counts.
popupToggled :: Response -> NanoUI Bool
popupToggled r = do
  inp <- askInput
  pure ((respPressed r && pressedIn MouseLeft inp) || (respClicked r && not (releasedIn MouseLeft inp)))

plural :: Int -> Text -> Text
plural n noun = T.pack (show n) <> " " <> noun <> (if n == 1 then "" else "s")
