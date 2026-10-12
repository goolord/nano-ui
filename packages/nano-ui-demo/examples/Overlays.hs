{-# LANGUAGE OverloadedRecordDot #-}

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
import GHC.Generics (Generic)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

data Item = Item {itemId :: !Int, itemName :: !Text, itemFruit :: !Bool, itemPicked :: !Bool}
  deriving (Eq, Generic)

-- | The items, and the id the next one gets.
data Model = Model {items :: ![Item], nextId :: !Int}
  deriving (Eq, Generic)

newtype App = App {appModel :: Var Model}

newApp :: IO App
newApp =
  App <$> newVar (Model (zipWith3 (\i (n, f) p -> Item i n f p) [0 ..] produce (cycle [True, False])) 6)
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
  let itemsVar = app.appModel.items
  openMenu <- useVar ""
  filterOpen <- useVar False
  showFruit <- useVar True
  showVeg <- useVar True
  confirmDelete <- useVar False
  inspectorOpen <- useVar False
  asking <- useVar False
  status <- useVar "Right-click a row for its menu."
  Model all' fresh <- readVar app.appModel
  fruit <- readVar showFruit
  veg <- readVar showVeg
  inspecting <- readVar inspectorOpen
  let picked = length (filter itemPicked all')
      visible i = if itemFruit i then fruit else veg
      -- A menu row that closes its menu when picked.
      item menuRow action = whenM menuRow (writeVar openMenu "" >> action)
      askDelete = writeVar confirmDelete True
  columnWith (grow . gap 0) $ do
    menuBar openMenu
      [ ( "File"
        , do
            item (menuItem "New item") $
              modifyVar app.appModel (\(Model xs n) -> Model (xs <> [Item n ("Item " <> T.pack (show n)) True False]) (n + 1))
            menuSeparator
            item (menuItem "Quit...") (writeVar asking True)
        )
      , ( "Edit"
        , do
            item (menuItem "Pick all") (modifyVar itemsVar (map (\i -> i {itemPicked = True})))
            item (menuItem "Pick none") (modifyVar itemsVar (map (\i -> i {itemPicked = False})))
            menuSeparator
            -- The enabled and disabled rows swap, so they share a scope.
            scope $ if picked > 0 then item (menuItem "Delete picked...") askDelete else menuItemDisabled "Delete picked..."
        )
      , ("Help", item (menuItem (if inspecting then "Hide inspector" else "Show inspector")) (writeVar inspectorOpen (not inspecting)))
      ]
    separator
    columnWith (padAll 16 . gap 12 . fillW . fillH) $ do
      rowWith (tight . gap 8 . alignMid . fillW) $ do
        filterR <- button' "Filter"
        tooltip filterR "Choose which kinds of item to show"
        open <- readVar filterOpen
        whenM (popupToggled filterR) (writeVar filterOpen (not open))
        let below = (defaultPopupConfig (AnchorRect (respRect filterR))) {cfgPlacement = PlacementBelow}
        (filterPopR, _) <- popup open below $ do
          menuHeader "Show"
          checkbox "Fruit" showFruit
          checkbox "Vegetables" showVeg
        when (respClicked filterPopR) (writeVar filterOpen False)
        disabledWhen (picked == 0) $ whenM (styled destructive (button "Delete picked...")) askDelete
        toggleButton Accent "Inspector" inspectorOpen
        flex
        void $
          withTooltip (labelWith (tight . alignMid . fontMuted) (plural picked "item" <> " picked")) $ do
            label "Picked items are deleted together."
            muted "Edit > Pick all picks every row."
      scrollWith (grow . fillW) $
        columnWith (tight . gap 4 . fillW) $
          -- Each row edits its own item through a Var that follows the
          -- item's id, whatever the filter hides around it.
          withElemVars itemId itemsVar $ \xs at ->
            forM_ (filter visible xs) $ \i -> withKey (itemId i) (itemRow itemsVar status (at i))
      muted =<< readVar status

  -- Overlays float over the page wherever they are declared; keeping them
  -- together at the end reads best. Each takes its ids whether open or not.
  (winR, _) <- window inspecting "Inspector" $ do
    kv "Items" (T.pack (show (length all')))
    kv "Showing" (T.pack (show (length (filter visible all'))))
    kv "Picked" (T.pack (show picked))
    kv "Next id" (T.pack (show fresh))
  when (respClicked winR) (writeVar inspectorOpen False)

  deleting <- readVar confirmDelete
  (deleteR, _) <- modal deleting ("Delete " <> plural picked "item" <> "?") $ do
    muted "Picked rows hidden by the filter are deleted too."
    rowWith (tight . gap 8 . fillW) $ do
      flex
      whenM (button "Cancel") (writeVar confirmDelete False)
      whenM (styled destructive (button "Delete")) $ do
        modifyVar itemsVar (filter (not . itemPicked))
        writeVar status ("Deleted " <> plural picked "item" <> ".")
        writeVar confirmDelete False
  when (respClicked deleteR) (writeVar confirmDelete False)

  -- The close button only asks while wsExitOnCloseRequest is off.
  closing <- winCloseRequested <$> askWindow
  when closing (writeVar confirmDelete False >> writeVar asking True)
  quitting <- readVar asking
  (quitR, _) <- modal quitting "Quit?" $ do
    label "Your list is not saved anywhere."
    rowWith (tight . gap 8 . fillW) $ do
      flex
      whenM (button "Keep working") (writeVar asking False)
      whenM (styled destructive (button "Quit")) quitUi
  when (respClicked quitR) (writeVar asking False)

-- | One row: a checkbox across the row that edits the item's picked flag,
-- with a context menu on it. Vars inside 'contextMenu' live under the
-- row's key.
itemRow :: Var [Item] -> Var Text -> Var Item -> NanoUI ()
itemRow itemsVar status item = do
  i <- readVar item
  let iid = itemId i
  rowR <- checkboxWith' fillW (itemName i <> if itemFruit i then "  (fruit)" else "  (vegetable)") item.itemPicked
  void $ contextMenu rowR $ do
    menuHeader (itemName i)
    whenM (menuItem "Pick only this") (modifyVar itemsVar (map (\x -> x {itemPicked = itemId x == iid})))
    whenM (menuItem "Move to top") (modifyVar itemsVar (\xs -> filter ((== iid) . itemId) xs <> filter ((/= iid) . itemId) xs))
    menuSeparator
    whenM (menuItem "Delete") $ do
      modifyVar itemsVar (filter ((/= iid) . itemId))
      writeVar status ("Deleted " <> itemName i <> ".")

-- | A row of menu titles, each with a drop-down. @openMenu@ names the open
-- one, or is empty. While one is open, pointing at another title switches
-- to it, as desktop menu bars do.
menuBar :: Var Text -> [(Text, NanoUI ())] -> NanoUI ()
menuBar openMenu entries =
  rowWith (tight . fillW . padXY 4 2) $ do
    current <- readVar openMenu
    forM_ entries $ \(title, body) -> do
      let isOpen = current == title
      btn <- menuButton' title isOpen
      whenM (popupToggled btn) (writeVar openMenu (if isOpen then "" else title))
      when (not isOpen && not (T.null current) && respHovered btn) (writeVar openMenu title)
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
      (popR, _) <- popup isOpen cfg (columnWith (tight . gap 0) body)
      when (respClicked popR) (writeVar openMenu "")

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
