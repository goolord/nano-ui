-- | Code for the book's Buttons, Menus & Inputs chapter
-- (@book/src/widgets/controls.md@).
module Book.Controls where

import Control.Monad (void, when)
import Data.Text (Text)
import NanoUI
import NanoUI.Shortcut

saveButton, refreshButton, iconSaveButton, fileMenu, rowMenu, byteField,
  queryField, packageSearch, choices, sizeSelect :: NanoUI ()

-- ANCHOR: button
saveButton = whenM (button "Save Document") (liftIO saveFile)
-- ANCHOR_END: button

-- ANCHOR: button_response
refreshButton = do
  resp <- button' "Refresh"
  tooltip resp "Poll the remote server for updates"
  when (respClicked resp) refresh
-- ANCHOR_END: button_response

-- ANCHOR: button_content
iconSaveButton =
  whenM (buttonContent $ rowWith (gap 6 . alignMid) $ do
            svgIcon 16 saveIcon
            label "Save") $
    saveDocument
-- ANCHOR_END: button_content

-- ANCHOR: menu_bar
fileMenu = do
  fileOpen <- useVar False
  isOpen <- readVar fileOpen
  btn <- menuButton' "File" isOpen
  when (respClicked btn) (writeVar fileOpen (not isOpen))

  let cfg = (defaultPopupConfig (AnchorRect (respRect btn)))
              {cfgPlacement = PlacementBelow, cfgOffset = 0}
  (popupResp, _) <- popup' (constVar isOpen) cfg $
    columnWith (tight . gap 0) $ do
      whenM (menuItemShortcut "New" (cmdOrCtrl <> key 'n')) newDocument
      whenM (menuItem "Open...") openDocument
      menuSeparator
      whenM (menuItem "Exit") (writeVar fileOpen False >> quitUi)
  when (respClicked popupResp) (writeVar fileOpen False)
-- ANCHOR_END: menu_bar

-- ANCHOR: context_menu
rowMenu = do
  (_, rowResp) <- mouseArea (fillW . gap 6) (label "Right-click for actions")
  void $ contextMenu rowResp $ do
    whenM (menuItem "Inspect") inspectRow
    whenM (menuItem "Copy") copyRow
-- ANCHOR_END: context_menu

-- ANCHOR: configured
byteField = do
  byte <- useVar (0 :: Double)
  numericInputConfigured defaultNumericInputConfig
    {nicMin = 0, nicMax = 255, nicHex = True} byte
-- ANCHOR_END: configured

-- ANCHOR: text_input
queryField = do
  query <- useVar ""
  resp <- textInput' query
  when (respSubmitted resp) (search =<< readVar query)
  when (respChanged resp) (preview =<< readVar query)
-- ANCHOR_END: text_input

-- ANCHOR: search_input
packageSearch = do
  query <- useVar ""
  resp <- searchInput' "Find a package" query
  when (respChanged resp) (refreshResults =<< readVar query)
-- ANCHOR_END: search_input

-- ANCHOR: choices
data Pace = Walk | Jog | Sprint
  deriving (Eq, Show, Enum, Bounded)

choices = do
  enabled <- useVar True
  checkbox "Enable notifications" enabled

  isBold <- useVar False
  toggleButton Accent "B" isBold

  pace <- useEnumVar Jog
  enumRadio pace
  enumSelect pace
-- ANCHOR_END: choices

-- ANCHOR: select
sizeSelect = do
  choice <- useVar 0
  select ["Small", "Medium", "Large"] choice
-- ANCHOR_END: select

-- ANCHOR: values
values :: Var Color -> NanoUI ()
values accent = do
  volume <- useVar (40 :: Float)
  slider 0 100 volume

  count <- useVar (0 :: Double)
  numericInput count

  colorPicker accent -- accent <- newVar (colorHex "#38bdf8") in setup
-- ANCHOR_END: values

-- Stand-ins for application code the snippets refer to.

saveFile :: IO ()
saveFile = pure ()

refresh, saveDocument, newDocument, openDocument, inspectRow, copyRow :: NanoUI ()
refresh = pure ()
saveDocument = pure ()
newDocument = pure ()
openDocument = pure ()
inspectRow = pure ()
copyRow = pure ()

search, preview, refreshResults :: Text -> NanoUI ()
search _ = pure ()
preview _ = pure ()
refreshResults _ = pure ()

saveIcon :: Svg
saveIcon = either error id (parseSvg "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'/>")
