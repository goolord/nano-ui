-- | Code for the book's Overlays & Windows chapter
-- (@book/src/widgets/overlays.md@).
module Book.Overlays where

import Control.Monad (when)
import NanoUI

confirmDelete, settingsDialog, notice, inspector, filterPopup, tooltips :: NanoUI ()

-- ANCHOR: modal
confirmDelete = do
  showConfirm <- useVar False

  whenM (button "Delete File...") (writeVar showConfirm True)

  modal showConfirm "Confirm Deletion" $ do
    label "Are you sure you want to permanently delete this file?"
    row $ do
      whenM (button "Cancel") (writeVar showConfirm False)
      whenM (styled destructive (button "Delete")) $ do
        deleteFile
        writeVar showConfirm False
-- ANCHOR_END: modal

-- ANCHOR: modal_with
settingsDialog = do
  open <- useVar False
  modalWith (fixedWH 600 400) open "Settings" $
    columnWith (fillW . fillH) settingsBody
-- ANCHOR_END: modal_with

-- ANCHOR: modal_derived
notice = do
  open <- useVar False
  isOpen <- readVar open
  (resp, _) <- modal' (constVar isOpen) "Important" $ do
    label "This dialog is shown while `open` is True."
    whenM (button "OK") (writeVar open False)
  when (respClicked resp) (writeVar open False)
-- ANCHOR_END: modal_derived

-- ANCHOR: window
inspector = do
  inspectorOpen <- useVar False
  window inspectorOpen "Inspector" $ do
    inspectorBody
-- ANCHOR_END: window

-- ANCHOR: popup
filterPopup = do
  filterOpen <- useVar False
  showFruit <- useVar True
  showVeg <- useVar True
  filterR <- button' "Filter"
  when (respClicked filterR) (modifyVar filterOpen not)

  popup filterOpen (defaultPopupConfig (AnchorRect (respRect filterR))) $ do
    checkbox "Fruit" showFruit
    checkbox "Vegetables" showVeg
-- ANCHOR_END: popup

-- ANCHOR: tooltips
tooltips = do
  save <- button' "Save"
  tooltip save "Write the file to disk"

  more <- button' "More"
  tooltipConfigured defaultTooltipConfig
    {tooltipPlacement = PlacementAtCursor} more "Follows the pointer"
-- ANCHOR_END: tooltips

-- Stand-ins for application code the snippets refer to.

deleteFile, settingsBody, inspectorBody :: NanoUI ()
deleteFile = pure ()
settingsBody = pure ()
inspectorBody = pure ()
