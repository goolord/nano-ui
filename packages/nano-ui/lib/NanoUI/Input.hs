-- | The per-frame 'Input' record backends fill in and views read: pointer
-- state, keys and modifiers, typed characters, scroll, window size, file
-- drops and an input method's composition, with the cursor kinds a widget
-- can ask for.
module NanoUI.Input
  ( -- * Input
    Input (..)
  , emptyInput
  , clearEphemeral
  , inputInteracted
  , inputPointerHeld

    -- * Mouse buttons
  , MouseButton (..)
  , mouseButtonNumber
  , MouseButtons
  , noButtons
  , buttonsMember
  , buttonsNull
  , buttonsInsert
  , buttonsDelete
  , buttonsToList
  , buttonsFromList
  , buttonHeld
  , buttonPressed
  , buttonReleased
  , anyButtonPressed
  , anyButtonReleased
  , applyMouseButton
  , applyPointerLeave
  , inputMouseDown
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightDown
  , inputMouseRightPressed
  , inputMouseRightReleased

    -- * Keys
  , Key (..)
  , Modifiers (..)
  , appendInputKey
  , inputKeysFromList
  , inputKeysNull
  , inputKeysElem
  , foldInputKeys
  , applyKey
  , noModifiers
  , modPrimary
  , primaryModifiers

    -- * File drops
  , DropType (..)
  , DropEvent (..)
  , appendDropEvent

    -- * Cursors
  , UiCursorKind (..)
  , grabHoverKind
  , grabDragKind

    -- * Input methods
  , Composition (..)
  , applyComposition
  ) where

import NanoUI.Internal.Input
