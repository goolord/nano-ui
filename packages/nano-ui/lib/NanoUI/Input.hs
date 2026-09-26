-- | The per-frame 'Input' record backends fill in and views read: pointer
-- state, keys and modifiers, typed characters, scroll, window size, file
-- drops and an input method's composition, with the cursor kinds a widget
-- can ask for.
module NanoUI.Input
  ( -- * Input
    Input (..)
  , Pressable (..)
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
  , releaseAllKeys
  , noModifiers
  , modPrimary
  , primaryModifiers
  , modJump
  , modMacCommand

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
