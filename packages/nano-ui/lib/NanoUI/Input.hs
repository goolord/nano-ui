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

    -- * Keys and buttons
  , Key (..)
  , Modifiers (..)
  , MouseButton (..)
  , applyMouseButton
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
  , CursorShape
  , grabHoverKind
  , grabDragKind

    -- * Input methods
  , Composition (..)
  , applyComposition
  ) where

import NanoUI.Internal.Input
