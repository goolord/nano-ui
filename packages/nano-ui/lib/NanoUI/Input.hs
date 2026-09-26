-- | The per-frame 'Input' record backends fill in and views read: pointer
-- state, keys and modifiers, typed characters, scroll, window size, and file
-- drops, with the cursor kinds a widget can ask for.
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
  ) where

import NanoUI.Internal.Input
