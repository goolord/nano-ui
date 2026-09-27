-- | Key chords for 'NanoUI.shortcut' and 'NanoUI.menuItemShortcut', built
-- from modifiers and a key with '<>'.
--
-- > import NanoUI.Shortcut
-- >
-- > whenM (shortcut (ctrl <> key 's')) save
-- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
-- > whenM (shortcut (alt <> key KeyEnter)) toggleFullscreen
-- > whenM (shortcut (key (KeyF 5))) refresh
--
-- The short names are meant for unqualified import where chords are
-- written, or qualified (@K.ctrl <> K.key 's'@). Chords are typed values, so
-- they cannot be misspelled; 'parseShortcut' reads one from text, such as a
-- settings file.
module NanoUI.Shortcut
  ( -- * Chords
    Shortcut (..)
  , ToKey (..)
  , key
  , ctrl
  , shift
  , alt
  , super
  , cmdOrCtrl

    -- * Text and labels
  , parseShortcut
  , shortcutLabel
  , keyLabel

    -- * Input
  , shortcutIn
  ) where

import NanoUI.Internal.Shortcut
