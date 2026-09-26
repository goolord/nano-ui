-- | Key chords for 'NanoUI.shortcut' and 'NanoUI.menuItemShortcut': the
-- modifiers and a key, put together with '<>'.
--
-- > import NanoUI.Shortcut
-- >
-- > whenM (shortcut (ctrl <> key 's')) save
-- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
-- > whenM (shortcut (alt <> key KeyEnter)) toggleFullscreen
-- > whenM (shortcut (key (KeyF 5))) refresh
--
-- Its short names are meant to be imported where chords are written, or
-- qualified (@K.ctrl <> K.key 's'@). A chord is checked when it is built, so
-- it cannot be misspelled; 'parseShortcut' reads one written as text, such
-- as one from a settings file.
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
