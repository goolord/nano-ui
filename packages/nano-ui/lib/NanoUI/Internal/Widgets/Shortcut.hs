-- | Keyboard listeners for a view (key pressed, released or held this frame)
-- and shortcuts. Both skip keys that the focused widget uses itself.
module NanoUI.Internal.Widgets.Shortcut
  ( keyPressed
  , keyPressedOnce
  , keyReleased
  , keyHeld
  , shortcut
  , shortcutOnce
  ) where

import Control.Monad (when)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (Ui, askInput, takeEscape, withContext)
import NanoUI.Internal.Shortcut
import NanoUI.Widgets.TextEditor (keyCommand, multiLineMode, singleLineMode)

-- | Whether the key went down this frame, including auto-repeats, with any
-- modifiers. 'False' behind an open modal, inside 'NanoUI.disabledWhen', and
-- when the focused widget uses the key itself ('focusTakesChord'), such as
-- Delete in a focused text field. Like iced's @keyboard::listen@, a view only
-- hears keys no widget took; 'pressedIn' sees all of them. To bind a command
-- to a key, use 'shortcut'.
keyPressed :: Ui :> es => Key -> Eff es Bool
keyPressed = keyIn pressedIn

-- | 'keyPressed' without auto-repeats, so holding the key acts once.
keyPressedOnce :: Ui :> es => Key -> Eff es Bool
keyPressedOnce = keyIn pressedOnceIn

-- | Whether the key came up this frame. Filtered like 'keyPressed'.
keyReleased :: Ui :> es => Key -> Eff es Bool
keyReleased = keyIn releasedIn

-- | Whether the key is down. Filtered like 'keyPressed'.
keyHeld :: Ui :> es => Key -> Eff es Bool
keyHeld = keyIn heldIn

keyIn :: Ui :> es => (Key -> Input -> Bool) -> Key -> Eff es Bool
keyIn happened k = do
  inp <- askInput
  if happened k inp
    then withContext (\ctx -> keyFree ctx (inputModifiers inp) k)
    else pure False

-- | Whether the view may handle this key: no modal is open and the focused
-- widget does not use it ('focusTakesChord').
keyFree :: Context -> Modifiers -> Key -> IO Bool
keyFree ctx mods k = do
  blocked <- pointerBlockedByModal ctx
  kind <- getsInteraction ctx isFocusKind
  pure (not blocked && not (focusTakesChord kind mods k))

-- | 'True' on a frame where the chord is pressed with exactly its modifiers.
-- Auto-repeats count as presses. Only the first shortcut declared for a chord
-- fires, and it consumes the frame's presses of that key. So a menu item and
-- a view binding the same chord do not both act, and a view that runs twice
-- in a frame sees the press once. The chord does not fire:
--
-- * behind an open modal, or inside 'NanoUI.disabledWhen';
-- * when the focused widget uses the key itself ('focusTakesChord'). A
--   button keeps Enter and Space; a slider or list also keeps the arrows
--   (each alone or with Shift). A text field keeps typing, cursor and delete
--   keys and its own shortcuts such as Ctrl+A and Ctrl+Z; a multi-line field
--   leaves Ctrl+Enter and Alt+Enter free. During input method composition,
--   and on the frame it commits, the field keeps every key. A custom widget
--   keeps the keys its 'NanoUI.Widgets.Custom.widgetKeys' names;
-- * for Escape, when 'NanoUI.takeEscape' fails. Otherwise the shortcut
--   consumes the Escape.
--
-- A Tab chord does not move focus.
--
-- > whenM (shortcut (ctrl <> key 's')) save
-- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
-- > whenM (shortcut (key (KeyF 5))) refresh
--
-- A chord with no key never fires.
shortcut :: Ui :> es => Shortcut -> Eff es Bool
shortcut = chordShortcut False

-- | 'shortcut' without auto-repeats, so holding a toggle chord toggles once.
-- On a frame with only auto-repeats, a later 'shortcut' for the same chord
-- can still take them.
--
-- > whenM (shortcutOnce (key (KeyF 11))) toggleFullscreen
shortcutOnce :: Ui :> es => Shortcut -> Eff es Bool
shortcutOnce = chordShortcut True

-- | 'shortcut', or 'shortcutOnce' when @once@ is set.
chordShortcut :: Ui :> es => Bool -> Shortcut -> Eff es Bool
chordShortcut _ (Shortcut Nothing _) = pure False
chordShortcut once (Shortcut (Just pressedKey) mods) = do
  inp <- askInput
  if inputModifiers inp /= mods || not ((if once then pressedOnceIn else pressedIn) pressedKey inp)
    then pure False
    else do
      free <- withContext (\ctx -> keyFree ctx mods pressedKey)
      -- Open overlays close on Escape, and they get it first.
      ours <- if free && pressedKey == KeyEscape then takeEscape else pure free
      if not ours
        then pure False
        else withContext $ \ctx -> do
          -- Take every press of the key, auto-repeats included, so a second
          -- binding of the chord sees none of them.
          let presses = [i | (i, k) <- zip [0 ..] (toList (inputKeys inp)), k == pressedKey]
          took <- or <$> mapM (takeKeyPress ctx) presses
          when (took && pressedKey == KeyTab) (markTabConsumed ctx)
          pure took

-- | Whether the focused widget uses this key and modifier combination
-- itself. If so, shortcuts and key listeners ignore it.
--
-- A control takes only its own keys, alone or with Shift ('shiftAtMost'),
-- so chords such as Alt+Left or Ctrl+Enter still reach shortcuts. Buttons
-- and checkboxes take Enter and Space ('KeysActivate'). Sliders, selects,
-- lists and pane grids also take the arrows, Home, End, Page Up and Page
-- Down ('KeysNavigate'). A custom widget takes what its
-- 'NanoUI.Widgets.Custom.widgetKeys' names.
--
-- A text field takes the arrows, Home and End with any modifiers, Enter if
-- single-line, typing keys ('isCommandKey'), and its editing keys and
-- shortcuts ('NanoUI.Widgets.TextEditor.keyCommand'). A multi-line field
-- leaves Ctrl+Enter and Alt+Enter free, for example to submit. During input
-- method composition, and on the frame it commits, the field takes every
-- key, so a chord that ends a composition does not also run a shortcut.
focusTakesChord :: FocusKind -> Modifiers -> Key -> Bool
focusTakesChord kind mods k =
  case kind of
    FocusNone -> False
    FocusControl KeysActivate -> activates
    FocusControl KeysNavigate -> activates || moves
    FocusControl KeysType -> fieldTakes True
    FocusControl KeysAll -> True
    FocusTextLine -> fieldTakes False
    FocusComposing -> True
  where
    activates = shiftAtMost mods && (k == KeyEnter || k == KeySpace)
    moves = shiftAtMost mods && k `elem` [KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd, KeyPageUp, KeyPageDown]
    fieldTakes multi =
      k `elem` [KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd]
        || (k == KeyEnter && not multi)
        || not (isCommandKey mods k)
        || isJust (keyCommand (if multi then multiLineMode else singleLineMode) mods k)
