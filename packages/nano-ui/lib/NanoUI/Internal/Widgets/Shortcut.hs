-- | Keyboard listeners for a view (key pressed, released or held this frame)
-- and shortcuts. Both skip keys that the focused widget uses itself.
module NanoUI.Internal.Widgets.Shortcut
  ( keyPressed
  , keyPressedOnce
  , keyReleased
  , keyHeld
  , shortcut
  , shortcutOnce
  , hardQuitUsed
  , focusTakesChord
  ) where

import Control.Monad (when)
import Data.Foldable (toList)
import Data.IntSet qualified as IS
import Data.Maybe (isJust)
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (NanoUI, askInput, takeEscape, withContext, (<&&>))
import NanoUI.Internal.Shortcut
import NanoUI.Widgets.TextEditor (keyCommand, multiLineMode, singleLineMode)

-- | Whether the key went down this frame, including auto-repeats, with any
-- modifiers. 'False' behind an open modal, inside 'NanoUI.disabledWhen', and
-- when the focused widget uses the key itself ('focusTakesChord'), such as
-- Delete in a focused text field. Also 'False' for an Escape that closes an
-- open dropdown, a focused combo box's list or a text-edit menu, or that
-- something earlier in the view took ('NanoUI.takeEscape'). Like iced's
-- @keyboard::listen@, this only reports keys not claimed by the focused
-- widget; 'pressedIn' checks raw frame input. To bind a command to a key,
-- use 'shortcut'.
keyPressed :: Key -> NanoUI Bool
keyPressed = keyIn pressedIn

-- | 'keyPressed' without auto-repeats, so holding the key acts once.
keyPressedOnce :: Key -> NanoUI Bool
keyPressedOnce = keyIn pressedOnceIn

-- | Whether the key came up this frame. Filtered like 'keyPressed'.
keyReleased :: Key -> NanoUI Bool
keyReleased = keyIn releasedIn

-- | Whether the key is down. Filtered like 'keyPressed'.
keyHeld :: Key -> NanoUI Bool
keyHeld = keyIn heldIn

keyIn :: (Key -> Input -> Bool) -> Key -> NanoUI Bool
keyIn happened k = do
  inp <- askInput
  if happened k inp
    then withContext $ \ctx ->
      keyFree ctx (inputModifiers inp) k <&&> (if k == KeyEscape then not <$> escapeReserved ctx inp else pure True)
    else pure False

-- | Whether the view may handle this key: no modal is open and the focused
-- widget does not use it ('focusTakesChord').
keyFree :: Context -> Modifiers -> Key -> IO Bool
keyFree ctx mods k = do
  blocked <- pointerBlockedByModal ctx
  kind <- getsInteraction ctx isFocusKind
  if blocked || not (focusTakesChord kind mods k)
    then pure (not blocked)
    else getFocusId ctx >>= \focus -> (||) <$> isDisabled ctx focus <*> isInert ctx focus

-- | 'True' on a frame where the chord is pressed with exactly its modifiers.
-- Auto-repeats count as presses. Only the first shortcut declared for a chord
-- fires, and it consumes the frame's presses of that key. So a menu item and
-- a view binding the same chord do not both act, and a view that runs twice
-- in a frame only matches the press once. The chord does not fire:
--
-- * behind an open modal, or inside 'NanoUI.disabledWhen';
-- * when the focused widget uses the key itself ('focusTakesChord'). A
--   button keeps Enter and Space; a slider or list also keeps the arrows
--   (each alone or with Shift). A text field keeps typing, cursor and delete
--   keys and its own shortcuts such as Ctrl+A and Ctrl+Z; a multi-line field
--   leaves Ctrl or Alt with Enter, Up or Down free. During input method
--   composition, and on the frame it commits, the field keeps every key. A custom widget
--   keeps the keys its 'NanoUI.Widgets.Custom.widgetKeys' names;
-- * for Escape, when 'NanoUI.takeEscape' fails. Otherwise the shortcut
--   consumes the Escape.
--
-- A Tab chord does not move focus. A Ctrl+C that no shortcut takes, and that
-- the focused widget does not use, closes the window
-- ('NanoUI.Runner.runSessionLoop').
--
-- > whenM (shortcut (ctrl <> key 's')) save
-- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
-- > whenM (shortcut (key (KeyF 5))) refresh
--
-- A chord with no key never fires.
shortcut :: Shortcut -> NanoUI Bool
shortcut = chordShortcut False

-- | 'shortcut' without auto-repeats, so holding a toggle chord toggles once.
-- On a frame with only auto-repeats, a later 'shortcut' for the same chord
-- can still take them.
--
-- > whenM (shortcutOnce (key (KeyF 11))) toggleFullscreen
shortcutOnce :: Shortcut -> NanoUI Bool
shortcutOnce = chordShortcut True

chordShortcut :: Bool -> Shortcut -> NanoUI Bool
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
          -- Consume every press of the key, including auto-repeats, so a second
          -- binding of the chord does not fire.
          let presses = [i | (i, k) <- zip [0 ..] (toList (inputKeys inp)), k == pressedKey]
          took <- or <$> mapM (takeKeyPress ctx) presses
          when (took && pressedKey == KeyTab) (markTabConsumed ctx)
          pure took

-- | Whether the frame just run used its Ctrl+C ('isHardQuitInput'): a
-- shortcut took the press, the focused widget uses the chord itself
-- ('focusTakesChord') or copies with the platform's copy chord, as a text
-- field does, or a text-edit menu is open. The session loop quits only on a
-- Ctrl+C that nothing used.
hardQuitUsed :: Context -> Input -> IO Bool
hardQuitUsed ctx inp = do
  kind <- getsInteraction ctx isFocusKind
  menu <- getsInteraction ctx isTextInputMenu
  let presses = [i | (i, k) <- zip [0 ..] (toList (inputKeys inp)), k == KeyChar 'c']
      -- On macOS a field copies with Cmd+C and leaves Ctrl+C alone, but a
      -- Ctrl+C typed there out of habit must not close the window either.
      takes mods = focusTakesChord kind mods (KeyChar 'c')
  taken <- getsOverlay ctx (\os -> any (`IS.member` osKeysTaken os) presses)
  pure (taken || isJust menu || takes (inputModifiers inp) || takes primaryModifiers)

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
-- A text field takes Left, Right, Home and End with any modifiers, typing
-- keys ('isCommandKey'), and its editing keys and shortcuts
-- ('NanoUI.Widgets.TextEditor.keyCommand'). A single-line field also takes
-- Enter, and Up and Down with any modifiers, which step a numeric field or a
-- combo's list. A multi-line field leaves Ctrl or Alt with Enter, Up or Down
-- free, for example to submit. A read-only selectable text takes only Left,
-- Right, Home and End, and select-all and copy. During input
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
    FocusControl KeysNone -> False
    FocusControl KeysReadOnly -> readOnlyTakes
    FocusControl (KeysOnly chords) -> Shortcut (Just k) mods `elem` chords
    FocusTextLine -> fieldTakes False
    FocusTextSelectable -> readOnlyTakes
    FocusComposing -> True
  where
    readOnlyTakes = k `elem` [KeyLeft, KeyRight, KeyHome, KeyEnd]
      || (chordModifiers mods && k `elem` [KeyChar 'a', KeyChar 'c'])
    activates = shiftAtMost mods && (k == KeyEnter || k == KeySpace)
    moves = shiftAtMost mods && k `elem` [KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd, KeyPageUp, KeyPageDown]
    fieldTakes multi =
      k `elem` [KeyLeft, KeyRight, KeyHome, KeyEnd]
        || (not multi && k `elem` [KeyEnter, KeyUp, KeyDown])
        || not (isCommandKey mods k)
        || isJust (keyCommand (if multi then multiLineMode else singleLineMode) mods k)
