-- | Keyboard listeners for a view: whether a key went down, came up or is
-- held this frame, and shortcuts, which act on a chord once and leave the
-- widget holding the keyboard the keys it uses itself.
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

-- | Whether the key went down this frame, auto-repeats included, whatever
-- the modifiers. 'False' behind an open modal, inside 'NanoUI.disabledWhen',
-- and for a key the widget that has the keyboard acts on itself
-- ('focusTakesChord'), as the Delete a focused text field deletes with: like
-- iced's @keyboard::listen@, a view hears only the keys no widget took. The
-- input itself has them all ('pressedIn'). For a command bound to a key,
-- use 'shortcut'.
keyPressed :: Ui :> es => Key -> Eff es Bool
keyPressed = keyIn pressedIn

-- | 'keyPressed' for the press alone: 'False' on the frames that only
-- auto-repeat a held key, so holding the key down acts once.
keyPressedOnce :: Ui :> es => Key -> Eff es Bool
keyPressedOnce = keyIn pressedOnceIn

-- | Whether the key came up this frame, as 'keyPressed' for a release.
keyReleased :: Ui :> es => Key -> Eff es Bool
keyReleased = keyIn releasedIn

-- | Whether the key is down, as 'keyPressed' for a key held.
keyHeld :: Ui :> es => Key -> Eff es Bool
keyHeld = keyIn heldIn

keyIn :: Ui :> es => (Key -> Input -> Bool) -> Key -> Eff es Bool
keyIn happened k = do
  inp <- askInput
  if happened k inp
    then withContext (\ctx -> keyFree ctx (inputModifiers inp) k)
    else pure False

-- | Whether a key pressed with these modifiers is the view's: no modal is
-- in front of it, and the widget that has the keyboard does not act on it
-- ('focusTakesChord').
keyFree :: Context -> Modifiers -> Key -> IO Bool
keyFree ctx mods k = do
  blocked <- pointerBlockedByModal ctx
  kind <- getsInteraction ctx isFocusKind
  pure (not blocked && not (focusTakesChord kind mods k))

-- | 'True' on a frame that presses the chord, with exactly its modifiers
-- held; a held key's auto-repeats press it again. Only the first shortcut
-- declared for a chord fires, taking the frame's presses of it, so a menu
-- row and a view binding the same chord do not both act, and a view that
-- runs twice in the frame sees the press once. The chord does not fire:
--
-- * behind an open modal, or inside 'NanoUI.disabledWhen';
-- * when the widget that has the keyboard acts on it itself
--   ('focusTakesChord'): a focused button keeps Enter and Space, a slider
--   or a list the arrows too, each alone or with Shift, and a text field
--   the keys that type, the keys it moves and deletes with, and its
--   shortcuts such as Ctrl+A and Ctrl+Z, though a multi-line one leaves
--   Ctrl+Enter and Alt+Enter, which it does not act on. While an input
--   method composes in the field, and in the frame it commits text into
--   it, the field takes every key. A custom widget keeps what its
--   'NanoUI.Widgets.Custom.widgetKeys' says;
-- * for Escape, when 'NanoUI.takeEscape' would not take it, which it takes
--   as well.
--
-- A Tab chord keeps focus where it is.
--
-- > whenM (shortcut (ctrl <> key 's')) save
-- > whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) (setPaletteOpen True)
-- > whenM (shortcut (key (KeyF 5))) refresh
--
-- A chord with no key never fires.
shortcut :: Ui :> es => Shortcut -> Eff es Bool
shortcut = chordShortcut False

-- | 'shortcut' for the chord's press alone: 'False' on the frames that only
-- auto-repeat its key, so holding a chord that toggles something toggles it
-- once. A frame of auto-repeats alone leaves them to a 'shortcut' for the
-- chord declared later.
--
-- > whenM (shortcutOnce (key (KeyF 11))) toggleFullscreen
shortcutOnce :: Ui :> es => Shortcut -> Eff es Bool
shortcutOnce = chordShortcut True

-- | 'shortcut', or with @once@ 'shortcutOnce'.
chordShortcut :: Ui :> es => Bool -> Shortcut -> Eff es Bool
chordShortcut _ (Shortcut Nothing _) = pure False
chordShortcut once (Shortcut (Just pressedKey) mods) = do
  inp <- askInput
  if inputModifiers inp /= mods || not ((if once then pressedOnceIn else pressedIn) pressedKey inp)
    then pure False
    else do
      free <- withContext (\ctx -> keyFree ctx mods pressedKey)
      -- Escape is also the key that closes whatever is open, which takes it first.
      ours <- if free && pressedKey == KeyEscape then takeEscape else pure free
      if not ours
        then pure False
        else withContext $ \ctx -> do
          -- Take every press of the key, auto-repeats included, so a second
          -- binding of the chord does not act on one of them.
          let presses = [i | (i, k) <- zip [0 ..] (toList (inputKeys inp)), k == pressedKey]
          took <- or <$> mapM (takeKeyPress ctx) presses
          when (took && pressedKey == KeyTab) (markTabConsumed ctx)
          pure took

-- | Whether the widget holding the keyboard acts on the key itself, pressed
-- with these modifiers, which keeps a shortcut and the key listeners quiet
-- for it. A control takes only the keys it acts on, alone or with Shift
-- ('shiftAtMost'), so a chord such as Alt+Left or Ctrl+Enter still reaches
-- a shortcut: a button or a checkbox Enter and Space ('KeysActivate'), and
-- a slider, a select, a list or a pane grid the arrows, Home, End, Page Up
-- and Page Down as well ('KeysNavigate'). A custom widget takes what its
-- 'NanoUI.Widgets.Custom.widgetKeys' says. A text field takes the arrows,
-- Home and End whatever the modifiers, and Enter in a single-line one, the
-- keys that type ('isCommandKey'), and its editing keys and shortcuts
-- ('NanoUI.Widgets.TextEditor.keyCommand'), so a multi-line field leaves
-- Ctrl+Enter and Alt+Enter, which it does not act on, to send what was
-- typed. A field an input method composes in, or commits into this frame,
-- takes every key: those keys are the input method's, and a chord that ends
-- a composition commits its text rather than also running a shortcut.
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
