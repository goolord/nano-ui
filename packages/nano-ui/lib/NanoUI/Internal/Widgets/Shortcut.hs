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
  , focusTakesChord
  ) where

import Control.Monad (when)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Primitive.SmallArray (SmallArray)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (Ui, askInput, takeEscape, withContext)
import NanoUI.Internal.Shortcut
import NanoUI.Widgets.TextEditor (keyCommand, multiLineMode, singleLineMode)

-- | Whether the key went down this frame, auto-repeats included, whatever
-- the modifiers and whatever has the keyboard. 'False' behind an open modal
-- and inside 'NanoUI.disabledWhen'. For a command bound to a key, use
-- 'shortcut'.
keyPressed :: Ui :> es => Key -> Eff es Bool
keyPressed = keyIn inputKeys

-- | 'keyPressed' for the press alone: 'False' on the frames that only
-- auto-repeat a held key, so holding the key down acts once.
keyPressedOnce :: Ui :> es => Key -> Eff es Bool
keyPressedOnce = keyIn inputKeysNew

-- | Whether the key came up this frame, as 'keyPressed' for a release.
keyReleased :: Ui :> es => Key -> Eff es Bool
keyReleased = keyIn inputKeysReleased

-- | Whether the key is down, as 'keyPressed' for a key held.
keyHeld :: Ui :> es => Key -> Eff es Bool
keyHeld = keyIn inputKeysHeld

keyIn :: Ui :> es => (Input -> SmallArray Key) -> Key -> Eff es Bool
keyIn field k = do
  inp <- askInput
  if inputKeysElem k (field inp)
    then withContext (fmap not . pointerBlockedByModal)
    else pure False

-- | 'True' on a frame that presses the chord, with exactly its modifiers
-- held; a held key's auto-repeats press it again. Only the first shortcut
-- declared for a chord fires, taking the frame's presses of it, so a menu
-- row and a view binding the same chord do not both act, and a view that
-- runs twice in the frame sees the press once. The chord does not fire:
--
-- * behind an open modal, or inside 'NanoUI.disabledWhen';
-- * when the widget that has the keyboard acts on it itself
--   ('focusTakesChord'): a focused control keeps Enter, Space, the arrows,
--   Home and End, and a text field also the keys that type, the keys it
--   moves and deletes with, and its shortcuts such as Ctrl+A and Ctrl+Z,
--   though a multi-line one leaves Ctrl+Enter and Alt+Enter, which it does
--   not act on. While an input method composes in the field, and in the
--   frame it commits text into it, the field takes every key. A custom
--   widget that holds the keyboard counts as a control;
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
  let presses = [i | (i, k) <- zip [0 ..] (toList (inputKeys inp)), k == pressedKey]
  if null presses || inputModifiers inp /= mods || (once && not (pressedOnceIn pressedKey inp))
    then pure False
    else do
      free <- withContext $ \ctx -> do
        blocked <- pointerBlockedByModal ctx
        kind <- getsInteraction ctx isFocusKind
        pure (not blocked && not (focusTakesChord kind mods pressedKey))
      -- Escape is also the key that closes whatever is open, which takes it first.
      ours <- if free && pressedKey == KeyEscape then takeEscape else pure free
      if not ours
        then pure False
        else withContext $ \ctx -> do
          -- Take every press of the key, auto-repeats included, so a second
          -- binding of the chord does not act on one of them.
          took <- or <$> mapM (takeKeyPress ctx) presses
          when (took && pressedKey == KeyTab) (markTabConsumed ctx)
          pure took

-- | Whether the widget holding the keyboard acts on the chord itself, which
-- keeps a shortcut for it quiet. A control takes Enter, the arrows, Home and
-- End whatever the modifiers, and Space with no Ctrl, Alt or Super held. A
-- text field takes those, the keys that type (a character key alone, with
-- Shift, or with AltGr, which is Ctrl+Alt, or on macOS with Option), and
-- its editing keys and shortcuts ('NanoUI.Widgets.TextEditor.keyCommand'),
-- except that a multi-line field takes Enter only when it breaks the line,
-- so Ctrl+Enter can send what was typed. A field an input method composes
-- in, or commits into this frame, takes every key: those keys are the input
-- method's, and a chord that ends a composition commits its text rather than
-- also running a shortcut.
focusTakesChord :: FocusKind -> Modifiers -> Key -> Bool
focusTakesChord kind mods k =
  case kind of
    FocusNone -> False
    FocusControl -> navigation
    FocusTextField multi ->
      (navigation && not (multi && k == KeyEnter))
        || typing
        || isJust (keyCommand (if multi then multiLineMode else singleLineMode) mods k)
    FocusComposing -> True
  where
    command = modCtrl mods || modAlt mods || modSuper mods
    navigation =
      k `elem` [KeyEnter, KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd]
        || (k == KeySpace && not command)
    typing = not (isCommandKey mods k)
