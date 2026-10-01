-- | Keyboard: shortcuts, arrow keys, and how both share the keyboard with
-- focused widgets.
--
-- 'shortcut' fires on a frame its chord is pressed; 'shortcutOnce' ignores
-- auto-repeat, which is what a toggle wants (holding Ctrl+B should not
-- flicker). Chords are built with "NanoUI.Shortcut": @ctrl <> key 's'@ is
-- Ctrl+S everywhere, while @cmdOrCtrl <> key 'b'@ is Cmd+B on macOS and
-- Ctrl+B elsewhere, so prefer 'cmdOrCtrl' for app commands.
--
-- A focused widget keeps the keys it uses. Click the search field: the
-- arrows now move its caret instead of the grid selection, and Ctrl+A
-- selects its text rather than reaching a shortcut. Ctrl+S still saves,
-- because text fields do not use it. 'keyPressed' and 'shortcut' do this
-- filtering for you, so the view never asks what has focus. Ctrl+F focuses
-- the search field ('requestFocus'), and Escape drops focus ('clearFocus')
-- once 'takeEscape' says the press is the view's to use.
--
-- A 'menuItemShortcut' row shows its chord, but binds it only while its
-- menu is open. Each action is therefore written once and triggered from
-- both its menu row and a 'shortcut' bound outside the menu. Only the first
-- binding of a chord in a frame fires, so the two never both run.
--
-- Escape closes the menu or clears focus here, so it does not quit: press
-- Ctrl+Q (or Cmd+Q), pick Quit, or close the window. See Forms.hs for Tab
-- order and Enter to submit.
--
-- Run it with @cabal run nano-ui-example-keyboard@.
module Main (main) where

import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import NanoUI.Shortcut

-- | The action log, newest first. A list needs a 'StateCell'.
newtype App = App {appLog :: StateCell [Text]}

newApp :: IO App
newApp = App <$> newState []

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions {sdlWindowSettings = defaultWindowSettings {wsTitle = "Keyboard", wsSize = Size 640 600}}
    (keyboardApp app)

saveChord, boldChord, findChord, quitChord :: Shortcut
saveChord = ctrl <> key 's'
boldChord = cmdOrCtrl <> key 'b'
findChord = cmdOrCtrl <> key 'f'
quitChord = cmdOrCtrl <> key 'q'

cols, rows :: Int
cols = 4
rows = 3

keyboardApp :: App -> NanoUI ()
keyboardApp app = do
  (saves, setSaves) <- useInt 0
  (isBold, setBold) <- useFlag False
  (sel, setSel) <- useInt 0
  (query, setQuery) <- useText ""
  (menuOpen, setMenuOpen) <- useFlag False
  (entries, _) <- useState (appLog app)
  -- Each action is defined once, whatever triggers it.
  let logged msg = modifyState (appLog app) (take 6 . (msg :))
      save = setSaves (saves + 1) >> logged ("Saved (" <> T.pack (show (saves + 1)) <> ")")
      toggleBold = setBold (not isBold) >> logged (if isBold then "Bold off" else "Bold on")
  columnWith (padAll 20 . gap 14 . fillW . fillH) $ do
    (mSave, mBold, mQuit) <- rowWith (tight . gap 12 . alignMid . fillW) $ do
      btn <- menuButton' "Actions" menuOpen
      whenM (popupToggled btn) (setMenuOpen (not menuOpen))
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
      (popR, picked) <- popup menuOpen cfg $ columnWith (tight . gap 0) $ do
        s <- menuItemShortcut "Save" saveChord
        b <- menuItemShortcut "Bold" boldChord
        menuSeparator
        q <- menuItemShortcut "Quit" quitChord
        pure (s, b, q)
      -- A click outside or Escape dismisses the menu; so does picking a row.
      let chosen@(s, b, q) = fromMaybe (False, False, False) picked
      when (respClicked popR || s || b || q) (setMenuOpen False)
      flex
      labelWith (tight . alignMid . fontMuted) ("Saved " <> T.pack (show saves) <> " times")
      pure chosen
    -- shortcutLabel spells a chord for this platform, as menu rows do.
    labelWith (if isBold then fontBold else id) (shortcutLabel boldChord <> " makes this line bold.")
    (searchR, query') <- searchInput' ("Search (" <> shortcutLabel findChord <> ")") query
    setQuery query'
    searchFocused <- isFocused (respId searchR)
    muted $
      if searchFocused
        then "The search field has focus: arrows and Ctrl+A are its own. Escape releases it."
        else "Nothing is typing: arrows move the selection below."
    gridWith cols (tight . gap 6) $
      forM_ [0 .. cols * rows - 1] $ \i -> do
        on <- toggleButtonWith (fixedWH 56 40) Accent (T.singleton (toEnum (fromEnum 'A' + i))) (i == sel)
        when (on /= (i == sel)) (setSel i)
    -- keyPressed hears a key only when no focused widget uses it, so the
    -- grid needs no focus check of its own.
    let dir k = fromEnum <$> keyPressed k
    dx <- (-) <$> dir KeyRight <*> dir KeyLeft
    dy <- (-) <$> dir KeyDown <*> dir KeyUp
    when (dx /= 0 || dy /= 0) $ do
      let (r, c) = sel `divMod` cols
      setSel (clampTo rows (r + dy) * cols + clampTo cols (c + dx))
    separator
    labelWith (tight . fontMuted) "Last actions"
    forM_ entries label
    -- Bindings that work while the menu is closed. Declared after the menu,
    -- so while it is open its rows take the chords first.
    whenM ((mSave ||) <$> shortcut saveChord) save
    whenM ((mBold ||) <$> shortcutOnce boldChord) toggleBold
    whenM ((mQuit ||) <$> shortcut quitChord) quitUi
    whenM (shortcut findChord) $ do
      requestFocus (respId searchR)
      logged "Focused search"
    -- After the menu's popup, which takes the Escape that closes it first.
    whenM takeEscape $ do
      clearFocus
      logged "Escape: focus cleared"

-- | Whether a button that opens a popup should flip it this frame. It acts
-- on the press: the popup closes on any press outside it, this button's
-- included, so flipping on the click (the release) would reopen it at once.
-- Enter or Space on the focused button still counts.
popupToggled :: Response -> NanoUI Bool
popupToggled r = do
  inp <- askInput
  pure ((respPressed r && pressedIn MouseLeft inp) || (respClicked r && not (releasedIn MouseLeft inp)))

clampTo :: Int -> Int -> Int
clampTo n = max 0 . min (n - 1)
