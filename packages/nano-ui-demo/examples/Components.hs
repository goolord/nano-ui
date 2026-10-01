-- | Components: your own reusable widgets, built from the built-in ones.
--
-- A component is an ordinary function returning @NanoUI a@. Wrap its parts
-- in one container (a row or column) and it behaves like a built-in: it
-- takes one widget id in its caller, so its insides never move the ids of
-- widgets around it, and hooks inside it are private to each place it is
-- called from.
--
-- Follow the library's conventions and callers can guess your API:
--
-- * Inputs are controlled: take the current value, return the new one
--   ('labeledSlider').
-- * Actions return what happened this frame, usually a 'Bool'
--   ('confirmButton').
-- * A primed variant also returns a 'Response', for tooltips, hover and
--   geometry: @button'@, @slider'@, and 'labeledSlider'' here. It is
--   optional; add one when callers need it.
-- * A @...With@ variant takes a 'LayoutModifier', applied after your
--   defaults so the caller's choice wins ('captionedWith').
-- * State the caller should own, share or reset is allocated in IO and
--   passed in ('newStepper' and 'stepper').
--
-- See State.hs for hooks, cells and keys. Escape quits.
--
-- Run it with @cabal run nano-ui-example-components@.
module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import Text.Printf (printf)

main :: IO ()
main = do
  state <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Components", wsSize = Size 720 760}
      }
    (view state)

data AppState = AppState
  { stepperA :: !Stepper
  , stepperB :: !Stepper
  , filesCell :: !(StateCell [T.Text])
  }

newApp :: IO AppState
newApp = AppState <$> newStepper <*> newStepper <*> newState initialFiles

initialFiles :: [T.Text]
initialFiles = ["notes.txt", "draft.md", "budget.csv", "photo.png"]

view :: AppState -> NanoUI ()
view state =
  scrollWith (padAll 12 . grow) $
    columnWith (tight . gap 12 . fillW) $ do
      section "A controlled component" $ do
        (volume, setVolume) <- useFloat 40
        (resp, volume') <- labeledSlider' "Volume" 0 100 volume
        setVolume volume'
        tooltip resp "The primed variant hands back the slider's Response."
        (balance, setBalance) <- useFloat 0
        setBalance =<< labeledSlider "Balance" (-1) 1 balance
      section "A component that returns an event" $ do
        (cleared, setCleared) <- useInt 0
        whenM (confirmButton "Clear history") (setCleared (cleared + 1))
        muted ("Cleared " <> T.pack (show cleared) <> " times")
      section "Owned state" $ do
        -- Two steppers, two cells: independent. Drawing stepper A a second
        -- time shares its cell, so both copies move together.
        a <- stepper (stepperA state)
        b <- stepper (stepperB state)
        _ <- stepper (stepperA state)
        kv "A + B" (T.pack (show (a + b)))
        whenM (button "Reset both") (resetStepper (stepperA state) >> resetStepper (stepperB state))
      section "A layout argument" $ do
        (city, setCity) <- useText ""
        (note, setNote) <- useText ""
        rowWith (tight . gap 12 . fillW) $ do
          setCity =<< captionedWith (fixedW 180) "City" (textInput city)
          setNote =<< captioned "Note" (textInput note)
      section "Many instances" $ do
        -- Each row's confirmButton keeps its own "Sure?" flag. Keyed by
        -- file name, the flag stays with its file when a row above is
        -- deleted. Keyed by position, it would slide onto the next file,
        -- and confirming would delete a file you never asked about.
        (files, setFiles) <- useState (filesCell state)
        column . forM_ files $ \file ->
          withKey file . rowWith (tight . gap 8 . alignMid . fillW) $ do
            labelWith (alignMid . fillW) file
            whenM (confirmButton "Delete") (modifyState (filesCell state) (filter (/= file)))
        whenM (button "Restore files") (setFiles initialFiles)

-- | A titled panel: a component that wraps a body, as 'panel' does.
section :: T.Text -> NanoUI a -> NanoUI a
section title body = panelWith (padAll 12 . gap 8 . fillW) (heading title >> body)

-- (a) A controlled component: the value goes in, the new value comes out,
-- and the caller stores it. The unprimed name drops the 'Response'.
labeledSlider :: T.Text -> Float -> Float -> Float -> NanoUI Float
labeledSlider caption lo hi value = snd <$> labeledSlider' caption lo hi value

labeledSlider' :: T.Text -> Float -> Float -> Float -> NanoUI (Response, Float)
labeledSlider' caption lo hi value =
  columnWith (tight . gap 4 . fillW) $ do
    kv caption (T.pack (printf "%.2f" value))
    slider' lo hi value

-- (b) A component with its own local state, returning an event: 'True' on
-- the frame the user confirms. The hook comes first, so it keeps its place
-- whichever branch runs; the branches differ only at the end of the row,
-- where nothing follows them to be moved.
confirmButton :: T.Text -> NanoUI Bool
confirmButton txt =
  rowWith (tight . gap 6 . alignMid) $ do
    (asking, setAsking) <- useFlag False
    if not asking
      then False <$ whenM (button txt) (setAsking True)
      else do
        sure <- styled destructive (button "Sure?")
        cancel <- button "Cancel"
        when (sure || cancel) (setAsking False)
        pure sure

-- (c) A component whose state the caller owns. 'newStepper' allocates it
-- once in IO; the newtype keeps the cell private, so callers go through
-- 'stepper' and 'resetStepper'. Unlike a hook, the value does not depend
-- on where the stepper is drawn, and code elsewhere can reset it.
newtype Stepper = Stepper (StateCell Int)

newStepper :: IO Stepper
newStepper = Stepper <$> newState 0

stepper :: Stepper -> NanoUI Int
stepper (Stepper cell) =
  rowWith (tight . gap 6 . alignMid) $ do
    (n, _) <- useState cell
    whenM (button "-") (modifyState cell (subtract 1))
    labelWith (alignMid . fixedW 32) (T.pack (show n))
    whenM (button "+") (modifyState cell (+ 1))
    pure n

resetStepper :: Stepper -> NanoUI ()
resetStepper (Stepper cell) = modifyState cell (const 0)

-- (d) A caption above any widget, taking a layout modifier like the
-- built-ins' @...With@ variants. The caller's modifier goes leftmost, so it
-- is applied last and overrides the defaults: @captionedWith (fixedW 180)@
-- replaces the 'fillW'.
captionedWith :: LayoutModifier -> T.Text -> NanoUI a -> NanoUI a
captionedWith f caption body =
  columnWith (f . tight . gap 4 . fillW) $ do
    labelWith fontMuted caption
    body

captioned :: T.Text -> NanoUI a -> NanoUI a
captioned = captionedWith id
