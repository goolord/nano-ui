-- | Components: your own reusable widgets, built from the built-in ones.
--
-- A component is an ordinary function returning @NanoUI a@. Wrap its parts
-- in one container (a row or column) and it behaves like a built-in: it
-- takes one widget id in its caller, so its insides never move the ids of
-- widgets around it, and a 'useVar' inside it is private to each place it
-- is called from.
--
-- Follow the library's conventions and callers can guess your API:
--
-- * Inputs take the 'Var' they edit and pass it on to the widgets inside
--   ('labeledSlider').
-- * Actions return what happened this frame, usually a 'Bool'
--   ('confirmButton').
-- * A primed variant returns a 'Response' where the plain one returns @()@,
--   for tooltips, hover and geometry: @button'@, @slider'@, and
--   'labeledSlider'' here. It is optional; add one when callers need it.
-- * A @...With@ variant takes a 'LayoutModifier', applied after your
--   defaults so the caller's choice wins ('captionedWith').
-- * State the caller should own, share or reset is a 'Var' made in IO and
--   passed in ('stepper').
--
-- See State.hs for where state lives. Escape quits.
--
-- Run it with @cabal run nano-ui-example-components@.
module Main (main) where

import Control.Monad (forM_, void, when)
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
  { stepperA :: !(Var Int)
  , stepperB :: !(Var Int)
  , files :: !(Var [T.Text])
  }

newApp :: IO AppState
newApp = AppState <$> newVar 0 <*> newVar 0 <*> newVar initialFiles

initialFiles :: [T.Text]
initialFiles = ["notes.txt", "draft.md", "budget.csv", "photo.png"]

view :: AppState -> NanoUI ()
view state =
  scrollWith (padAll 12 . grow) $
    columnWith (tight . gap 12 . fillW) $ do
      section "A component on a Var" $ do
        volume <- useVar 40
        resp <- labeledSlider' "Volume" 0 100 volume
        tooltip resp "The primed variant hands back the slider's Response."
        labeledSlider "Balance" (-1) 1 =<< useVar 0
      section "A component that returns an event" $ do
        cleared <- useVar (0 :: Int)
        whenM (confirmButton "Clear history") (modifyVar cleared (+ 1))
        n <- readVar cleared
        muted ("Cleared " <> T.pack (show n) <> " times")
      section "Owned state" $ do
        -- Two Vars, two steppers: independent. Drawing stepper A a second
        -- time shows the same Var, so both copies move together.
        stepper (stepperA state)
        stepper (stepperB state)
        stepper (stepperA state)
        total <- (+) <$> readVar (stepperA state) <*> readVar (stepperB state)
        kv "A + B" (T.pack (show total))
        whenM (button "Reset both") (writeVar (stepperA state) 0 >> writeVar (stepperB state) 0)
      section "A layout argument" $ do
        city <- useVar ""
        note <- useVar ""
        rowWith (tight . gap 12 . fillW) $ do
          captionedWith (fixedW 180) "City" (textInput city)
          captioned "Note" (textInput note)
      section "Many instances" $ do
        -- Each row's confirmButton keeps its own "Sure?" flag. Keyed by
        -- file name, the flag stays with its file when a row above is
        -- deleted. Keyed by position, it would slide onto the next file,
        -- and confirming would delete a file you never asked about.
        names <- readVar (files state)
        column . forM_ names $ \file ->
          withKey file . rowWith (tight . gap 8 . alignMid . fillW) $ do
            labelWith (alignMid . fillW) file
            whenM (confirmButton "Delete") (modifyVar (files state) (filter (/= file)))
        whenM (button "Restore files") (writeVar (files state) initialFiles)

-- | A titled panel: a component that wraps a body, as 'panel' does.
section :: T.Text -> NanoUI a -> NanoUI a
section title body = panelWith (padAll 12 . gap 8 . fillW) (heading title >> body)

-- (a) An input component: it takes the Var it edits and hands it to the
-- slider inside. The unprimed name drops the 'Response'.
labeledSlider :: T.Text -> Float -> Float -> Var Float -> NanoUI ()
labeledSlider caption lo hi v = void (labeledSlider' caption lo hi v)

-- | The value shows above the slider, so a drag runs the view a second
-- time to update it; shown below the slider, it would not.
labeledSlider' :: T.Text -> Float -> Float -> Var Float -> NanoUI Response
labeledSlider' caption lo hi v =
  columnWith (tight . gap 4 . fillW) $ do
    value <- readVar v
    kv caption (T.pack (printf "%.2f" value))
    slider' lo hi v

-- (b) A component with its own local state, returning an event: 'True' on
-- the frame the user confirms. Vars count apart from widgets, so the flag
-- keeps its place whichever branch's widgets run.
confirmButton :: T.Text -> NanoUI Bool
confirmButton txt =
  rowWith (tight . gap 6 . alignMid) $ do
    asking <- useVar False
    isAsking <- readVar asking
    if not isAsking
      then False <$ whenM (button txt) (writeVar asking True)
      else do
        sure <- styled destructive (button "Sure?")
        cancel <- button "Cancel"
        when (sure || cancel) (writeVar asking False)
        pure sure

-- (c) A component whose state the caller owns: it takes the Var. Unlike a
-- 'useVar' inside it, the value does not depend on where the stepper is
-- drawn, and code elsewhere can read or reset it.
stepper :: Var Int -> NanoUI ()
stepper n =
  rowWith (tight . gap 6 . alignMid) $ do
    whenM (button "-") (modifyVar n (subtract 1))
    labelWith (alignMid . fixedW 32) . T.pack . show =<< readVar n
    whenM (button "+") (modifyVar n (+ 1))

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
