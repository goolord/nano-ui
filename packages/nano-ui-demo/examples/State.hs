-- | State: where a view's state lives, and how widgets keep theirs.
--
-- A view runs every frame, so anything that must outlive a frame is stored
-- somewhere. There are two places:
--
-- * Positional hooks ('useInt', 'useText', 'useFlag', 'useEnum', 'useFloat',
--   'useToggle') need no setup. Each takes the next widget id in its
--   container and stores its value under that id.
-- * A 'StateCell' holds any type with an 'Eq' instance. Allocate it once in
--   IO ('newApp' below) and pass it to the view; the value lives in the
--   cell, not at a position.
--
-- Widget identity is positional: ids count up in call order among siblings,
-- and every container (row, column, panel) starts a new count for its
-- children while taking one id from its parent. Run the same widgets and
-- hooks in the same order every frame. Where that cannot hold, use 'scope'
-- for a part that comes and goes and 'withKey' for list items, as the last
-- two sections show.
--
-- State a view stops showing is kept: hide the extra options below and show
-- them again, and their value is still there.
--
-- See Hello.hs for the frame model and Components.hs for state owned by
-- your own widgets. Escape quits.
--
-- Run it with @cabal run nano-ui-example-state@.
module Main (main) where

import Control.Monad (forM_, unless, when)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main = do
  state <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "State", wsSize = Size 720 760}
      }
    (view state)

-- | State with no hook of its own type, allocated once before the session.
-- Allocating a cell inside the view would make a new one every frame.
data AppState = AppState
  { notesCell :: !(StateCell [T.Text])
  , playersCell :: !(StateCell [T.Text])
  }

newApp :: IO AppState
newApp = AppState <$> newState ["Buy milk", "Call Sam"] <*> newState ["Ada", "Brook", "Cyd"]

data Pace = Walk | Jog | Sprint
  deriving (Bounded, Enum, Eq, Show)

view :: AppState -> NanoUI ()
view state =
  scrollWith (padAll 12 . grow) $
    columnWith (tight . gap 12 . fillW) $ do
      hooks
      cells state
      derived state
      conditional
      keyed state

-- | A titled panel. Being a container, it gives each section its own id
-- count, so one section's widgets never move another's.
section :: T.Text -> NanoUI a -> NanoUI a
section title body = panelWith (padAll 12 . gap 8 . fillW) (heading title >> body)

-- (a) Positional hooks: a value and a setter, kept under this call's id.
hooks :: NanoUI ()
hooks = section "Positional hooks" $ do
  (count, setCount) <- useInt 0
  (title, setTitle) <- useText "Morning run"
  (outdoors, setOutdoors) <- useFlag True
  (pace, setPace) <- useEnum Jog
  counterRow "Count" count setCount
  setTitle =<< textInput title
  setOutdoors =<< checkbox "Outdoors" outdoors
  setPace =<< enumRadio pace

-- (b) A cell for a list, a type no hook stores. 'useState' reads it and
-- takes no id, so it can be read anywhere, as often as you like.
cells :: AppState -> NanoUI ()
cells state = section "State cells" $ do
  (notes, setNotes) <- useState (notesCell state)
  (draft, setDraft) <- useText ""
  rowWith (tight . gap 8 . fillW) $ do
    draft' <- textInput draft
    setDraft draft'
    -- 'modifyState' applies a change to the latest value. Writing
    -- @setNotes (notes <> [draft'])@ starts from this frame's snapshot
    -- instead, so two such writes in one frame would lose the first.
    whenM (button "Add") . unless (T.null draft') $ do
      modifyState (notesCell state) (<> [draft'])
      setDraft ""
    whenM (button "Clear") (setNotes [])
  -- The list is in its own column, so its length does not move the ids of
  -- anything after it.
  column (forM_ notes label)

-- (c) Derived values are computed from state every frame, never stored. A
-- second copy kept in a hook would have to be updated everywhere the
-- notes change, and would drift the first time one place forgot.
derived :: AppState -> NanoUI ()
derived state = section "Derived values" $ do
  (notes, _) <- useState (notesCell state)
  let longest = foldr (\n best -> if T.length n > T.length best then n else best) "" notes
  kv "Notes" (T.pack (show (length notes)))
  kv "Characters" (T.pack (show (sum (map T.length notes))))
  kv "Longest" (if T.null longest then "-" else longest)

-- (d) Conditional UI. The extra options run on some frames and not others,
-- so they sit in 'scope', which takes exactly one id whether or not its
-- body runs. Without it, the hook and row inside would push the counter
-- below along by two ids: showing the options, the level would read the
-- counter's slot and show its count, and the counter would read an empty
-- slot and start again from 0; hiding them would bring the old count back.
-- Wrapping the part in any container (a row, a column) works the same way,
-- since a container also takes one id.
conditional :: NanoUI ()
conditional = section "Conditional UI" $ do
  (advanced, setAdvanced) <- useFlag False
  setAdvanced =<< checkbox "Show advanced options" advanced
  scope . when advanced $ do
    -- Hidden, this hook does not run, but its value is kept: hide the
    -- options and show them again and the level is where you left it.
    (level, setLevel) <- useInt 3
    counterRow "Level" level setLevel
  (clicks, setClicks) <- useInt 0
  counterRow "Clicks" clicks setClicks

counterRow :: T.Text -> Int -> (Int -> NanoUI ()) -> NanoUI ()
counterRow name n setN =
  rowWith (tight . gap 8 . alignMid) $ do
    labelWith (alignMid . fixedW 60) name
    whenM (button "-") (setN (n - 1))
    labelWith alignMid (T.pack (show n))
    whenM (button "+") (setN (n + 1))

-- (e) A list that reorders. Each row runs under 'withKey' with the player's
-- name, so its counter is stored under the key rather than the position
-- and moves with the row. Without 'withKey' the names would move and the
-- scores would stay where they were.
keyed :: AppState -> NanoUI ()
keyed state = section "Keyed lists" $ do
  (players, _) <- useState (playersCell state)
  rowWith (tight . gap 8) $ do
    whenM (button "Reverse") (modifyState (playersCell state) reverse)
    whenM (button "Rotate") (modifyState (playersCell state) (\ps -> drop 1 ps <> take 1 ps))
  column . forM_ players $ \name ->
    withKey name . rowWith (tight . gap 8 . alignMid) $ do
      (score, setScore) <- useInt 0
      labelWith (alignMid . fixedW 80) name
      whenM (button "+1") (setScore (score + 1))
      labelWith alignMid (T.pack (show score))
