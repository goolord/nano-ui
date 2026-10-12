-- | State: where a view's state lives, and how widgets keep theirs.
--
-- A view runs every frame, so anything that must outlive a frame is kept in
-- a 'Var'. An input takes the Var it edits, and you read a Var with
-- 'readVar' where you show it. Vars come from two places:
--
-- * 'useVar' keeps a Var at its position in the view and needs no setup.
--   It holds an 'Int', 'Double', 'Float', 'Bool' or 'Text'; 'useEnumVar'
--   holds an enumeration.
-- * 'newVar' makes a Var of any type in IO ('newApp' below). Pass it to the
--   view; the value lives in the Var, not at a position.
--
-- A record in a Var gives a Var for each field: @state.settings.volume@
-- reads and writes just that field. That needs @OverloadedRecordDot@ and a
-- 'Generic' instance on the record.
--
-- Positions count in call order. Widgets and Vars count separately, so a
-- widget that comes and goes never moves a Var; a 'useVar' that comes and
-- goes, or a list of rows each with its own, needs 'scope' or 'withKey', as
-- the last two sections show. State a view stops showing is kept: hide the
-- extra options below and show them again, and their value is still there.
--
-- What a change costs: a write to a Var the view already read this frame
-- runs the view a second time, so what was drawn from the old value is
-- redrawn. Read a Var below the widgets that edit it and nothing reruns.
--
-- See Hello.hs for the frame model and Components.hs for state owned by
-- your own widgets. Escape quits.
--
-- Run it with @cabal run nano-ui-example-state@.
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

main :: IO ()
main = do
  state <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "State", wsSize = Size 720 900}
      }
    (view state)

data Settings = Settings
  { volume :: !Float
  , silent :: !Bool
  , nickname :: !T.Text
  }
  deriving (Eq, Generic)

-- | State the application owns, made once before the session. Making a Var
-- inside the view would make a new one every frame.
data AppState = AppState
  { notes :: !(Var [T.Text])
  , players :: !(Var [T.Text])
  , settings :: !(Var Settings)
  }

newApp :: IO AppState
newApp =
  AppState
    <$> newVar ["Buy milk", "Call Sam"]
    <*> newVar ["Ada", "Brook", "Cyd"]
    <*> newVar (Settings 40 False "Ada")

data Pace = Walk | Jog | Sprint
  deriving (Bounded, Enum, Eq, Show)

view :: AppState -> NanoUI ()
view state =
  scrollWith (padAll 12 . grow) $
    columnWith (tight . gap 12 . fillW) $ do
      local
      owned state
      fields state
      derived state
      conditional
      keyed state

-- | A titled panel. Being a container, it gives each section its own id
-- count, so one section's widgets never move another's.
section :: T.Text -> NanoUI a -> NanoUI a
section title body = panelWith (padAll 12 . gap 8 . fillW) (heading title >> body)

-- (a) Positional Vars: no setup, kept at their place in the view.
local :: NanoUI ()
local = section "Positional Vars" $ do
  count <- useVar (0 :: Int)
  title <- useVar "Morning run"
  outdoors <- useVar True
  pace <- useEnumVar Jog
  counterRow "Count" count
  textInput title
  checkbox "Outdoors" outdoors
  enumRadio pace

-- (b) An owned Var holding a list, a type 'useVar' does not hold. Any
-- number of places can read and write it.
owned :: AppState -> NanoUI ()
owned state = section "Owned Vars" $ do
  draft <- useVar ""
  rowWith (tight . gap 8 . fillW) $ do
    inputR <- textInput' draft
    clicked <- button "Add"
    when (clicked || respSubmitted inputR) $ do
      text <- T.strip <$> readVar draft
      -- 'modifyVar' applies a change to the latest value, so two changes
      -- in one frame both land.
      unless (T.null text) $ do
        modifyVar state.notes (<> [text])
        writeVar draft ""
    whenM (button "Clear") (writeVar state.notes [])
  -- The list is in its own column, so its length does not move the ids of
  -- anything after it.
  column (mapM_ label =<< readVar state.notes)

-- (c) One record in one Var, a widget per field. Each widget edits its own
-- field; nothing copies values back and forth.
fields :: AppState -> NanoUI ()
fields state = section "Fields of a record" $ do
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    labelWith (alignMid . fixedW 80) "Volume"
    sliderWith (fixedW 200) 0 100 state.settings.volume
  checkbox "Silent" state.settings.silent
  textInput state.settings.nickname
  s <- readVar state.settings
  muted (s.nickname <> (if s.silent then " (silent)" else " at volume " <> T.pack (show (round s.volume :: Int))))
  whenM (button "Reset") (writeVar state.settings (Settings 40 False "Ada"))

-- (d) Derived values are computed from state every frame, never stored. A
-- second copy kept in a Var would have to be updated everywhere the notes
-- change, and would drift the first time one place forgot.
derived :: AppState -> NanoUI ()
derived state = section "Derived values" $ do
  ns <- readVar state.notes
  let longest = foldr (\n best -> if T.length n > T.length best then n else best) "" ns
  kv "Notes" (T.pack (show (length ns)))
  kv "Characters" (T.pack (show (sum (map T.length ns))))
  kv "Longest" (if T.null longest then "-" else longest)

-- (e) Conditional UI. The extra options run on some frames and not others,
-- so they sit in 'scope', which keeps their Vars apart from the rest.
-- Without it, the 'useVar' inside would push the counter below along by
-- one: showing the options, the level would read the counter's Var and
-- show its count, and the counter would start again from 0. Widgets that
-- come and go need no scope to keep Vars in place, since widgets and Vars
-- count separately.
conditional :: NanoUI ()
conditional = section "Conditional UI" $ do
  advanced <- useVar False
  checkbox "Show advanced options" advanced
  shown <- readVar advanced
  scope . when shown $
    -- Hidden, this Var's position is not reached, but its value is kept:
    -- hide the options and show them again and the level is where you
    -- left it.
    counterRow "Level" =<< useVar 3
  counterRow "Clicks" =<< useVar 0

counterRow :: T.Text -> Var Int -> NanoUI ()
counterRow name n =
  rowWith (tight . gap 8 . alignMid) $ do
    labelWith (alignMid . fixedW 60) name
    whenM (button "-") (modifyVar n (subtract 1))
    labelWith alignMid . T.pack . show =<< readVar n
    whenM (button "+") (modifyVar n (+ 1))

-- (f) A list that reorders. Each row runs under 'withKey' with the player's
-- name, so its counter is kept under the key rather than the position and
-- moves with the row. Without 'withKey' the names would move and the scores
-- would stay where they were.
keyed :: AppState -> NanoUI ()
keyed state = section "Keyed lists" $ do
  rowWith (tight . gap 8) $ do
    whenM (button "Reverse") (modifyVar state.players reverse)
    whenM (button "Rotate") (modifyVar state.players (\ps -> drop 1 ps <> take 1 ps))
  names <- readVar state.players
  column . forM_ names $ \name ->
    withKey name . rowWith (tight . gap 8 . alignMid) $ do
      score <- useVar (0 :: Int)
      labelWith (alignMid . fixedW 80) name
      whenM (button "+1") (modifyVar score (+ 1))
      labelWith alignMid . T.pack . show =<< readVar score
