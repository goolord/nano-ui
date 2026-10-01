-- | TodoList: a list whose rows are added, removed, filtered and reordered,
-- each keeping its own local state.
--
-- The list lives in a 'StateCell' allocated once in 'newApp', together with
-- the id the next item gets. Ids are never reused, so an id always names the
-- same item. Each row runs under @withKey (todoId todo)@: its widgets and
-- hooks are stored under that key rather than under its position.
--
-- To see why that matters, click Edit on the third row, type something, then
-- delete the first row. The edited row moves up, and its editing flag and
-- half-typed draft (two 'useFlag's and a 'useText' inside the row) move with
-- it. Without the key, the row sliding into position three would inherit
-- them instead. Filtering hides rows the same way: a hidden row's state waits
-- under its key until it is shown again.
--
-- Enter in the new-item field adds, like the Add button. The field then
-- clears, because the app passes it "" and inputs show what they are passed.
-- Clicking Add moves focus to the button, so 'requestFocus' hands it back to
-- the field. Reordering uses Up and Down buttons; 'useReorder' does drag and
-- drop, see its haddock.
--
-- Escape cancels an edit; 'takeEscape' claims that press, so the app's
-- Escape-to-quit skips it. Otherwise Escape quits. See Forms.hs for
-- validation and resetting.
--
-- Run it with @cabal run nano-ui-example-todo-list@.
module Main (main) where

import Control.Monad (forM_, when)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

data Todo = Todo {todoId :: !Int, todoText :: !Text, todoDone :: !Bool}
  deriving (Eq)

-- | The items in order, and the id the next one gets.
data Todos = Todos ![Todo] !Int
  deriving (Eq)

newtype App = App {appTodos :: StateCell Todos}

newApp :: IO App
newApp =
  App <$> newState (Todos [Todo 0 "Read the header" True, Todo 1 "Water the plants" False, Todo 2 "Edit me, then delete the first row" False] 3)

data Filter = All | Active | Done
  deriving (Eq, Show, Enum, Bounded)

matches :: Filter -> Todo -> Bool
matches All _ = True
matches Active t = not (todoDone t)
matches Done t = todoDone t

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Todo list", wsSize = Size 720 560}
      }
    (todoApp app)

todoApp :: App -> NanoUI ()
todoApp app = do
  (Todos items _, _) <- useState (appTodos app)
  (draft, setDraft) <- useText ""
  (filt, setFilt) <- useEnum All
  -- Every change goes through modifyState, which applies it to the latest
  -- list, so two edits in one frame both land.
  let editItems f = modifyState (appTodos app) (\(Todos xs n) -> Todos (f xs) n)
      shown = filter (matches filt) items
      left = length (filter (not . todoDone) items)
  columnWith (padAll 20 . gap 12 . fillW . fillH) $ do
    heading "Todos"
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      (inputR, draft') <- textInputConfigured' defaultTextInputConfig {ticPlaceholder = "What needs doing?"} draft
      clicked <- button "Add"
      let text = T.strip draft'
      if (clicked || respSubmitted inputR) && not (T.null text)
        then do
          modifyState (appTodos app) (\(Todos xs n) -> Todos (xs <> [Todo n text False]) (n + 1))
          setDraft ""
          requestFocus (respId inputR)
        else setDraft draft'
    rowWith (tight . gap 12 . alignMid . fillW) $ do
      -- tabBar keys must be Hashable; an enum's index is.
      picked <-
        tabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} (fromEnum filt) $
          [tab (fromEnum f) (T.pack (show f)) () | f <- [minBound .. maxBound :: Filter]]
      setFilt (toEnum picked)
      flex
      labelWith (tight . alignMid . fontMuted) (T.pack (show left) <> if left == 1 then " item left" else " items left")
      disabledWhen (left == length items) $
        whenM (button "Clear completed") (editItems (filter (not . todoDone)))
    scrollWith (grow . fillW) $
      columnWith (tight . gap 6 . fillW) $
        -- Each row gets its visible neighbours, so Up and Down skip rows
        -- the filter hides.
        forM_ (zip3 shown (Nothing : map Just shown) (map Just (drop 1 shown) <> [Nothing])) $ \(todo, above, below) ->
          withKey (todoId todo) (todoRow editItems todo above below)

todoRow :: (([Todo] -> [Todo]) -> NanoUI ()) -> Todo -> Maybe Todo -> Maybe Todo -> NanoUI ()
todoRow editItems todo above below = do
  -- Local hooks, kept under this row's key.
  (editing, setEditing) <- useFlag False
  (focusPending, setFocusPending) <- useFlag False
  (draft, setDraft) <- useText ""
  let tid = todoId todo
      change f = editItems (map (\t -> if todoId t == tid then f t else t))
      save = change (\t -> t {todoText = draft}) >> setEditing False
  when editing (whenM takeEscape (setEditing False))
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    done <- checkboxWith alignMid "" (todoDone todo)
    when (done /= todoDone todo) (change (\t -> t {todoDone = done}))
    -- The label and the field swap places, so they share a scope.
    scope $
      if editing
        then do
          (fieldR, draft') <- textInput' draft
          setDraft draft'
          -- Focus can only move to a widget declared this frame, and the
          -- field first appears on the frame after Edit was clicked.
          when focusPending (requestFocus (respId fieldR) >> setFocusPending False)
          when (respSubmitted fieldR) save
        else labelWith (fillW . alignMid . (if todoDone todo then fontStrike . fontMuted else id)) (todoText todo)
    whenM (button (if editing then "Save" else "Edit")) $
      if editing
        then save
        else setDraft (todoText todo) >> setEditing True >> setFocusPending True
    disabledWhen (null above) $ whenM (button "Up") (forM_ above (editItems . swapIds tid . todoId))
    disabledWhen (null below) $ whenM (button "Down") (forM_ below (editItems . swapIds tid . todoId))
    whenM (styled destructive (button "Delete")) (editItems (filter ((/= tid) . todoId)))

-- | Swap the places of two items, by id.
swapIds :: Int -> Int -> [Todo] -> [Todo]
swapIds a b xs = map pick xs
  where
    byId i t = fromMaybe t (find ((== i) . todoId) xs)
    pick t
      | todoId t == a = byId b t
      | todoId t == b = byId a t
      | otherwise = t
