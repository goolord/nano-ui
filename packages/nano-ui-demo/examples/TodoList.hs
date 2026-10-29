{-# LANGUAGE OverloadedRecordDot #-}

-- | TodoList: a list whose rows are added, removed, filtered and reordered,
-- each keeping its own local state.
--
-- The list lives in a 'Var' made once in 'newApp', together with the id the
-- next item gets. Ids are never reused, so an id always names the same
-- item. Each row gets a Var for its own item, from 'withElemVars': the
-- row's checkbox edits @todo.todoDone@ directly, and the Var follows the
-- item's id when rows move. Each row also runs under @withKey (todoId todo)@,
-- so its widgets and positional Vars are kept under that key rather than
-- under its position.
--
-- To see why that matters, click Edit on the third row, type something, then
-- delete the first row. The edited row moves up, and its editing flag and
-- half-typed draft (two 'useVar's inside the row) move with it. Without the
-- key, the row sliding into position three would inherit them instead.
-- Filtering hides rows the same way: a hidden row's state waits under its key
-- until it is shown again.
--
-- Enter in the new-item field adds, like the Add button, and the field then
-- clears. Clicking Add moves focus to the button, so 'requestFocus' hands it
-- back to the field. Reordering uses Up and Down buttons; 'useReorder' does
-- drag and drop, see its haddock.
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
import Data.Text qualified as T
import GHC.Generics (Generic)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)

-- ANCHOR: model
data Todo = Todo
  { todoId :: !Int
  , todoText :: !Text
  , todoDone :: !Bool
  }
  deriving (Eq, Generic)

-- | The items in order, and the id the next one gets.
data Todos = Todos
  { todoItems :: ![Todo]
  , todoNext :: !Int
  }
  deriving (Eq, Generic)

newtype App = App {appTodos :: Var Todos}
-- ANCHOR_END: model

-- ANCHOR: new_app
newApp :: IO App
newApp =
  App
    <$> newVar
      ( Todos
          [ Todo 0 "Read the header" True
          , Todo 1 "Water the plants" False
          , Todo 2 "Edit me, then delete the first row" False
          ]
          3
      )
-- ANCHOR_END: new_app

-- ANCHOR: main
main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings =
          defaultWindowSettings {wsTitle = "Todo list", wsSize = Size 720 560}
      }
    (todoApp app)
-- ANCHOR_END: main

-- ANCHOR: todo_app
todoApp :: App -> NanoUI ()
todoApp app = do
  let
    items = app.appTodos.todoItems
  draft <- useVar ""
  filt <- useEnumVar All
  columnWith (padAll 20 . gap 12 . fillW . fillH) $ do
    heading "Todos"
    newItemRow app.appTodos draft
    filterRow items filt
    todoList items filt
-- ANCHOR_END: todo_app

-- ANCHOR: new_item_row
newItemRow :: Var Todos -> Var Text -> NanoUI ()
newItemRow todosVar draft = rowWith (tight . gap 8 . alignMid . fillW) $ do
  inputR <-
    textInputConfigured'
      defaultTextInputConfig {ticPlaceholder = "What needs doing?"}
      draft
  clicked <- button "Add"
  text <- T.strip <$> readVar draft
  when ((clicked || respSubmitted inputR) && not (T.null text)) $ do
    modifyVar todosVar (\(Todos xs n) -> Todos (xs <> [Todo n text False]) (n + 1))
    writeVar draft ""
    requestFocus (respId inputR)
-- ANCHOR_END: new_item_row

-- ANCHOR: filter_and_count
data Filter = All | Active | Done
  deriving (Eq, Show, Enum, Bounded)

matches :: Filter -> Todo -> Bool
matches All _ = True
matches Active t = not (todoDone t)
matches Done t = todoDone t

filterRow :: Var [Todo] -> Var Filter -> NanoUI ()
filterRow items filt = rowWith (tight . gap 12 . alignMid . fillW) $ do
  -- A header for every filter, as a segmented control.
  boundedTabBarConfigured
    defaultTabsConfig {tabsStyle = TabSegmented}
    (T.pack . show)
    filt
  flex
  todos <- readVar items
  let
    left = length (filter (not . todoDone) todos)
  labelWith
    (tight . alignMid . fontMuted)
    (T.pack (show left) <> if left == 1 then " item left" else " items left")
  disabledWhen (left == length todos) $
    whenM (button "Clear completed") (modifyVar items (filter (not . todoDone)))
-- ANCHOR_END: filter_and_count

-- ANCHOR: todo_list
todoList :: Var [Todo] -> Var Filter -> NanoUI ()
todoList items filt = do
  shownFilter <- readVar filt
  scrollWith (grow . fillW) $
    columnWith (tight . gap 6 . fillW) $
      withElemVars todoId items $ \todos at -> do
        let
          shown = filter (matches shownFilter) todos
        -- Each row gets its visible neighbours, so Up and Down skip rows
        -- the filter hides.
        forM_
          (zip3 shown (Nothing : map Just shown) (map Just (drop 1 shown) <> [Nothing])) $ \(todo, above, below) ->
          withKey (todoId todo) (todoRow items (at todo) above below)
-- ANCHOR_END: todo_list

-- ANCHOR: todo_row
todoRow :: Var [Todo] -> Var Todo -> Maybe Todo -> Maybe Todo -> NanoUI ()
todoRow items todo above below = do
  -- Local state, kept under this row's key.
  editing <- useVar False
  focusPending <- useVar False
  draft <- useVar ""
  t <- readVar todo
  isEditing <- readVar editing
  let
    tid = todoId t
    save = do
      writeVar todo.todoText =<< readVar draft
      writeVar editing False
  -- ANCHOR: take_escape
  when isEditing (whenM takeEscape (writeVar editing False))
  -- ANCHOR_END: take_escape
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    checkboxWith alignMid "" todo.todoDone
    -- The label and the field swap places, so they share a scope.
    scope $
      if isEditing
        then do
          fieldR <- textInput' draft
          -- Focus can only move to a widget declared in the pass that asks,
          -- and the field first appears in the pass after Edit was clicked,
          -- so the request waits in a Var.
          whenM
            (readVar focusPending)
            (requestFocus (respId fieldR) >> writeVar focusPending False)
          when (respSubmitted fieldR) save
        else
          labelWith
            (fillW . alignMid . (if todoDone t then fontStrike . fontMuted else id))
            (todoText t)
    whenM (button (if isEditing then "Save" else "Edit")) $
      if isEditing
        then save
        else
          writeVar draft (todoText t)
            >> writeVar editing True
            >> writeVar focusPending True
    disabledWhen (null above) $
      whenM (button "Up") (forM_ above (modifyVar items . swapIds tid . todoId))
    disabledWhen (null below) $
      whenM (button "Down") (forM_ below (modifyVar items . swapIds tid . todoId))
    whenM
      (styled destructive (button "Delete"))
      (modifyVar items (filter ((/= tid) . todoId)))
-- ANCHOR_END: todo_row

-- ANCHOR: swap_ids
-- | Swap the places of two items, by id.
swapIds :: Int -> Int -> [Todo] -> [Todo]
swapIds a b xs = map pick xs
 where
  byId i t = fromMaybe t (find ((== i) . todoId) xs)
  pick t
    | todoId t == a = byId b t
    | todoId t == b = byId a t
    | otherwise = t
-- ANCHOR_END: swap_ids
