-- | Code for the book's Widget Identity chapter (@book/src/state/identity.md@).
module Book.Identity where

import Control.Monad (forM_, when)
import Data.Foldable (for_)
import Data.Text (Text)
import GHC.Generics (Generic)
import NanoUI

detailsBad, detailsGood :: Bool -> Var Text -> NanoUI ()

-- ANCHOR: scope
-- BAD: when showDetails is False, every later sibling shifts down one id.
detailsBad showDetails detailsVar =
  when showDetails $ do
    label "Details..."
    textInput detailsVar

-- GOOD: one stable slot, whether or not the body runs.
detailsGood showDetails detailsVar =
  scope $
    when showDetails $ do
      label "Details..."
      textInput detailsVar
-- ANCHOR_END: scope

-- ANCHOR: scope_branch
todoLabel :: Bool -> Var Text -> Todo -> NanoUI ()
todoLabel isEditing draft todo =
  scope $
    if isEditing
      then textInput draft
      else label (todoText todo)
-- ANCHOR_END: scope_branch

-- ANCHOR: with_key
todoList :: [Todo] -> NanoUI ()
todoList items =
  column $ for_ items $ \todo ->
    withKey (todoId todo) $
      row $ do
        label (todoText todo)
        whenM (button "Delete") (deleteItem (todoId todo))
-- ANCHOR_END: with_key

-- ANCHOR: with_key_use_var
expandable :: [Todo] -> NanoUI ()
expandable items =
  for_ items $ \todo ->
    withKey (todoId todo) $ do
      expanded <- useVar False -- follows todoId, not the row index
      checkbox (todoText todo) expanded
-- ANCHOR_END: with_key_use_var

-- ANCHOR: with_elem_vars
visibleTodos :: Var [Todo] -> NanoUI ()
visibleTodos items =
  withElemVars todoId items $ \todos at ->
    forM_ (filter visible todos) $ \todo ->
      withKey (todoId todo) (todoRow (at todo))
-- ANCHOR_END: with_elem_vars

-- ANCHOR: overlay
deleteDialog :: NanoUI ()
deleteDialog = do
  confirm <- useVar False
  whenM (button "Delete...") (writeVar confirm True)

  modal confirm "Delete the file?" $
    whenM (button "Delete") (deleteFile >> writeVar confirm False)
-- ANCHOR_END: overlay

-- ANCHOR: retain
pruneRows :: VarMap Int RowState -> [Int] -> NanoUI ()
pruneRows rowVars liveIds = retainVars (`elem` liveIds) rowVars
-- ANCHOR_END: retain

-- Stand-ins for application code the snippets refer to.

data Todo = Todo {todoId :: Int, todoText :: Text, todoDone :: Bool}
  deriving (Eq, Generic)

newtype RowState = RowState Text

visible :: Todo -> Bool
visible = not . todoDone

todoRow :: Var Todo -> NanoUI ()
todoRow todo = checkbox "Done" todo.todoDone

deleteItem :: Int -> NanoUI ()
deleteItem _ = pure ()

deleteFile :: NanoUI ()
deleteFile = pure ()
