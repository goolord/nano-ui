# Tutorial 1: Build a Todo List

This tutorial builds a todo list with the features that make a list worth using:

- add items with Enter or a button
- edit an item in place
- mark items done
- filter by all / active / done
- reorder items, keeping each row's local state with it
- clear completed items

The complete program is [`packages/nano-ui-demo/examples/TodoList.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/TodoList.hs). Run it with:

```sh
cabal run nano-ui-example-todo-list
```

## 1. The data model

A todo item needs an identity that outlives its position. Positions change when an item is added, removed, filtered, or reordered; the id does not. Every widget or `Var` that belongs to a row is keyed by that id later.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

data Todo = Todo
  { todoId :: !Int
  , todoText :: !Text
  , todoDone :: !Bool
  }
  deriving (Eq, Generic)

data Todos = Todos
  { todoItems :: ![Todo]
  , todoNext :: !Int
  }
  deriving (Eq, Generic)

newtype App = App {appTodos :: Var Todos}
```

The list lives in a `Var Todos`. That is not a type `useVar` can allocate, so it is created once during setup with `newVar`:

```haskell
newApp :: IO App
newApp = App <$> newVar (Todos [Todo 0 "Read the header" True, Todo 1 "Water the plants" False] 2)
```

`newVar` returns `IO (Var a)`. Allocating it in `main` and passing it into the view is what makes it survive between frames. Allocating a `Var` *inside* the view would create a fresh one on every pass.

Both records derive `Generic`; with `OverloadedRecordDot`, `app.appTodos.todoItems` is a `Var [Todo]` projecting just that field. Editing the list does not repaint a widget that reads only `todoNext`.

## 2. The application shell

```haskell
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
```

`runSdlApp` takes the options and the view. The view is not called once: the backend calls it every frame, and it must describe the whole window each time.

```haskell
todoApp :: App -> NanoUI ()
todoApp app = do
  let items = app.appTodos.todoItems
  draft <- useVar ""
  filt <- useEnumVar All
  columnWith (padAll 20 . gap 12 . fillW . fillH) $ do
    heading "Todos"
    newItemRow app.appTodos draft
    filterRow items filt
    todoList items filt
```

The view's local state — the draft text and the current filter — is allocated with `useVar` and `useEnumVar`. Both are positional: they are identified by their call site, so they need no setup and no record field.

`columnWith (padAll 20 . gap 12 . fillW . fillH)` is a layout argument. Modifiers compose left to right, and the leftmost one wins when two set the same property.

## 3. Adding an item

The input row has three jobs: draw the field, draw the button, and decide whether this frame is an "add" frame.

```haskell
newItemRow :: Var Todos -> Var Text -> NanoUI ()
newItemRow todosVar draft = rowWith (tight . gap 8 . alignMid . fillW) $ do
  inputR <- textInputConfigured' defaultTextInputConfig {ticPlaceholder = "What needs doing?"} draft
  clicked <- button "Add"
  text <- T.strip <$> readVar draft
  when ((clicked || respSubmitted inputR) && not (T.null text)) $ do
    modifyVar todosVar (\(Todos xs n) -> Todos (xs <> [Todo n text False]) (n + 1))
    writeVar draft ""
    requestFocus (respId inputR)
```

Several things are worth pausing on:

- **The primed widget returns a `Response`.** `textInputConfigured'` reports the widget's identity, rectangle, hover and click state, and whether Enter was submitted (`respSubmitted`). `button` already returns the one `Bool` this row needs.
- **`readVar draft` sits below the field that edits it.** The field's write lands first, then the read sees the new text in the same pass. Reading *above* an editing widget forces a second pass.
- **`modifyVar` reads the latest value.** Two changes in one frame both apply; the append does not lose one.
- **The field is cleared and focused again**, so the next item can be typed immediately. `requestFocus` takes effect at the end of the frame.

> [!NOTE]
> The `when` guard around the mutation is not optional. The view runs every frame, so an unguarded `modifyVar` would add an item on every pass. Event-driven work goes inside `whenM (button ...)` or a condition like the one above.

## 4. Filtering and a derived count

The filter is a local value with three states. `useEnumVar` accepts any `Enum`/`Bounded` type and keeps the chosen value across frames:

```haskell
data Filter = All | Active | Done
  deriving (Eq, Show, Enum, Bounded)

matches :: Filter -> Todo -> Bool
matches All _ = True
matches Active t = not (todoDone t)
matches Done t = todoDone t

filterRow :: Var [Todo] -> Var Filter -> NanoUI ()
filterRow items filt = rowWith (tight . gap 12 . alignMid . fillW) $ do
  boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} (T.pack . show) filt
  flex
  todos <- readVar items
  let left = length (filter (not . todoDone) todos)
  labelWith (tight . alignMid . fontMuted)
    (T.pack (show left) <> if left == 1 then " item left" else " items left")
  disabledWhen (left == length todos) $
    whenM (button "Clear completed") (modifyVar items (filter (not . todoDone)))
```

`boundedTabBarConfigured` draws a segmented control over every constructor of `Filter`; the label function is `show`. The count is *derived*: it is computed from the list every frame, never stored in a second `Var` that could drift. `disabledWhen` dims the clear button and blocks its events without removing it from the layout.

## 5. Rows own their state

Now the interesting part. Each row has local state — whether it is being edited and the draft of its text — but the list can be filtered and reordered underneath it. Per-row state must follow the item, not the row's position on screen.

```haskell
todoList :: Var [Todo] -> Var Filter -> NanoUI ()
todoList items filt = do
  shownFilter <- readVar filt
  scrollWith (grow . fillW) $
    columnWith (tight . gap 6 . fillW) $
      withElemVars todoId items $ \todos at -> do
        let shown = filter (matches shownFilter) todos
        forM_ (zip3 shown (Nothing : map Just shown) (map Just (drop 1 shown) <> [Nothing])) $
          \(todo, above, below) ->
            withKey (todoId todo) (todoRow items (at todo) above below)
```

- `withElemVars todoId items` gives the current list and a function `at :: Todo -> Var Todo`. `at todo` is a `Var` addressed by the item's id, so it survives a reorder or a filter change.
- `withKey (todoId todo)` does the same for the row's *widget* state and its positional `useVar` slots. Without the key, deleting the first row would move every later row's caret, edit flag, and half-typed draft one position up.
- `matches` filters the rows. The neighbours (`above`, `below`) are passed in so the Up/Down buttons can skip rows that the filter hides.

Here is the row itself, trimmed to the essential structure:

```haskell
todoRow :: Var [Todo] -> Var Todo -> Maybe Todo -> Maybe Todo -> NanoUI ()
todoRow items todo above below = do
  editing <- useVar False
  focusPending <- useVar False
  draft <- useVar ""
  t <- readVar todo
  isEditing <- readVar editing
  let tid = todoId t
      save = do
        writeVar todo.todoText =<< readVar draft
        writeVar editing False
  when isEditing (whenM takeEscape (writeVar editing False))
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    checkboxWith alignMid "" todo.todoDone
    scope $
      if isEditing
        then do
          fieldR <- textInput' draft
          whenM (readVar focusPending) $
            requestFocus (respId fieldR) >> writeVar focusPending False
          when (respSubmitted fieldR) save
        else
          labelWith (fillW . alignMid . (if todoDone t then fontStrike . fontMuted else id)) (todoText t)
    whenM (button (if isEditing then "Save" else "Edit")) $
      if isEditing then save else do
        writeVar draft (todoText t)
        writeVar editing True
        writeVar focusPending True
    disabledWhen (null above) $ whenM (button "Up") (forM_ above (modifyVar items . swapIds tid . todoId))
    disabledWhen (null below) $ whenM (button "Down") (forM_ below (modifyVar items . swapIds tid . todoId))
    whenM (styled destructive (button "Delete")) (modifyVar items (filter ((/= tid) . todoId)))
```

Three details make this work:

1. **`scope` around the label/field swap.** Only one of the two branches runs on a given frame. Without `scope`, the widgets after the branch would shift ids whenever the row enters or leaves edit mode. `scope` reserves one slot either way. (The `useVar` slots inside the row are stable because the whole row is under `withKey`.)
2. **A delayed focus request.** `requestFocus` can only target a widget declared in the same pass. The field first appears in the pass *after* Edit was clicked, so the request waits in `focusPending` until the field exists.
3. **Editing writes back into the item's `Var`.** `todo.todoText` projects one field; `todo.todoDone` does too, which is why the checkbox can edit it in place. Saving writes the draft and clears the edit flag.

## 6. Reordering by id

Up and Down need to swap two items wherever they are in the full list. Swapping by id keeps the operation correct even though the visible list is filtered:

```haskell
swapIds :: Int -> Int -> [Todo] -> [Todo]
swapIds a b xs = map pick xs
  where
    byId i t = fromMaybe t (find ((== i) . todoId) xs)
    pick t
      | todoId t == a = byId b t
      | todoId t == b = byId a t
      | otherwise = t
```

The row's local state follows the id, not the index, so the edit flag and draft travel with the item. This is the payoff for the keys in the previous section.

## 7. Keyboard behaviour

Enter in the new-item field adds an item because `respSubmitted` reports it. Escape cancels an edit:

```haskell
when isEditing (whenM takeEscape (writeVar editing False))
```

`takeEscape` claims the press for this handler, so later Escape handlers in the view — an open popup or a second `takeEscape` — do not also act on it. The window's quit policy is separate: the example sets `sdlAppShouldQuit = pressedOnceIn KeyEscape`, so Escape still quits the application unless the application chooses another policy (or handles Escape itself in the view and leaves the predicate off).

For the full shortcut model — `shortcut`, `shortcutOnce`, menu bindings, and how focused widgets claim keys — see [Keyboard, Focus & IME](../input/index.md).

## 8. Try it

Start from the runnable source and change one thing at a time:

- Replace the Up/Down buttons with `useReorder` for drag-and-drop reordering (see [Drag & Drop](../widgets/drag-and-drop.md)).
- Add a "hide completed" filter persistence setting with `newVar` and a `Settings` record.
- Show an empty state when `shown` is empty, for example `muted "Nothing here yet."`
- Validate the draft length with the techniques in [Ecosystem Packages](../packages/index.md).

Next: [Tutorial 2: Build a Notepad](notepad.md) adds menus, dialogs, and a real text document.
