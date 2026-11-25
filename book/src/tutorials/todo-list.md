# Tutorial 1: Build a Todo List

In this tutorial, you will build a todo list that supports adding items with Enter, editing rows inline, filtering by status (all, active, or completed), reordering items without scrambling per-row state, and clearing finished tasks. The full source lives in [`packages/nano-ui-demo/examples/TodoList.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/TodoList.hs).

Run the finished example with:

```sh
cabal run nano-ui-example-todo-list
```

## 1. The data model

Every todo item needs a stable ID that stays the same even when the list changes. List indices shift whenever an item is added, deleted, filtered, or moved; an explicit ID does not. Later, we will key every row's widgets and local `Var`s by this ID.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:model}}
```

Because `useVar` only handles a fixed set of primitive types, we create `Var Todos` once in `IO` during startup using `newVar`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:new_app}}
```

Creating the `Var` in `main` and passing it into the view lets it persist across frames. Calling `liftIO (newVar ...)` *inside* the view would allocate a brand-new `Var` on every single pass.

Both records derive `Generic`. With `OverloadedRecordDot`, `app.appTodos.todoItems` gives you a `Var [Todo]` focused on that one field, so changing the item list will not trigger repaints in widgets that only read `todoNext`.

## 2. The application shell

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:main}}
```

`runSdlApp` calls `todoApp` on every frame, and `todoApp` lays out the entire window from top to bottom each time:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:todo_app}}
```

Notice that the draft input text and the active filter do not need fields in `App`. Both are local to this view, so `useVar` and `useEnumVar` allocate them automatically based on their call position.

## 3. Adding an item

The input row displays a text field next to an Add button and inserts a new item when the user presses Enter or clicks the button.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:new_item_row}}
```

A few important details are at work here:

- `textInputConfigured'` (the primed form) returns a `Response` containing the widget's ID, bounds, hover/click flags, and `respSubmitted` (which turns `True` when the user presses Enter). By contrast, `button` returns just the `Bool` we need.
- `readVar draft` runs *below* the text input that edits it, so the read immediately sees the new text in the same pass. Reading above an editing widget forces a second pass of the view.
- `modifyVar` applies a pure function to the latest stored value, so multiple updates in one frame compose without overwriting each other.
- After inserting the item, we clear `draft` and call `requestFocus` so the user can keep typing the next task right away. Focus requests take effect at the end of the frame.

> [!NOTE]
> Always guard state mutations with an event condition. Because the view runs every frame, an unguarded `modifyVar` would insert a new todo on every single frame. Put event-driven work inside `whenM (button ...)` or an explicit `when` check like the one above.

## 4. Filtering and a derived count

The active filter has three values. `useEnumVar` works with any `Enum`/`Bounded` type and remembers the selection across frames:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:filter_and_count}}
```

`enumTabBarByConfigured` creates a segmented tab bar with one segment per constructor of `Filter`, using `show` for the labels. The active item count is *derived* on the fly from the current list rather than copied into a separate `Var` that could fall out of sync. Wrapping the clear button in `disabledWhen` dims it and ignores clicks without removing it from the layout.

## 5. Rows own their state

Each row tracks two pieces of local UI state: whether it is currently being edited, and the text currently in its edit box. Because the list can be filtered or reordered at any time, that state has to stay attached to the item's ID rather than its row position on screen.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:todo_list}}
```

- `withElemVars todoId items` returns the current list alongside an `at :: Todo -> Var Todo` lookup function. Each `at todo` tracks its element by `todoId`, so edits keep hitting the right item after filtering or reordering.
- `withKey (todoId todo)` ties the row's internal widget state (caret position, selection) and its `useVar` slots to the same ID. Without `withKey`, deleting the first item would shift every later row's edit mode and half-typed text up onto the wrong item.
- `matches` filters the visible items. We also pass each row its visible neighbors (`above` and `below`) so the Up and Down buttons move past hidden items instead of swapping with rows the user cannot see.

Here is the core structure of a single row:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:todo_row}}
```

Three patterns keep this row solid:

1. **`scope` wraps the label/input toggle.** Only one branch of the `if` runs on any given frame. Without `scope`, toggling between a label and a text input would shift the widget IDs of all siblings that follow. `scope` reserves one consistent slot either way (and the row's `useVar` slots stay isolated under `withKey`).
2. **Focus is requested on the frame the field appears.** `requestFocus` can only target a widget declared during the current pass. When the user clicks Edit, the text input will not exist until the next pass, so we set a `focusPending` flag and call `requestFocus` once the input is actually drawn.
3. **Field projections update items in place.** `todo.todoDone` is a `Var Bool` for that item's completion flag, which lets `checkbox` toggle it directly. Saving writes the draft into `todo.todoText` and leaves edit mode.

## 6. Reordering by ID

The Up and Down buttons need to swap two items in the full list, even when a filter hides rows between them. Looking up items by ID handles both filtered and unfiltered views:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:swap_ids}}
```

Because each row is keyed by `todoId`, any active edit state moves right along with the item.

## 7. Keyboard behavior

Pressing Enter in the input box adds an item via `respSubmitted`. While editing an existing row, Escape cancels the edit:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:take_escape}}
```

`takeEscape` consumes the Escape press so later handlers in the view (such as an open popup or another row) will not react to the same keystroke. Window exit policy is configured separately: the example sets `sdlAppShouldQuit = pressedOnceIn KeyEscape`, so Escape still exits the app unless you change that option or handle quitting inside the view.

For the complete keyboard model (`shortcut`, `shortcutOnce`, menu bindings, and how focused widgets claim keys), see [Keyboard, Focus & IME](../input/index.md).

## 8. Things to try next

- Replace the Up and Down buttons with drag-and-drop reordering using `useReorder` (see [Drag & Drop](../widgets/drag-and-drop.md)).
- Persist the active filter across sessions in a `Settings` record created with `newVar`.
- Display an empty-state message such as `muted "Nothing here yet."` when `shown` is empty.
- Add draft validation using `nano-ui-form` (see [Ecosystem Packages](../packages/index.md)).

Next: [Tutorial 2: Build a Notepad](notepad.md) introduces menu bars, native file dialogs, and full document editing.
