# Tutorial 1: Build a Todo List

A todo list that adds items with Enter, edits them in place, filters by all/active/done, reorders rows with their local state, and clears completed items. The complete program is [`packages/nano-ui-demo/examples/TodoList.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/TodoList.hs). Run it with:

```sh
cabal run nano-ui-example-todo-list
```

## 1. The data model

A todo item needs an identity that outlives its position. Positions change when an item is added, removed, filtered, or reordered; the id does not. Every widget or `Var` that belongs to a row is keyed by that id later.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:model}}
```

The list lives in a `Var Todos`. That is not a type `useVar` can allocate, so it is created once during setup with `newVar`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:new_app}}
```

Allocated in `main` and passed into the view, it survives between frames. A `Var` allocated *inside* the view would be fresh on every pass.

Both records derive `Generic`; with `OverloadedRecordDot`, `app.appTodos.todoItems` is a `Var [Todo]` projecting just that field. Editing the list does not repaint a widget that reads only `todoNext`.

## 2. The application shell

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:main}}
```

`runSdlApp` calls the view every frame, and the view describes the whole window each time.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:todo_app}}
```

The draft text and the filter are view-local, allocated with `useVar` and `useEnumVar`. Both are identified by their call site, so they need no setup and no record field.

## 3. Adding an item

The input row draws the field and the button, and adds an item on Enter or a click.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:new_item_row}}
```

- **The primed widget returns a `Response`.** `textInputConfigured'` reports the widget's identity, rectangle, hover and click state, and whether Enter was submitted (`respSubmitted`). `button` already returns the one `Bool` this row needs.
- **`readVar draft` sits below the field that edits it.** The field's write lands first, then the read sees the new text in the same pass. Reading *above* an editing widget forces a second pass.
- **`modifyVar` reads the latest value.** Two changes in one frame both apply; the append does not lose one.
- **The field is cleared and focused again**, so the next item can be typed immediately. `requestFocus` takes effect at the end of the frame.

> [!NOTE]
> The `when` guard around the mutation is not optional. The view runs every frame, so an unguarded `modifyVar` would add an item on every pass. Event-driven work goes inside `whenM (button ...)` or a condition like the one above.

## 4. Filtering and a derived count

The filter is a local value with three states. `useEnumVar` accepts any `Enum`/`Bounded` type and keeps the chosen value across frames:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:filter_and_count}}
```

`boundedTabBarConfigured` draws a segmented control over every constructor of `Filter`; the label function is `show`. The count is *derived*: it is computed from the list every frame, never stored in a second `Var` that could drift. `disabledWhen` dims the clear button and blocks its events without removing it from the layout.

## 5. Rows own their state

Each row needs local state (whether it is being edited and the draft of its text), but the list can be filtered and reordered underneath it. Per-row state must follow the item, not the row's position on screen.

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:todo_list}}
```

- `withElemVars todoId items` gives the current list and a function `at :: Todo -> Var Todo`. `at todo` is a `Var` addressed by the item's id, so it survives a reorder or a filter change.
- `withKey (todoId todo)` does the same for the row's *widget* state and its positional `useVar` slots. Without the key, deleting the first row would move every later row's caret, edit flag, and half-typed draft one position up.
- `matches` filters the rows. The neighbours (`above`, `below`) are passed in so the Up/Down buttons can skip rows that the filter hides.

Here is the row itself, trimmed to the essential structure:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:todo_row}}
```

Three details make this work:

1. **`scope` around the label/field swap.** Only one of the two branches runs on a given frame. Without `scope`, the widgets after the branch would shift ids whenever the row enters or leaves edit mode. `scope` reserves one slot either way. (The `useVar` slots inside the row are stable because the whole row is under `withKey`.)
2. **A delayed focus request.** `requestFocus` can only target a widget declared in the same pass. The field first appears in the pass *after* Edit was clicked, so the request waits in `focusPending` until the field exists.
3. **Editing writes back into the item's `Var`.** `todo.todoText` projects one field; `todo.todoDone` does too, which is why the checkbox can edit it in place. Saving writes the draft and clears the edit flag.

## 6. Reordering by id

Up and Down need to swap two items wherever they are in the full list. Swapping by id keeps the operation correct even though the visible list is filtered:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:swap_ids}}
```

The row's local state follows the id, so the edit flag and draft travel with the item.

## 7. Keyboard behaviour

Enter in the new-item field adds an item because `respSubmitted` reports it. Escape cancels an edit:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/TodoList.hs:take_escape}}
```

`takeEscape` claims the press for this handler, so later Escape handlers in the view (an open popup or a second `takeEscape`) do not also act on it. The window's quit policy is separate: the example sets `sdlAppShouldQuit = pressedOnceIn KeyEscape`, so Escape still quits the application unless the application chooses another policy (or handles Escape itself in the view and leaves the predicate off).

For the full shortcut model (`shortcut`, `shortcutOnce`, menu bindings, and how focused widgets claim keys), see [Keyboard, Focus & IME](../input/index.md).

## 8. Try it

- Replace the Up/Down buttons with `useReorder` for drag-and-drop reordering (see [Drag & Drop](../widgets/drag-and-drop.md)).
- Remember the filter across runs in a `Settings` record allocated with `newVar`.
- Show an empty state when `shown` is empty, for example `muted "Nothing here yet."`
- Validate the draft length with the techniques in [Ecosystem Packages](../packages/index.md).

Next: [Tutorial 2: Build a Notepad](notepad.md) adds menus, dialogs, and a real text document.
