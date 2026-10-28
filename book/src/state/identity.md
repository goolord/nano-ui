# Widget Identity & Keys

Widget state (caret position, text selection, an open drop-down, a scroll offset, per-row local `Var`s) is stored under the widget's **id**, not in the widget. Ids are generated deterministically from the call order among siblings. If the order changes, the state appears to jump to a different widget. This section explains how to keep the order stable.

## How ids are assigned

Each container assigns ids to its children counting from zero. A `column` with three labels has children 0, 1, and 2; a nested `row` starts its own count. Ids are per container, not global.

Two separate id spaces exist:

- **Widget ids** for layout nodes, focus, input state, and scroll state.
- **`useVar` slots**, counted in call order, with their own counter and key space.

That separation means a conditional *widget* does not shift `useVar` slots, and a conditional *`useVar`* does not shift widget ids. Both still need their position kept stable if they come and go, which is what `scope` does.

## Rule 1: keep the call order stable

The structure of the calls should be the same on every frame. Straight-line code and ordinary `if`/`case` over widget *values* are fine; conditionally *calling* a widget changes the count for everything after it.

## Rule 2: wrap conditional subtrees in `scope`

`scope` reserves one slot whether or not its body runs, so later siblings keep their ids:

```haskell
-- BAD: when showDetails is False, every later sibling shifts down one id.
when showDetails $ do
  label "Details..."
  textInput detailsVar

-- GOOD: one stable slot, whether or not the body runs.
scope $
  when showDetails $ do
    label "Details..."
    textInput detailsVar
```

The same applies to a branch that swaps two widgets:

```haskell
scope $
  if isEditing
    then textInput draft
    else label (todoText todo)
```

`scope` is cheap and local. Use it around anything that may not run on every frame: a conditional block, a case whose branches declare different widgets, a placeholder that appears while loading. Overlays are the exception with a simpler rule: call them unconditionally (see Rule 4).

## Rule 3: key dynamic lists with `withKey`

Never key a list item by its index. Wrap each item in `withKey` with a stable, sibling-unique key: an item id, a UUID, a file path.

```haskell
column $ for_ items $ \todo ->
  withKey (todoId todo) $ do
    row $ do
      label (todoText todo)
      whenM (button "Delete") (deleteItem (todoId todo))
```

Without the key, removing the first row makes every later row inherit the previous row's caret, selection, and local `Var`s. With it, the row's state travels with the item through filters, sorts, and reorders.

`withKey` is also how per-row `useVar` calls stay attached to their row:

```haskell
for_ items $ \todo ->
  withKey (todoId todo) $ do
    expanded <- useVar False      -- follows todoId, not the row index
    ...
```

The full todo-list tutorial builds this up step by step: [Build a Todo List](../tutorials/todo-list.md).

### Lists that use `Var`s per element

For a list stored in a `Var [a]`, `forEachVar key list body` gives each element a `Var a` and puts the body under the element's key automatically. `withElemVars` does the same but passes the elements and an `at` accessor, for when you filter or sort the list yourself:

```haskell
withElemVars todoId items $ \todos at ->
  forM_ (filter visible todos) $ \todo ->
    withKey (todoId todo) (row (at todo))
```

## Rule 4: call overlays unconditionally

`modal`, `window`, and `popup` take a `Var Bool` for their open state and draw nothing while it is false. Call them every frame:

```haskell
confirm <- useVar False
whenM (button "Delete...") (writeVar confirm True)

modal confirm "Delete the file?" $
  whenM (button "Delete") (deleteFile >> writeVar confirm False)
```

Wrapping the call in `when (readVar confirm)` shifts every later sibling's id when the dialog opens and closes, which is exactly the jump `scope` and keys exist to prevent. For derived open state, pass `constVar` and use the primed variants.

## Rule 5: retain state for dynamic collections

Widget and `Var` state under a key survives the key not being drawn for a while: hide a filtered row and show it again, and its draft is still there. That state is not freed automatically. For an unbounded collection (a chat log, a set of tabs, a streaming table), keep the per-row state in a `VarMap` and prune it periodically:

```haskell
retainVars (`elem` liveIds) rowVars
```

Without pruning, a long-running application accumulates state for rows that no longer exist. With aggressive pruning, state disappears the moment a row is filtered out; choose the policy per collection. A todo row can keep its draft while hidden; a row from a 100,000-row log cannot.

## Symptoms and their fixes

| Symptom | Likely cause |
| --- | --- |
| Typing in one field moves the caret in another | A conditional widget before it changed the id count; wrap the branch in `scope`. |
| Deleting a row resets the next row's edit state | The list is keyed by index; use `withKey item.id`. |
| A modal's fields do not respond after opening another modal | The modal is called conditionally; call it unconditionally with a `Var Bool`. |
| State appears in the wrong row after sorting | Per-row `useVar`s are outside `withKey`, or the key is the index. |
| Memory grows with every streamed entry | Per-row state is in an unbounded `VarMap`; `retainVars`. |

For allocation, projections, and write costs, read [State & `Var`](index.md). For the lifecycle that makes ids reset, read [Architecture & Internals](../architecture/index.md).
