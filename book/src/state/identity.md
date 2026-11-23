# Widget Identity & Keys

Every widget's internal state (caret position, text selection, open dropdowns, scroll offsets, and per-row `useVar` slots) is stored under its **widget ID**. By default, IDs are assigned by sibling call order. If that call order shifts between frames, internal state jumps to the wrong widget.

## How IDs are assigned

Each container numbers its children starting from zero. A `column` containing three labels gives them child IDs `0`, `1`, and `2`, and a nested `row` starts its own counter from `0`.

`nano-ui` keeps two separate ID spaces:

- **Widget IDs**, which back layout nodes, keyboard focus, input state, and scroll offsets.
- **`useVar` slots**, which have their own sequential counter and key space.

Adding or hiding a widget will not shift `useVar` slots, and skipping a `useVar` will not shift widget IDs. Each still shifts later calls of its own kind, which is where `scope` comes in.

## Rule 1: keep call order stable

Try to run the same sequence of widget calls on every frame. Branching on a widget's *arguments* (such as changing a label's text or a button's style) is always safe. Conditionally *skipping* a widget call changes the index of every sibling declared after it.

## Rule 2: wrap conditional subtrees in `scope`

`scope` reserves one fixed slot whether its body runs or not, keeping every later sibling's ID intact:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:scope}}
```

Use `scope` whenever a branch swaps between different widgets too:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:scope_branch}}
```

`scope` is cheap, so reach for it whenever a block might be skipped or change shape: conditional panels, `case` expressions whose branches render different widgets, or loading placeholders. The only exception is overlays, which should be called unconditionally (Rule 4).

## Rule 3: key dynamic lists with `withKey`

Never key list rows by their array index. Wrap each item in `withKey` using a stable key that is unique among its siblings, such as a database ID, UUID, or file path:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:with_key}}
```

Without `withKey`, deleting the first row shifts every remaining row up by one index, so each row inherits the previous row's caret, text selection, and local `Var`s. With a stable key, a row's state stays attached to the item across filters, sorts, and reorders.

`withKey` scopes `useVar` slots as well, so per-row local variables travel with the row:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:with_key_use_var}}
```

For a complete worked example, see [Tutorial 1: Build a Todo List](../tutorials/todo-list.md).

### Lists with per-element `Var`s

When your list is stored in a `Var [a]`, `forEachVar key list body` gives each element its own `Var a` and automatically wraps the body in `withKey`. If you need to filter or sort the list yourself before drawing it, use `withElemVars`, which hands you the current elements along with an `at` lookup function:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:with_elem_vars}}
```

## Rule 4: call overlays unconditionally

`modal`, `window`, and `popup` each take a `Var Bool` controlling whether they are open, and they render nothing when that flag is `False`. Call them on every frame:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:overlay}}
```

Putting a modal or popup inside `when isOpen` shifts the IDs of following siblings whenever it opens or closes. If your open state is derived from other data instead of a `Var Bool`, pass `constVar` and handle close events through the primed variants (`modal'`, `window'`, `popup'`).

## Rule 5: prune state for unbounded collections

Keyed widget state and `useVar` slots stay in memory even while their key is not drawn. That is handy when a user filters a list and clears the filter: any half-typed draft in a hidden row is still there when the row returns.

However, that state is never garbage-collected on its own. For collections that grow indefinitely (chat logs, streaming tables, open tabs over long sessions), store per-row state in a `VarMap` and prune deleted keys with `retainVars`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:retain}}
```

Balance retention per collection: keeping drafts for hidden todo items makes sense, while keeping state for 100,000 scrolled-out log entries does not.

## Symptoms and quick fixes

| Symptom | Likely cause and fix |
| --- | --- |
| Typing in one field moves the caret in another | A conditional widget above it shifted the sibling count. Wrap the conditional block in `scope`. |
| Deleting a row corrupts the next row's edit state | The list is keyed by position. Wrap each row in `withKey item.id`. |
| A modal's inputs stop responding after opening another modal | The modal was called inside `when`. Call it unconditionally with a `Var Bool`. |
| Row state jumps to the wrong item after sorting | Per-row `useVar` calls sit outside `withKey`, or the key is the list index. |
| Memory keeps climbing as new items stream in | Per-row state is accumulating in an unpruned `VarMap`. Prune stale keys with `retainVars`. |

For `Var` allocation, projections, and write costs, see [State & `Var`](index.md). To see how frame passes interact with IDs and state, read [Architecture & Internals](../architecture/index.md).
