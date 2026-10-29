# Widget Identity & Keys

Widget state (caret position, text selection, an open drop-down, a scroll offset, per-row local `Var`s) is stored under the widget's **id**. Ids come from the call order among siblings. If the order changes, the state jumps to a different widget.

## How ids are assigned

Each container assigns ids to its children counting from zero. A `column` with three labels has children 0, 1, and 2; a nested `row` starts its own count.

Two separate id spaces exist:

- Widget ids, for layout nodes, focus, input state, and scroll state.
- `useVar` slots, counted in call order, with their own counter and key space.

A conditional *widget* does not shift `useVar` slots, and a conditional *`useVar`* does not shift widget ids. Both still shift their own kind; `scope` prevents that.

## Rule 1: keep the call order stable

The structure of the calls should be the same on every frame. Straight-line code and ordinary `if`/`case` over widget *values* are fine; conditionally *calling* a widget changes the count for everything after it.

## Rule 2: wrap conditional subtrees in `scope`

`scope` reserves one slot whether or not its body runs, so later siblings keep their ids:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:scope}}
```

The same applies to a branch that swaps two widgets:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:scope_branch}}
```

`scope` is cheap. Use it around anything that may not run every frame: a conditional block, a `case` whose branches declare different widgets, a loading placeholder. Overlays are the exception: call them unconditionally (Rule 4).

## Rule 3: key dynamic lists with `withKey`

Never key a list item by its index. Wrap each item in `withKey` with a stable, sibling-unique key: an item id, a UUID, a file path.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:with_key}}
```

Without the key, removing the first row makes every later row inherit the previous row's caret, selection, and local `Var`s. With it, the row's state travels with the item through filters, sorts, and reorders.

`withKey` is also how per-row `useVar` calls stay attached to their row:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:with_key_use_var}}
```

The full todo-list tutorial builds this up step by step: [Build a Todo List](../tutorials/todo-list.md).

### Lists that use `Var`s per element

For a list stored in a `Var [a]`, `forEachVar key list body` gives each element a `Var a` and puts the body under the element's key automatically. `withElemVars` does the same but passes the elements and an `at` accessor, for when you filter or sort the list yourself:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:with_elem_vars}}
```

## Rule 4: call overlays unconditionally

`modal`, `window`, and `popup` take a `Var Bool` for their open state and draw nothing while it is false. Call them every frame:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:overlay}}
```

Wrapping the call in `when` shifts every later sibling's id each time the dialog opens or closes. For derived open state, pass `constVar` and use the primed variants.

## Rule 5: retain state for dynamic collections

Widget and `Var` state under a key survives the key not being drawn for a while: hide a filtered row and show it again, and its draft is still there. That state is not freed automatically. For an unbounded collection (a chat log, a set of tabs, a streaming table), keep the per-row state in a `VarMap` and prune it periodically:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Identity.hs:retain}}
```

Prune too little and state for deleted rows accumulates; prune too eagerly and a filtered-out row loses its draft. Choose per collection: a todo row can keep its draft while hidden, a row from a 100,000-row log cannot.

## Symptoms and their fixes

| Symptom | Likely cause |
| --- | --- |
| Typing in one field moves the caret in another | A conditional widget before it changed the id count; wrap the branch in `scope`. |
| Deleting a row resets the next row's edit state | The list is keyed by index; use `withKey item.id`. |
| A modal's fields do not respond after opening another modal | The modal is called conditionally; call it unconditionally with a `Var Bool`. |
| State appears in the wrong row after sorting | Per-row `useVar`s are outside `withKey`, or the key is the index. |
| Memory grows with every streamed entry | Per-row state is in an unbounded `VarMap`; `retainVars`. |

For allocation, projections, and write costs, read [State & `Var`](index.md). For the lifecycle that makes ids reset, read [Architecture & Internals](../architecture/index.md).
