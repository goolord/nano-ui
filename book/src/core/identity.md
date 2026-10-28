# Widget Identity & Scoping

In `nano-ui`, widget IDs are generated deterministically based on invocation call order among siblings. Internal interactive state—such as text caret positions, active text selection, open dropdown menus, and scroll offsets—is keyed to these widget IDs.

## The Identity Rules

### 1. Consistent Call Order
Because widget IDs count up in traversal order, the structure of calls should remain stable across consecutive frames.

### 2. Wrap Conditional Subtrees in `scope`
If an element or section is conditionally displayed, wrapping it in `scope` reserves a single stable slot regardless of whether the branch executes:

```haskell
-- BAD: If showDetails is False, subsequent siblings shift IDs!
when showDetails $ do
  label "Details..."
  textInput detailsVar

-- GOOD: scope reserves a consistent slot in the parent hierarchy
scope $ when showDetails $ do
  label "Details..."
  textInput detailsVar
```

> [!NOTE]
> `useVar` maintains its own dedicated counter separate from layout widget IDs. While conditional widgets alone do not shift `useVar` slots, conditional `useVar` calls themselves still require `scope`.

### 3. Reorderable Lists with `withKey`
When rendering collections that can be sorted, filtered, inserted, or removed, **never key by list index**. Wrap each item in `withKey` with a unique, persistent identifier (such as an item ID or UUID):

```haskell
column $ for_ items $ \item -> do
  withKey item.id $ do
    row $ do
      label item.name
      whenM (button "Delete") (deleteItem item.id)
```

### 4. Retaining Dynamic State
State associated with dynamic keys is preserved while visible. For unbounded dynamic collections (such as streamed logs or ephemeral tabs), maintain row state in a `VarMap` and prune dead keys periodically with `retainVars`:

```haskell
retainVars activeKeys varMap
```
