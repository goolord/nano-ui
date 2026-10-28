# Tables & Trees

Data presentation widgets in `nano-ui` support sorting, custom cell rendering, and hierarchical tree navigation.

## Tables (Colonnade-Based)

`table` and `simpleTable` render structured columns with sortable headers:

```haskell
data Process = Process
  { pid  :: Int
  , name :: Text
  , mem  :: Int
  }

processTable :: [Process] -> Var SortCol -> NanoUI ()
processTable procs sortVar = do
  let cols = mconcat
        [ headed "PID" (\p -> label (T.pack (show p.pid)))
        , headed "Name" (\p -> label p.name)
        , headed "Memory (MB)" (\p -> label (T.pack (show p.mem)))
        ]
  table "procs" cols procs sortVar
```

`SortCol` tracks which column is active and whether the order is ascending or descending.

---

## Trees

`tree` renders collapsible hierarchical collections:

```haskell
data TreeItem = TreeItem
  { tiId       :: Int
  , tiLabel    :: Text
  , tiChildren :: [TreeItem]
  }

treeView :: [TreeItem] -> Var Int -> NanoUI ()
treeView items selectedIdVar = do
  tree "file-tree" items selectedIdVar
```

`selectedIdVar` reflects the currently selected item and updates on click or arrow navigation.
