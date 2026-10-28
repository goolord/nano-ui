# Tabs, Tables & Trees

Widgets that show collections: tab strips, sortable tables, and hierarchical trees.

## Tabs

A tab strip binds the active key to a `Var`; only the selected tab's body runs:

```haskell
data Page = Overview | Logs | Settings
  deriving (Eq, Show, Enum, Bounded)

page <- useEnumVar Overview
boundedTabs (T.pack . show) page $ \case
  Overview -> overviewView
  Logs     -> logsView
  Settings -> settingsView
```

`tab` creates a plain header; `closableTab` adds a close button and reports the close through the primed response:

```haskell
active <- useVar ("overview" :: Text)
resp <- tabs' active
  [ closableTab "overview" "Overview" overviewView
  , closableTab "logs" "Logs" logsView
  ]
for_ (tabClosed resp) closeTab
let activeKey = tabActive resp
```

The caller owns removal: `tabs'` reports `tabClosed`, it does not remove the tab. A key no tab has selects the first tab and writes that key back to the `Var`.

`tabBar` / `tabBar'` draw headers only and leave the body to the caller — useful for pane layouts and custom bodies. `tabsConfigured` / `tabBarConfigured` take a `TabsConfig`:

```haskell
let cfg = defaultTabsConfig
      {tabsStyle = TabSegmented, tabsOrientation = TabTop, tabsTrailing = newTabButton}
tabsConfigured cfg active tabs
```

Styles: `TabUnderline`, `TabPill`, `TabSegmented`, `TabContained`. Placements: `TabTop`, `TabBottom`, `TabLeft`, `TabRight`. `boundedTabs` / `boundedTabBar` generate a tab per enum value.

## Tables

`table` renders Colonnade columns. Header clicks sort through a `Var SortCol`:

```haskell
data Process = Process
  { pid :: Int
  , name :: Text
  , mem :: Int
  }

processTable :: [Process] -> Var SortCol -> NanoUI ()
processTable procs sortVar =
  table "procs" cols procs sortVar
  where
    cols = mconcat
      [ headed "PID" (\p -> label (T.pack (show (pid p))))
      , headed "Name" (\p -> label (name p))
      , headed "Memory (MB)" (\p -> label (T.pack (show (mem p))))
      ]
```

`SortCol` holds the active column and direction. `simpleTable headers rows` renders a table of text rows. `table'` returns a `TableResponse` with the updated sort, column order, and hidden columns; `tableConfigured` adds frozen rows/columns, fixed column widths, and hidden columns through `TableConfig`. Rows are virtualized internally.

Cell helpers: `textCell`, `cellWidget`, `widgetCell`, and `Cell` with an explicit sort key (`cellKey`). A cell with a widget view still needs `cellText` if its natural width should be measured.

## Trees

```haskell
items :: [TreeItem]
items =
  [ TreeItem "src" [TreeItem "Main.hs" [], TreeItem "Widgets" []]
  , TreeItem "README.md" []
  ]

fileTree :: [TreeItem] -> Var Int -> NanoUI ()
fileTree = tree "file-tree"
```

`TreeItem` is the library type (`treeItemLabel`, `treeItemChildren`), not an app-defined record. `tree` reports the selected **pre-order index** through the `Var Int`, not an item id. Build the index in the same pre-order when you need to map a selection back to an item. Collapsed nodes keep their state; rows are drawn as focusable buttons and support arrow-key navigation.

For very large trees, keep the expansion set in a `VarMap` keyed by a stable path, and rebuild the visible items before calling `tree`.

## See also

- The rest of the widget family: [Widget Gallery](index.md).
- Styling tables and tabs: [Styling & Theming](../styling/index.md) and the [lab](../styling/lab.md).
- Drag-and-drop tab reordering: [Drag & Drop](drag-and-drop.md).
