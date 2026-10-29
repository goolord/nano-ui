-- | Code for the book's Tabs, Tables & Trees chapter
-- (@book/src/widgets/data-views.md@).
module Book.DataViews where

import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI

pages, closableTabs :: NanoUI ()

-- ANCHOR: bounded_tabs
data Page = Overview | Logs | Settings
  deriving (Eq, Show, Enum, Bounded)

pages = do
  page <- useEnumVar Overview
  boundedTabs (T.pack . show) page $ \case
    Overview -> overviewView
    Logs     -> logsView
    Settings -> settingsView
-- ANCHOR_END: bounded_tabs

-- ANCHOR: closable_tabs
closableTabs = do
  active <- useVar ("overview" :: Text)
  resp <- tabs' active
    [ closableTab "overview" "Overview" overviewView
    , closableTab "logs" "Logs" logsView
    ]
  for_ (tabClosed resp) closeTab
-- ANCHOR_END: closable_tabs

-- ANCHOR: tabs_config
segmented :: Var Text -> [Tab Text (NanoUI ())] -> NanoUI ()
segmented active docTabs =
  let cfg = defaultTabsConfig
        {tabsStyle = TabSegmented, tabsOrientation = TabTop, tabsTrailing = newTabButton}
   in tabsConfigured cfg active docTabs
-- ANCHOR_END: tabs_config

-- ANCHOR: table
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
      [ headed "PID" (T.pack . show . pid)
      , headed "Name" name
      , headed "Memory (MB)" (T.pack . show . mem)
      ]
-- ANCHOR_END: table

-- ANCHOR: tree
items :: [TreeItem]
items =
  [ TreeItem "src" [TreeItem "Main.hs" [], TreeItem "Widgets" []]
  , TreeItem "README.md" []
  ]

fileTree :: [TreeItem] -> Var Int -> NanoUI ()
fileTree = tree "file-tree"
-- ANCHOR_END: tree

-- Stand-ins for application code the snippets refer to.

overviewView, logsView, settingsView, newTabButton :: NanoUI ()
overviewView = pure ()
logsView = pure ()
settingsView = pure ()
newTabButton = pure ()

closeTab :: Text -> NanoUI ()
closeTab _ = pure ()
