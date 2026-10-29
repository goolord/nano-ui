-- | Code for the book's State chapter (@book/src/state/index.md@).
module Book.State where

import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlApp)

-- ANCHOR: counter
counter :: NanoUI ()
counter = rowWith (gap 8 . alignMid) $ do
  n <- useVar (0 :: Int)
  whenM (button "-") (modifyVar n (subtract 1))
  label . T.pack . show =<< readVar n
  whenM (button "+") (modifyVar n (+ 1))
-- ANCHOR_END: counter

-- ANCHOR: keep_derive
appView :: App -> NanoUI ()
appView app = do
  -- Keep: the inputs to the next frame.
  draft <- useVar ""
  settings <- readVar app.appSettings
  textInput draft

  -- Derive: computed from what was kept.
  len <- T.length <$> readVar draft
  label ("Signed in as " <> settings.nickname)
  label ("Draft: " <> T.pack (show len) <> " characters")
-- ANCHOR_END: keep_derive

-- ANCHOR: conditional_use_var
advanced :: NanoUI ()
advanced = do
  showAdvanced <- useVar False
  checkbox "Advanced" showAdvanced
  scope . whenM (readVar showAdvanced) $ do
    level <- useVar (3 :: Float)
    slider 0 10 level
-- ANCHOR_END: conditional_use_var

-- ANCHOR: new_var
data App = App
  { appSettings :: Var Settings
  , appTodos :: Var [Todo]
  }
  deriving (Generic)

newApp :: IO App
newApp = App <$> newVar (Settings 40 False "Ada") <*> newVar []

main :: IO ()
main = do
  app <- newApp
  runSdlApp defaultSdlOptions (appView app)
-- ANCHOR_END: new_var

-- ANCHOR: var_map
data RowState = RowState {editing :: Bool, rowDraft :: Text}
  deriving (Eq, Generic)

newRows :: IO (VarMap Int RowState)
newRows = newVarMap

todoRows :: VarMap Int RowState -> [Int] -> NanoUI ()
todoRows rows liveIds = do
  for_ liveIds $ \todoId -> withKey todoId $ do
    rowState <- varAt rows todoId (RowState False "")
    checkbox "Editing" rowState.editing
  retainVars (`elem` liveIds) rows
-- ANCHOR_END: var_map

-- ANCHOR: projections
data Settings = Settings
  { volume :: !Float
  , silent :: !Bool
  , nickname :: !Text
  }
  deriving (Eq, Generic)

settingsView :: Var Settings -> NanoUI ()
settingsView settings = do
  slider 0 100 settings.volume
  checkbox "Silent" settings.silent
  textInput settings.nickname
-- ANCHOR_END: projections

-- ANCHOR: handles
data Editor = Editor
  { editorDocument :: Var TextDocument
  , editorSearch :: Task Text [Text]
  }

newEditor :: IO Editor
newEditor = Editor <$> newVar emptyDocument <*> newTask
-- ANCHOR_END: handles

-- Stand-ins for application code the snippets refer to.

data Todo = Todo {todoId :: Int, todoText :: Text}
  deriving (Eq)
