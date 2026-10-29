-- | Code for the book's Architecture chapter
-- (@book/src/architecture/index.md@).
module Book.Architecture where

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import NanoUI
import NanoUI.Backend.Sdl (defaultSdlOptions, runSdlApp)

-- ANCHOR: main
main :: IO ()
main = do
  env <- newEnv
  runSdlApp defaultSdlOptions (appView env)
-- ANCHOR_END: main

-- ANCHOR: pattern_a
data Env = Env
  { envState :: IORef AppState
  , envWake  :: IORef (IO ())
  }

appView :: Env -> NanoUI ()
appView env = do
  wake <- askWake
  liftIO $ writeIORef env.envWake wake
  state <- liftIO $ readIORef env.envState
  mainLayout state

-- On a worker thread:
publish :: Env -> [Text] -> IO ()
publish env results = do
  atomicModifyIORef' env.envState (\s -> (s {packages = results}, ()))
  wake <- readIORef env.envWake
  wake
-- ANCHOR_END: pattern_a

-- ANCHOR: pattern_b
data App = App
  { filterText   :: Var Text
  , activeTab    :: Var TabId
  , zoomLevel    :: Var Float
  , isInspecting :: Var Bool
  }

newApp :: IO App
newApp = App <$> newVar "" <*> newVar TabOverview <*> newVar 1.0 <*> newVar False
-- ANCHOR_END: pattern_b

-- ANCHOR: pattern_c
editorView :: Editor -> NanoUI ()
editorView (Editor ref) = do
  app <- liftIO (readIORef ref)
  renderEditor app
  app' <- liftIO (readIORef ref)
  when (viewChanged app app') requestFrame
-- ANCHOR_END: pattern_c

-- Stand-ins for application code the snippets refer to.

newtype AppState = AppState {packages :: [Text]}

data TabId = TabOverview | TabDetails

newtype EditorState = EditorState Int
  deriving (Eq)

newtype Editor = Editor (IORef EditorState)

newEnv :: IO Env
newEnv = Env <$> newIORef (AppState []) <*> newIORef (pure ())

mainLayout :: AppState -> NanoUI ()
mainLayout s = mapM_ label s.packages

renderEditor :: EditorState -> NanoUI ()
renderEditor _ = pure ()

viewChanged :: EditorState -> EditorState -> Bool
viewChanged = (/=)
