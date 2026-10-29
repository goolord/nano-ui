-- | Code for the book's Desktop Windows chapter
-- (@book/src/advanced/window-management.md@).
module Book.Windows where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Text (Text)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), WindowDecorations (..), defaultSdlOptions, runSdlApp, windowCaption)

-- ANCHOR: settings
main :: IO ()
main = do
  docName <- newVar ("untitled.txt" :: Text)
  runSdlApp opts (editor docName)

opts :: SdlOptions
opts =
  defaultSdlOptions
    { sdlWindowSettings =
        defaultWindowSettings
          { wsTitle       = "Main Window"
          , wsSize        = Size 1280 800
          , wsMinSize     = Just (Size 640 480)
          , wsMode        = Windowed      -- Windowed | Fullscreen | Hidden
          , wsTransparent = False
          , wsResizable   = True
          }
    }

editor :: Var Text -> NanoUI ()
editor docName = column $ do
  textInput docName
  name <- readVar docName
  setWindowTitleUi (name <> " - My App")         -- setter: every frame
  whenM (button "Maximize") toggleMaximizedUi    -- command: on click only
  whenM (button "Quit") quitUi
-- ANCHOR_END: settings

-- ANCHOR: close_request
confirmMain :: IO ()
confirmMain =
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsExitOnCloseRequest = False} }
    appView

appView :: NanoUI ()
appView = do
  confirming <- useVar False
  mainContent
  -- True on one frame only: keep it in a Var so the modal stays open.
  whenM (winCloseRequested <$> askWindow) (writeVar confirming True)
  modal confirming "Quit?" $ do
    label "Unsaved changes will be lost."
    whenM (button "Quit") quitUi
    whenM (button "Cancel") (writeVar confirming False)
-- ANCHOR_END: close_request

-- ANCHOR: title_bar
framelessOpts :: SdlOptions
framelessOpts = defaultSdlOptions
  { sdlWindowDecorations = DecorationsNone
  , sdlWindowSettings = defaultWindowSettings {wsSize = Size 1000 720}
  }

framelessView :: NanoUI ()
framelessView = columnWith (fillW . fillH) $ do
  titleBar
  mainContent

titleBar :: NanoUI ()
titleBar = rowWith (fixedH 32 . fillW . alignMid) $ do
  menuResp <- button' "File"          -- interactive: exclude from dragging
  label "My Application"
  spacer (Grow 1) Fit
  closeClicked <- windowCaption [respRect menuResp]
  when closeClicked quitUi
-- ANCHOR_END: title_bar

-- ANCHOR: window_frame
framedView :: NanoUI ()
framedView = windowFrame (defaultWindowFrame (colorRGBA 70 70 80 255)) $ do
  mainContent
-- ANCHOR_END: window_frame

-- ANCHOR: request_screenshot
screenshotButton :: NanoUI ()
screenshotButton =
  whenM (button "Screenshot") $
    requestScreenshot $ \maybeScreenshot -> for_ maybeScreenshot saveShot
-- ANCHOR_END: request_screenshot

-- ANCHOR: use_screenshot
data App = App
  { appShots :: Task Int (Maybe Screenshot)
  , appCount :: Var Int
  }

screenshotMain :: IO ()
screenshotMain = newApp >>= runSdlApp defaultSdlOptions . view

newApp :: IO App
newApp = App <$> newTask <*> newVar 0

view :: App -> NanoUI ()
view app = do
  whenM (button "Screenshot") (modifyVar (appCount app) (+ 1))
  shotKey <- readVar (appCount app)
  shot <- if shotKey == 0 then pure Nothing else useScreenshot (appShots app) shotKey
  for_ shot showShot
-- ANCHOR_END: use_screenshot

-- ANCHOR: native_window
inspectorMain :: IO ()
inspectorMain = newVar False >>= runSdlApp defaultSdlOptions . editorView

editorView :: Var Bool -> NanoUI ()
editorView showInspector = do
  checkbox "Inspector" showInspector
  nativeWindow showInspector
    defaultWindowSettings {wsTitle = "Inspector", wsSize = Size 360 480}
    inspectorView
-- ANCHOR_END: native_window

-- Stand-ins for application code the snippets refer to.

mainContent, inspectorView :: NanoUI ()
mainContent = pure ()
inspectorView = pure ()

saveShot :: Screenshot -> IO ()
saveShot _ = pure ()

showShot :: Screenshot -> NanoUI ()
showShot _ = pure ()
