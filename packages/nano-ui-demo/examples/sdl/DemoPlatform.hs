-- | What the widget cookbook ("Demo") takes from the SDL backend: installed
-- font families, data files read from disk, native file dialogs, and the
-- backend's frame statistics. The browser build of the cookbook has a
-- module of the same name in @examples/web@ with the same exports.
module DemoPlatform
  ( backendLabel
  , FontList
  , newFontList
  , fontFamilies
  , setUiFontFamily
  , readDataFile
  , FileDialogState
  , newFileDialogState
  , fileDialogSection
  , debugStats
  , diagnosticsRows
  , formatExplainRows
  ) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl
import Paths_nano_ui_demo (getDataFileName)
import Text.Printf (printf)

-- | The backend's name, shown in the cookbook's title bar.
backendLabel :: Text
backendLabel = "SDL3"

-- | The font families installed on this machine, found when the demo
-- starts.
newtype FontList = FontList [Text]

-- | Search the platform's font directories, or Inter alone when none are
-- found.
newFontList :: IO FontList
newFontList = do
  families <- map T.pack <$> listFontFamilies
  pure (FontList (if null families then ["Inter"] else families))

-- | The families found.
fontFamilies :: FontList -> NanoUI [Text]
fontFamilies (FontList families) = pure families


-- | Use an installed family as the UI font.
setUiFontFamily :: Text -> NanoUI ()
setUiFontFamily family = setSdlUiFont (FontSearch [T.unpack family])

-- | A file from the package's @data@ directory, by its path there
-- (@data/lick.gif@).
readDataFile :: FilePath -> IO BS.ByteString
readDataFile path = BS.readFile =<< getDataFileName path

-- | The native file dialog the Controls tab has open, if any.
newtype FileDialogState = FileDialogState (Var (Maybe FileDialogId))

-- | No dialog open.
newFileDialogState :: IO FileDialogState
newFileDialogState = FileDialogState <$> newVar Nothing

-- | A native file dialog runs outside the view: ask for one, keep its
-- handle, and 'useFileDialog' polls it until it answers. The chosen paths
-- go to @opened@.
fileDialogSection :: FileDialogState -> Var Text -> NanoUI ()
fileDialogSection (FileDialogState dialog) opened = do
  useFileDialog dialog (writeVar opened . T.intercalate ", " . map T.pack)
  heading "File Dialogs"
  whenM (button "Open File…") $
    writeVar dialog . Just =<< askOpenFileDialog defaultFileDialogOptions {dialogAllowMany = True}
  paths <- readVar opened
  kv "Opened" (if T.null paths then "-" else paths)

-- | The SDL backend's frame statistics, which it samples four times a
-- second while something asks for them, as the Debug window's sections.
debugStats :: NanoUI ()
debugStats = do
  stats <- askSdlDebug
  scope . for_ stats $ \s ->
    for_ (debugSections s) $ \(title, rows) -> do
      heading title
      mapM_ (uncurry kvMono) rows
      separator

debugSections :: SdlDebugSnapshot -> [(Text, [(Text, Text)])]
debugSections s =
  [ ("Frame", formatFpsRows c)
  , ("Draw", formatDrawRows c)
  , ( "Display"
    , [ ("window", T.pack (printf "%.0f x %.0f" (dbgWinW c) (dbgWinH c)))
      , ("refresh", T.pack (printf "%d Hz" (dbgRefreshHz s)))
      , ("scale", T.pack (printf "%.2f" (dbgScale s)))
      , ("renderer", rendererText s)
      , ("font", T.pack (dbgFontPath s))
      ]
    )
  , ("Runtime", dbgRts c)
  ]
  where
    c = dbgCore s

rendererText :: SdlDebugSnapshot -> Text
rendererText s = dbgRenderer s <> if dbgVsync s then ", vsync on" else ", vsync off"

-- | Headline frame statistics for the Diagnostics tab. Querying stats keeps
-- sampling active only while the tab is open.
diagnosticsRows :: NanoUI ()
diagnosticsRows =
  askSdlDebug >>= \case
    Nothing -> muted "No SDL session."
    Just s -> do
      let c = dbgCore s
      kv "Present FPS" (T.pack (printf "%.1f fps" (dbgPresentFps c)))
      kv "Loop FPS" (T.pack (printf "%.1f fps" (dbgLoopFps c)))
      kv "Display" (T.pack (printf "%d Hz" (dbgRefreshHz s)))
      kv "Frame time" (T.pack (printf "%.2f ms" (dbgFrameMs c)))
      kv "Haskell time" (T.pack (printf "%.2f ms (UI %.2f, render %.2f)" (dbgUiMs c + dbgRenderMs c) (dbgUiMs c) (dbgRenderMs c)))
      kv "SDL present" (T.pack (printf "%.2f ms" (dbgPresentMs c)))
      kv "Draw calls" (T.pack (show (dbgCmds c)))
      kv "Vertices / indices" (T.pack (printf "%d / %d" (dbgVerts c) (dbgIndices c)))
      kv "Renderer" (rendererText s)
