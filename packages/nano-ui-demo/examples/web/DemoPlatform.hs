-- | What the widget cookbook ("Demo") takes from the browser backend. The
-- page has no font directories, file system or native dialogs: the font
-- list starts with the bundled Inter and adds the installed families once
-- the user lets the browser list them, the data files are compiled into
-- the module, and the File Dialogs section says what it would do on the
-- desktop. The SDL build has a module of the same name in @examples/sdl@
-- with the same exports.
module DemoPlatform
  ( backendLabel
  , FontList
  , newFontList
  , fontFamilies
  , fontFinder
  , setUiFontFamily
  , readDataFile
  , FileDialogState
  , newFileDialogState
  , fileDialogSection
  , debugStats
  , diagnosticsRows
  , formatExplainRows
  ) where

import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.SdlWeb
  ( CoreDebugSnapshot (..)
  , SdlWebDebugSnapshot (..)
  , WebFont (..)
  , askSdlWebDebug
  , formatDrawRows
  , formatExplainRows
  , formatFpsRows
  , listWebFontFamilies
  , setSdlWebUiFont
  )
import Text.Printf (printf)

import DemoFiles (dataFiles)

-- | The backend's name, shown in the cookbook's title bar.
backendLabel :: Text
backendLabel = "Web"

-- | The font families to choose from: Inter, and the installed families
-- once 'fontFinder' has found them.
data FontList = FontList
  { flTask :: !(Task Int (Either String [String]))
  , flAsked :: !(Var Int)
  -- ^ How many times the button was pressed; each press asks again.
  , flFound :: !(Var (Maybe (Either String [String])))
  -- ^ The last answer, kept while the Controls tab is away.
  }

-- | Nothing found yet.
newFontList :: IO FontList
newFontList = FontList <$> newTask <*> newVar 0 <*> newVar Nothing

-- | Inter, then the installed families found so far.
fontFamilies :: FontList -> NanoUI [Text]
fontFamilies fl = do
  installed <- readVar (flFound fl) >>= \case
    Just (Right families) -> pure (map T.pack families)
    _ -> pure []
  pure ("Inter" : filter (/= "Inter") installed)

-- | A button that asks the browser for the installed families, and what it
-- answered. The browser asks the user first, and only right after a press,
-- so the task starts on the button's frame.
fontFinder :: FontList -> NanoUI ()
fontFinder fl = do
  asked <- readVar (flAsked fl)
  when (asked > 0) $ do
    answer <- useTask (flTask fl) asked listWebFontFamilies
    found <- readVar (flFound fl)
    for_ answer $ \a -> unless (found == Just a) (writeVar (flFound fl) (Just a))
  found <- readVar (flFound fl)
  rowWith (tight . gap 12 . alignMid . fillW) $ do
    whenM (button "Find installed fonts") (modifyVar (flAsked fl) (+ 1))
    muted $ case found of
      _ | asked > 0 && found == Nothing -> "Waiting for the browser..."
      Nothing -> "Inter is bundled."
      Just (Left why) -> T.pack ("None: " <> why)
      Just (Right families) -> T.pack (show (length families) <> " installed families")

-- | Use a family: the bundled Inter, or an installed one, which the session
-- loads through the browser before the next frame.
setUiFontFamily :: Text -> NanoUI ()
setUiFontFamily family
  | family == "Inter" = setSdlWebUiFont WebFontInter
  | otherwise = setSdlWebUiFont (WebFontInstalled (T.unpack family))

-- | A file from the package's @data@ directory, by its path there
-- (@data/lick.gif@). A path that was not embedded throws, as reading a
-- missing file does.
readDataFile :: FilePath -> IO BS.ByteString
readDataFile path =
  maybe (ioError (userError (path <> ": not embedded in the web build"))) pure $
    lookup path [("data/" <> name, bytes) | (name, bytes) <- dataFiles]

-- | The browser has no native file dialogs, so there is no state to keep.
data FileDialogState = FileDialogState

-- | The (empty) state.
newFileDialogState :: IO FileDialogState
newFileDialogState = pure FileDialogState

-- | The section the desktop build fills with an Open File button.
fileDialogSection :: FileDialogState -> Var Text -> NanoUI ()
fileDialogSection FileDialogState _ = do
  heading "File Dialogs"
  muted "Native file dialogs open from the desktop build (nano-ui-sdl). A page has none."

-- | The web backend's frame statistics, which it samples four times a second
-- while something asks for them, as the Debug window's sections.
debugStats :: NanoUI ()
debugStats = do
  stats <- askSdlWebDebug
  scope . for_ stats $ \s ->
    for_ (debugSections s) $ \(title, rows) -> do
      heading title
      mapM_ (uncurry kvMono) rows
      separator

debugSections :: SdlWebDebugSnapshot -> [(Text, [(Text, Text)])]
debugSections s =
  [ ("Frame", formatFpsRows c)
  , ("Draw", formatDrawRows c)
  , ( "Display"
    , [ ("canvas", T.pack (printf "%.0f x %.0f" (dbgWinW c) (dbgWinH c)))
      , ("pixel ratio", T.pack (printf "%.2f" (dbgPixelRatio s)))
      , ("scale", T.pack (printf "%.2f" (dbgScale s)))
      , ("pacing", T.pack (printf "%d Hz" (dbgRefreshHz s)))
      , ("renderer", dbgRenderer s)
      , ("font", T.pack (dbgFont s))
      ]
    )
  , ("Runtime", dbgRts c)
  ]
  where
    c = dbgCore s

-- | The headline frame statistics for the Diagnostics tab. Asking for them
-- keeps the backend sampling, so only while the tab shows.
diagnosticsRows :: NanoUI ()
diagnosticsRows =
  askSdlWebDebug >>= \case
    Nothing -> muted "No browser session."
    Just s -> do
      let c = dbgCore s
      kv "Present FPS" (T.pack (printf "%.1f fps" (dbgPresentFps c)))
      kv "Loop FPS" (T.pack (printf "%.1f fps" (dbgLoopFps c)))
      kv "Pacing" (T.pack (printf "%d Hz" (dbgRefreshHz s)))
      kv "Frame time" (T.pack (printf "%.2f ms" (dbgFrameMs c)))
      -- WebGL calls return before the GPU runs them, so the render time is
      -- the whole of the frame's cost here; the browser composites later.
      kv "Haskell time" (T.pack (printf "%.2f ms (UI %.2f, render %.2f)" (dbgUiMs c + dbgRenderMs c) (dbgUiMs c) (dbgRenderMs c)))
      kv "Draw calls" (T.pack (show (dbgCmds c)))
      kv "Vertices / indices" (T.pack (printf "%d / %d" (dbgVerts c) (dbgIndices c)))
      kv "Renderer" (dbgRenderer s)
      kv "Pixel ratio" (T.pack (printf "%.2f" (dbgPixelRatio s)))
