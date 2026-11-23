-- | What the widget cookbook ("Demo") takes from the browser backend. The
-- page has no font directories or file system: the font list is the bundled
-- Inter, the installed families once the user lets the browser list them,
-- and the font service's; the data files are compiled into the module; and
-- the File Dialogs section opens files and folders by their contents and
-- saves through the browser. The SDL build has a module of the same name in @examples/sdl@
-- with the same exports.
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

import Control.Exception (displayException)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.SdlWeb
  ( CoreDebugSnapshot (..)
  , SdlWebDebugSnapshot (..)
  , WebFile (..)
  , WebFont (..)
  , askSdlWebDebug
  , defaultFileDialogOptions
  , FileDialogOptions (..)
  , openWebFiles
  , openWebFolder
  , saveWebFile
  , formatDrawRows
  , formatExplainRows
  , formatFpsRows
  , setSdlWebUiFont
  , webFontFamilies
  )
import Text.Printf (printf)

import DemoFiles (dataFiles)

-- | The backend's name, shown in the cookbook's title bar.
backendLabel :: Text
backendLabel = "Web"

-- | Nothing to keep: the session finds the families.
data FontList = FontList

-- | The (empty) list state.
newFontList :: IO FontList
newFontList = pure FontList

-- | Inter, then the families the browser and the font service offer
-- ('webFontFamilies'). The installed ones arrive after the user's next
-- press, when the browser asks them, and in Chromium-based browsers only.
fontFamilies :: FontList -> NanoUI [Text]
fontFamilies FontList = ("Inter" :) . filter (/= "Inter") . map T.pack <$> webFontFamilies

-- | Use a family: the bundled Inter, or one found by name, installed or
-- from the font service, which the session loads before the next frame.
setUiFontFamily :: Text -> NanoUI ()
setUiFontFamily family
  | family == "Inter" = setSdlWebUiFont WebFontInter
  | otherwise = setSdlWebUiFont (WebFontSearch [T.unpack family])

-- | A file from the package's @data@ directory, by its path there
-- (@data/lick.gif@). A path that was not embedded throws, as reading a
-- missing file does.
readDataFile :: FilePath -> IO BS.ByteString
readDataFile path =
  maybe (ioError (userError (path <> ": not embedded in the web build"))) pure $
    lookup path [("data/" <> name, bytes) | (name, bytes) <- dataFiles]

-- | What a dialog button asks the browser for.
data DialogAction = OpenFiles | OpenFolder | SaveFile
  deriving (Eq, Show)

-- | How a dialog ended.
data DialogOutcome = Opened [WebFile] | Saved String | Cancelled

-- | The dialog the section has open: each press numbers a request, the
-- task runs the latest, and the view takes each request's answer once.
data FileDialogState = FileDialogState
  { fdTask :: !(Task (Int, DialogAction) DialogOutcome)
  , fdRequest :: !(Var (Int, DialogAction))
  , fdTaken :: !(Var Int)
  , fdStatus :: !(Var Text)
  }

-- | No dialog asked for yet.
newFileDialogState :: IO FileDialogState
newFileDialogState = FileDialogState <$> newTask <*> newVar (0, OpenFiles) <*> newVar 0 <*> newVar ""

-- | Open files or a folder, which the browser hands over by name and
-- contents, or save a file through the browser. The browser shows its
-- dialog only right after a press, so each runs on a task the press starts.
-- The names opened go to @opened@.
fileDialogSection :: FileDialogState -> Var Text -> NanoUI ()
fileDialogSection st opened = do
  request@(n, _) <- readVar (fdRequest st)
  when (n > 0) $ do
    -- The status, not the latest result: while a new request runs, the
    -- latest result is still the previous request's.
    status <- useTaskStatus (fdTask st) request (runDialog request)
    taken <- readVar (fdTaken st)
    let take' act = when (taken /= n) (writeVar (fdTaken st) n >> act)
    case status of
      TaskRunning _ -> pure ()
      TaskFailed err _ -> take' (writeVar (fdStatus st) ("Failed: " <> T.pack (displayException err)))
      TaskDone outcome -> take' $ case outcome of
        Opened files -> do
          writeVar opened (T.intercalate ", " (map describe files))
          writeVar (fdStatus st) (T.pack (show (length files) <> " opened"))
        Saved name -> writeVar (fdStatus st) ("Saved as " <> T.pack name)
        Cancelled -> writeVar (fdStatus st) "Cancelled"
  heading "File Dialogs"
  muted "A page sees no paths: the browser hands over the files you pick, and saves where you say."
  rowWith (tight . gap 12 . alignMid . fillW) $ do
    let ask action = modifyVar (fdRequest st) (\(k, _) -> (k + 1, action))
    whenM (button "Open File…") (ask OpenFiles)
    whenM (button "Open Folder…") (ask OpenFolder)
    whenM (button "Save…") (ask SaveFile)
    status <- readVar (fdStatus st)
    unless (T.null status) (muted status)
  names <- readVar opened
  kv "Opened" (if T.null names then "-" else names)
  where
    describe f = T.pack (webFileName f) <> " (" <> T.pack (show (BS.length (webFileBytes f))) <> " bytes)"
    runDialog (_, action) = case action of
      OpenFiles -> maybe Cancelled Opened <$> openWebFiles defaultFileDialogOptions {dialogAllowMany = True}
      OpenFolder -> maybe Cancelled Opened <$> openWebFolder
      SaveFile -> maybe Cancelled Saved <$> saveWebFile defaultFileDialogOptions "nano-ui-cookbook.txt" saved
    saved = "Saved from the nano-ui widget cookbook in the browser.\n"

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
