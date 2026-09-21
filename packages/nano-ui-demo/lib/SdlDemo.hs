-- | nano-ui widget cookbook.
--
-- The whole showcase is one function, 'demoUi'. Every tab is a different
-- widget family; jump to the family you care about.
--
-- Every interactive widget follows the same immediate-mode shape:
--
-- @
--   newVal <- widget label currentVal           -- draw the current value
--   setX newVal                                 -- store what the user changed
--   (resp, newVal) <- widget' label currentVal  -- primed: also the Response
-- @
--
-- Inputs are controlled: the widget's value outlives the frame only because
-- you persist it and pass it back. State lives in hooks created by the
-- @use*@ functions. Hooks must be called in the same order every frame, so
-- all of them sit together at the top of 'demoUi', even for tabs that are
-- currently hidden.
--
-- Style is expressed as layout-style functions threaded through the container
-- widget: columnWith (padAll 6 . gap 8 . fillW) $...  Text widgets compose
-- font styles the same way (fontBold, fontSize n, fontMuted, ...).
--
-- Families, by tab:
--
--   * Controls:     button, checkbox, slider, select, comboBox,
--                   boundedRadio, colorPicker, textInput, textArea,
--                   numericInput,
--                   button + tooltip, contextMenu, file dialogs, dropZone
--   * Graphics:     image gallery, an animated GIF, and a progressBar driven
--                   by a pulsing value
--   * Typography:   label / labelWith + the @font*@ style combinators
--   * List:         tree, searchInput
--   * Table:        tableWith (needs useTableSort)
--   * Panes:        paneGrid
--   * Plots:        plot, barChart, areaChart, diagram
--   * Diagnostics:  debug readouts from the SDL backend
--
-- The entry point is 'main' (§1) with a small CLI; the argument plumbing is the
-- last section of this file. The automated UI test lives in its own module,
-- "SdlSelftest".

module SdlDemo
    ( main
    , demoUi
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, toList)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Word (Word64)
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Internal.Debug (CoreDebugSnapshot (..), formatCoreRtsRows)
import NanoUI.Diagrams
import NanoUI.Internal.Monad (askContext)
import NanoUI.Internal.Context (askHostIO, setHost)
import Paths_nano_ui_demo (getDataFileName)
import Diagrams.Prelude
  ( Diagram
  , circle
  , fc
  , fromVertices
  , lc
  , lw
  , lwO
  , none
  , p2
  , ( # )
  )
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg)
  , ArgOrder (Permute)
  , OptDescr (Option)
  , getOpt
  , usageInfo
  )
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)
import Text.Printf (printf)
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import qualified Data.Vector.Storable as VS
import qualified SdlRecord
import qualified SdlSelftest

import DemoApp (useFileDialog)
import DemoData
  ( DemoPerson (..)
  , colPeople
  , demoPeople
  , demoSwatches
  , demoTree
  , sineCosineChart
  , weeklyBars
  )

------------------------------------------------------------------------------
-- §1  App entry (main)
------------------------------------------------------------------------------

-- | Run @cabal run nano-ui-sdl-demo@ for the windowed app, or
-- @cabal run nano-ui-sdl-demo -- --selftest@ for the headless UI test
-- (defined in "SdlSelftest"). @--record DIR@ records the README video's
-- frames (see "SdlRecord").
main :: IO ()
main = do
  args <- getArgs
  case dropWhile (/= "--record") args of
    _ : dir : _ -> SdlRecord.record dir demoUi
    _ | "--selftest" `elem` args -> SdlSelftest.selftest ("--continuous" `elem` args) demoUi
    _ -> do
      let (cfgUpdates, _, _) = getOpt Permute options args
          cfg = foldl' (flip id) defaultDemoConfig cfgUpdates
      if cfgHelp cfg
        then putStr (usageInfo "Usage: nano-ui-sdl-demo [OPTIONS]" options)
        else do
          runSdlApp
            defaultSdlOptions
              { sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
              , sdlAppTheme = Just defaultTheme
              , sdlAppVsync = cfgVsync cfg
              , sdlAppContinuous = cfgContinuous cfg
              , sdlWindowFullscreen = cfgFullscreen cfg
              , sdlWindowDecorations = if cfgBorderless cfg then DecorationsFrame else DecorationsFull
              , sdlWindowAlwaysOnTop = cfgAlwaysOnTop cfg
              , sdlWindowSize = Size (fromMaybe 1280 (cfgWidth cfg)) (fromMaybe 800 (cfgHeight cfg))
              }
            demoUi

------------------------------------------------------------------------------
-- §2  Assets & shared look
------------------------------------------------------------------------------

-- | An animated GIF loading in the background: each frame's width, height and
-- RGBA pixels once decoded, or why the file could not be used.
newtype GifLoad = GifLoad (MVar (Either String [(Int, Int, BS.ByteString)]))
  deriving (Eq)

-- | Start loading an animated GIF while the app runs. The file is read and
-- decoded with JuicyPixels on a background thread, so the frame that starts
-- the load does not stall. Collect the frames with 'gifFrames'.
loadGif :: FilePath -> NanoUI GifLoad
loadGif path =
  liftIO $ do
    done <- newEmptyMVar
    _ <- forkIO $ do
      decoded <- try $ do
        bytes <- BS.readFile path
        frames <- either fail pure (JP.decodeGifImages bytes)
        when (null frames) (fail "the file has no frames")
        forM frames $ \frame -> do
          let rgba = JP.convertRGBA8 frame
              (fp, n) = VS.unsafeToForeignPtr0 (JP.imageData rgba)
          -- Decode and convert here, so registering the frames only copies.
          pixels <- evaluate (BSI.fromForeignPtr0 fp n)
          pure (JP.imageWidth rgba, JP.imageHeight rgba, pixels)
      putMVar done (either (\e -> Left (displayException (e :: SomeException))) Right decoded)
    pure (GifLoad done)

-- | Register a finished load's frames under fresh ids, in order. 'Nothing'
-- while the file is still decoding, then the frames' ids or why the file could
-- not be used. The result comes once; keep it. Registering stops at the first
-- frame the atlas refuses.
gifFrames :: GifLoad -> NanoUI (Maybe (Either String (SmallArray ImageId)))
gifFrames (GifLoad done) =
  liftIO (tryTakeMVar done) >>= traverse (either (pure . Left) (register []))
  where
    register ids [] = pure (Right (smallArrayFromList (reverse ids)))
    register ids ((w, h, pixels) : rest) = do
      iid <- freshImageId
      ok <- registerImageRgba iid w h pixels
      if ok then register (iid : ids) rest else pure (Left "the image atlas is full")

-- | Demo accent used across the state readout and pane headers.
demoAccent :: Color
demoAccent = colorRGBA 204 102 102 255

-- Spacing rhythm for the demo cards and columns.
gapLayout, gapInline, gapMicro, gapText :: Float
gapLayout = 10
gapInline = 12
gapMicro = 6
gapText = 4

-- | The tabbed card in the right column; each tab is a widget family.
data DemoTab
  = Controls
  | Graphics
  | Typography
  | List
  | Table
  | Panes
  | Plots
  | Diagnostics
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Theme choices for the Controls-tab theme pickers.
data DemoTheme
  = ThemeDefault
  | TomorrowNightMin
  | TomorrowLight
  | TomorrowMidnightMin
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

themeDisplayName :: DemoTheme -> T.Text
themeDisplayName ThemeDefault = "Default"
themeDisplayName TomorrowNightMin = "Tomorrow Night Min"
themeDisplayName TomorrowLight = "Tomorrow Light"
themeDisplayName TomorrowMidnightMin = "Tomorrow at Midnight Min"

themeForChoice :: DemoTheme -> Theme
themeForChoice ThemeDefault = defaultTheme
themeForChoice TomorrowNightMin = tomorrowNightMinDarkTheme
themeForChoice TomorrowLight = tomorrowMinLightTheme
themeForChoice TomorrowMidnightMin = tomorrowMidnightMinDarkTheme

-- | Font families offered by the Controls-tab font combo box, straight from
-- the SDL backend's system font-directory scan ('listFontFamilies'; cached
-- once per context). The chosen family is pushed to the backend through
-- 'setSdlUiFont' and re-rendered on the next frame. The fallback list only
-- kicks in on systems where the scan finds no font files.
data DemoSettings = DemoSettings ![T.Text] !Bool

demoSettings :: NanoUI DemoSettings
demoSettings = do
  ctx <- askContext
  liftIO $ do
    cached <- askHostIO ctx
    case cached of
      Just settings -> pure settings
      Nothing -> do
        families <- map T.pack <$> listFontFamilies
        debugOpen <- isJust <$> lookupEnv "NANO_DEBUG_OPEN"
        let settings = DemoSettings
              (if null families
                then ["Inter", "Noto Sans", "Adwaita Sans", "Cantarell", "Liberation Sans", "FreeSans"]
                else families)
              debugOpen
        setHost ctx settings
        pure settings

------------------------------------------------------------------------------
-- §3  The showcase UI
------------------------------------------------------------------------------

-- | The whole app. Composition, top to bottom:
--   1. state hooks          every frame, fixed order (see module header)
--   2. toolbar              brand, live FPS, OK / Cancel / About / Debug
--   3. two-column body      left: live state; right: tabbed demos
--   4. overlays             Debug window + About modal
demoUi :: NanoUI ()
demoUi = do
  DemoSettings demoFontFamilies debugOpenFromEnv <- demoSettings
  ---------------------------------------------------------------- hooks ---
  -- Toolbar / overlays.
  (click, setClick) <- useText "" -- label of the last button / menu item clicked
  (aboutOpen, setAbout) <- useFlag False
  (debugOpen, setDebug) <- useFlag debugOpenFromEnv
  (inspectorOpen, setInspectorOpen) <- useFlag False -- compact-window readout
  (activeTab, setActiveTab) <- useEnum Controls -- tabs
  -- Controls tab.
  (checked, setChecked) <- useFlag False -- checkbox
  (vol, setVol) <- useFloat 50 -- slider
  (quality, setQuality) <- useText "Medium" -- select
  (accent, setAccent) <- useState demoAccent -- colorPickerRGBA
  (themeChoice, setThemeChoice) <- useEnum ThemeDefault -- boundedRadio
  (fontChoice, setFontChoice) <- useText "Inter" -- comboBox
  (name, setName) <- useText "" -- textInput
  (notes, setNotes) <- useText "Edit me.\nSecond line." -- textArea
  (count, setCount) <- useState (12 :: Double) -- numericInput
  (mask, setMask) <- useState (0xC0FF :: Double) -- hexadecimal numericInput
  (dropLog, setDropLog) <- useText "" -- dropZone result, multi-line
  (dropHovering, setDropHovering) <- useFlag False -- drag-over state
  -- File dialog handles; results land in the paths below via useFileDialog.
  (openDlg, setOpenDlg) <- useState (Nothing :: Maybe FileDialogId)
  (saveDlg, setSaveDlg) <- useState (Nothing :: Maybe FileDialogId)
  (lick, setLick) <- useState (Nothing :: Maybe (Either String (SmallArray ImageId))) -- GIF frames, once loaded
  (lickLoad, setLickLoad) <- useState (Nothing :: Maybe GifLoad) -- the GIF while it decodes
  (icons, setIcons) <- useState (Nothing :: Maybe [Either String Svg]) -- SVG icons, read on first show
  (swatches, setSwatches) <- useState (Nothing :: Maybe [(ImageId, T.Text)]) -- generated images, registered on first show
  (folderDlg, setFolderDlg) <- useState (Nothing :: Maybe FileDialogId)
  (openPath, setOpenPath) <- useText ""
  (savePath, setSavePath) <- useText ""
  (folderPath, setFolderPath) <- useText ""
  useFileDialog openDlg setOpenDlg $ \paths ->
    setOpenPath (T.intercalate ", " (map T.pack paths))
  useFileDialog saveDlg setSaveDlg $ \paths ->
    setSavePath (maybe "" T.pack (listToMaybe paths))
  useFileDialog folderDlg setFolderDlg $ \paths ->
    setFolderPath (maybe "" T.pack (listToMaybe paths))
  -- List tab.
  (searchText, setSearchText) <- useText "" -- live searchInput text
  (searchQuery, setSearchQuery) <- useText "" -- committed searchInput value
  (peopleMatches, setPeopleMatches) <- useState demoPeople -- filtered rows
  (treeSel, setTreeSel) <- useInt 0 -- tree selection index
  -- Table tab.
  (tableSortVal, setTableSort) <- useTableSort (SortCol 0 SortAsc)
  -- Panes tab.
  (showPaneHeaders, setShowPaneHeaders) <- useFlag True -- pane headers on/off
  -- Typography tab.
  (sampleText, setSampleText) <- useText "The quick brown fox jumps over the lazy dog"
  (typeSize, setTypeSize) <- useFloat 20.0
  (typeBold, setTypeBold) <- useFlag False
  (typeItalic, setTypeItalic) <- useFlag True
  (typeUnderline, setTypeUnderline) <- useFlag False
  (typeStrike, setTypeStrike) <- useFlag False
  -- Diagnostics tab: last raw drop event (files/text/paths).
  (dropRaw, setDropRaw) <- useText ""
  rawInp <- askInput
  let wideWorkspace = sizeW (inputWindowSize rawInp) >= 1000
      inspectorWidth = if wideWorkspace then fixedW 280 else fillW
      volText = T.pack (show (round vol :: Int))
  let rawDrop = T.intercalate " | " [T.pack (show (dropEventType ev)) <> " " <> dropEventData ev | ev <- toList (inputDrops rawInp)]
  when (not (T.null rawDrop)) (setDropRaw rawDrop)

  -------------------------------------------------------------- toolbar ---
  -- The page padding matches the gap between cards. The page scrollbar sits
  -- just inside the window's right edge, that same gap from the cards.
  scrollWith (padAll gapLayout . grow) $
    columnWith (tight . gap gapLayout . fillW) $ do
      panelWith (padXY 16 12 . gap gapInline . fillW) $
        responsiveRowCol 960 (tight . gap gapInline . alignMid . fillW) $ do
          rowWith (tight . gap gapInline . alignMid) $ do
            labelWith (tight . alignMid . fontMedium . fontSize 22) "nano-ui"
            labelWith (tight . alignMid . fontMuted) "SDL3 / Widget cookbook"
          when (sizeW (inputWindowSize rawInp) >= 960) flex
          -- Live frame stats + the shared header buttons.
          fpsText <- dtFps <$> (debugText =<< askSdlDebug)
          unless (T.null fpsText) $
            labelWith (tight . alignMid . fontMono . fontMuted) fpsText
          rowWith (tight . gap gapMicro . alignMid) $ do
            whenM (button "OK") (setClick "OK")
            whenM (button "Cancel") (setClick "Cancel")
            whenM (button "About") (setAbout True)
            whenM (button "Debug") (setDebug (not debugOpen))

      ----------------------------------------------------------- body ----
      responsiveRowCol 1000 (tight . gap gapLayout . fillW) $ do
        -- Left: a live readout of every hook value above. Tweak a widget on the
        -- right and watch its line update: instant confirmation the write-back
        -- idiom worked.
        columnWith (tight . gap gapLayout . inspectorWidth) $ do
          panelWith (padAll 16 . gap 8 . fillW) $ do
            rowWith (tight . gap gapInline . alignMid . fillW) $ do
              labelWith (tight . alignMid . fontMedium) "State"
              flex
              unless wideWorkspace $
                whenM (button (if inspectorOpen then "Hide values" else "Show values")) $
                  setInspectorOpen (not inspectorOpen)
            when (wideWorkspace || inspectorOpen) $ do
              muted "Live widget values"
              separator
              kv "Feature" (if checked then "on" else "off")
              kv "Volume" volText
              kv "Quality" quality
              -- The swatch sits beside the value, so the key lines up with
              -- the rows above and below.
              rowWith (tight . gap gapMicro . alignMid . fillW) $ do
                labelWith (tight . alignMid . fontMuted . minW 88) "Accent"
                flex
                box (alignMid . fixedWH 14 14) accent
                labelWith (tight . alignMid) (colorToHexA accent)
              separator
              kv "Theme" (themeDisplayName themeChoice)
              kv "Font" fontChoice
              kv "Name" (orDash name)
              kv "Notes" (orDash notes)
              kv "Count" (T.pack (show (round count :: Int)))
              kv "Mask" (T.pack (printf "0x%04X" (round mask :: Int)))
              separator
              kv "Tree" (T.pack (show treeSel))
              kv "Table sort" (tableColumnLabel tableSortVal)
              kv "Table order" (tableSortDirText tableSortVal)
              kv "Clicked" (orDash click)
              separator
              kv "Open file" (orDash openPath)
              kv "Save file" (orDash savePath)
              kv "Folder" (orDash folderPath)
              kv "Dropped" (orDash (T.take 80 (fromMaybe "" (listToMaybe (T.lines dropLog)))))
              separator
              muted "Edit a control to see its value here."
              muted "Esc closes About, then quits."

        -- Right: the tabbed widget demos. Each tab body below is one widget
        -- family; its state hooks all live at the top of demoUi.
        panelWith (padAll 16 . gap 12 . fillW) $ do
          newTab <- tabs activeTab $ flip map [minBound .. maxBound] $ \page ->
            tab page (T.pack (show page)) $ case page of
            ----------------------------------------------- Controls ---------
            -- Form widgets. The returned value is stored back through the hook;
            -- the State card on the left then shows it.
            Controls -> do
              responsiveRowCol 760 (tight . gap 24 . fillW) $ do
                columnWith (tight . gap 10 . fillW) $ do
                  heading "Controls"
                  setChecked =<< checkbox "Feature" checked
                  columnWith (tight . gap 4 . fillW) $ do
                    kv "Volume" volText
                    setVol =<< slider 0 100 vol
                  let qualities = ["Low", "Medium", "High"]
                  qualityIdx <- demoField "Quality" $
                    selectWith fillW qualities (fromMaybe 1 (elemIndex quality qualities))
                  setQuality (qualities !! qualityIdx)
                  separator
                  heading "Text input"
                  nVal <- demoField "Name" $
                    textInputConfigured defaultTextInputConfig {ticPlaceholder = "Enter name"} name
                  setName nVal
                  notesVal <- demoField "Notes" $
                    textArea notes
                  setNotes notesVal
                  separator
                  heading "Numbers"
                  -- Arrow keys or the stepper step the value; Shift steps by ten.
                  rowWith (tight . gap gapInline . fillW) $ do
                    countVal <- demoField "Count (0-100)" $
                      numericInputConfigured defaultNumericInputConfig {nicMin = 0, nicMax = 100} count
                    setCount countVal
                    maskVal <- demoField "Mask (hex)" $
                      numericInputConfigured defaultNumericInputConfig {nicMin = 0, nicMax = 0xFFFF, nicHex = True} mask
                    setMask maskVal
                columnWith (tight . gap 10 . fillW) $ do
                  heading "Appearance"
                  tVal <- demoField "Theme" $
                    boundedRadio themeDisplayName themeChoice
                  setThemeChoice tVal
                  setUiTheme (themeForChoice tVal)
                  (fResp, fVal) <- demoField "Font" $
                    comboBox' "Font" demoFontFamilies fontChoice
                  tooltip fResp "Type to filter; Enter applies, Esc reverts."
                  setFontChoice fVal
                  when (respChanged fResp && not (T.null fVal)) $
                    setSdlUiFont (FontSearch [T.unpack fVal])
                  separator
                  heading "Accent"
                  muted "Choose a color or enter an exact value."
                  setAccent =<< colorPickerRGBA accent
              separator
              -- Popups & menus: act on respClicked of the item you want.
              heading "Popups & Menus"
              rowWith (tight . gap gapInline . fillW) $ do
                btnTip <- button' "Hover for Tooltip"
                tooltip btnTip "This is a floating tooltip widget!"
                btnMenu <- button' "Right-click Menu"
                void $ contextMenu btnMenu $ do
                  menuHeader "Context Menu"
                  menuSeparator
                  whenM (menuItemShortcut "Cut" "Ctrl+X") (setClick "Cut")
                  whenM (menuItemShortcut "Copy" "Ctrl+C") (setClick "Copy")
                  whenM (menuItemShortcut "Paste" "Ctrl+V") (setClick "Paste")
                  menuSeparator
                  menuItemDisabled "Disabled Option"
              separator
              -- File dialogs: ask for a modal dialog handle, store it, and poll
              -- it every frame via useFileDialog.
              heading "File Dialogs"
              rowWith (tight . gap gapInline . fillW) $ do
                whenM (button "Open File…") $ do
                  mdid <- askOpenFileDialog defaultFileDialogOptions { dialogAllowMany = True }
                  setOpenDlg mdid
                whenM (button "Save File…") $ do
                  mdid <- askSaveFileDialog defaultFileDialogOptions
                  setSaveDlg mdid
                whenM (button "Browse Folder…") $ do
                  mdid <- askOpenFolderDialog defaultFileDialogOptions
                  setFolderDlg mdid
              separator
              -- Drag & drop: dropZone returns a target; dropReceived reports its
              -- files and texts. dropHovering mirrors the hover state for styling.
              heading "Drag & Drop"
              muted "Drag a file or highlighted text from another app onto the zone."
              (_, _, dropTgt) <-
                dropZone (padXY 16 12 . gap gapText . fillW) $ do
                  columnWith (tight . gap gapText . fillW) $ do
                    rowWith (tight . gap gapInline . alignMid . fillW) $ do
                      labelWith (tight . alignMid . fontMedium) "Drop Zone"
                      flex
                      labelWith (tight . alignMid . fontMono . fontMuted) (if dropHovering then "hovering" else "idle")
                    labelWith (tight . fontMuted . fillW) $
                      if dropHovering
                        then "Release to accept dropped files or text."
                        else "Files land here; text lands here too."
                    when (not (T.null dropLog)) $ do
                      separator
                      labelWith (tight . fontMono . fillW) dropLog
              when (dropReceived dropTgt) $ do
                let droppedLines =
                      [ "file:  " <> (if T.length f <= 60 then f else "…" <> T.takeEnd 59 f) | f <- dropFiles dropTgt ]
                        ++ [ "text:  " <> (if T.length t <= 60 then t else T.take 59 t <> "…") | t <- dropTexts dropTgt ]
                setDropLog (if null droppedLines then dropLog else T.intercalate "\n" droppedLines)
              when (dropHovered dropTgt && not dropHovering) (setDropHovering True)
              when (not (dropHovered dropTgt) && dropHovering) (setDropHovering False)

            ----------------------------------------------- Graphics ---------
            Graphics -> do
              heading "Graphics"
              separator
              -- Generated RGBA images, registered under fresh ids the first
              -- time this tab shows.
              case swatches of
                Nothing -> do
                  registered <- forM demoSwatches $ \(caption, pixels) -> do
                    iid <- freshImageId
                    ok <- registerImageRgba iid 32 32 pixels
                    pure [(iid, caption) | ok]
                  setSwatches (Just (concat registered))
                Just registered ->
                  rowWith (tight . gap gapInline . fillW) $
                    for_ registered $ \(iid, caption) ->
                      columnWith (tight . gap gapMicro) $ do
                        image (fixedWH 88 88) iid
                        muted caption
              separator
              -- SVG icons read from disk the first time this tab shows. A
              -- one-colour icon takes the text colour (or a fontColor), and
              -- each size rasterizes once.
              case icons of
                Nothing -> do
                  paths <- liftIO (mapM (\icon -> getDataFileName ("data/icons/" <> icon <> ".svg")) ["clock", "check", "star", "face"])
                  setIcons . Just =<< liftIO (mapM loadSvg paths)
                Just loaded -> do
                  tint <- themeAccent <$> uiTheme
                  rowWith (tight . gap gapInline . alignMid) $
                    forM_ loaded $ \case
                      Left err -> muted (T.pack err)
                      Right doc -> do
                        svgIcon 20 doc
                        svgIconWith (fixedWH 32 32 . fontColor tint) doc
              separator
              -- An animated GIF loaded from disk the first time this tab
              -- shows: loadGif decodes it in the background, and gifFrames
              -- registers its frames once that is done. Each frame is its own
              -- image, and the clock picks which one to show; every frame of
              -- this GIF lasts 100 ms. keepAnimating keeps frames coming while
              -- it loads and plays.
              case lick of
                Nothing -> do
                  keepAnimating =<< labelWith' (fillW . fontMuted) "Loading lick.gif..."
                  case lickLoad of
                    Nothing -> do
                      path <- liftIO (getDataFileName "data/lick.gif")
                      setLickLoad . Just =<< loadGif path
                    Just pending -> mapM_ (setLick . Just) =<< gifFrames pending
                Just (Left err) -> muted ("Could not load lick.gif: " <> T.pack err)
                Just (Right frames) -> do
                  t <- uiTime
                  columnWith (tight . gap gapMicro) $ do
                    keepAnimating =<< image' (fixedWH 150 150) (indexSmallArray frames (floor (t * 10) `mod` sizeofSmallArray frames))
                    muted "lick.gif"
              separator
              -- A plain response-driven bar. pulse provides a smooth
              -- clock-driven 0-1 sweep and keepAnimating holds it live.
              muted "A single rounded bar, smoothly oscillating 0-100%."
              progResp <- progressBar' =<< pulse 6
              keepAnimating progResp
              rowWith (tight . gap gapInline . alignMid) $ do
                spinner
                muted "Loading"

            --------------------------------------------- Typography ---------
            Typography -> do
              heading "Typography & Font Styling"
              muted "Font sizing, variable weights, synthetic slant, and text decorations."
              separator
              -- Live playground: type in the box, flip toggles, drag the size
              -- slider and watch the composed font style update the preview.
              heading "Live Playground"
              columnWith (tight . gap gapInline . fillW) $ do
                muted "Preview text"
                setSampleText =<< textInput sampleText
              rowWith (tight . gap gapInline . fillW . alignMid) $ do
                setTypeBold =<< checkbox "Bold" typeBold
                setTypeItalic =<< checkbox "Italic" typeItalic
                setTypeUnderline =<< checkbox "Underline" typeUnderline
                setTypeStrike =<< checkbox "Strike" typeStrike
              columnWith (tight . gap gapText . fillW) $ do
                kv "Size" (T.pack (printf "%.0f px" typeSize))
                setTypeSize =<< slider 12 40 typeSize
              -- Font styles are ordinary style combinators; fold the toggles in.
              let applyWeight = if typeBold then fontBold else id
                  applyItalic = if typeItalic then fontItalic else id
                  applyDeco
                    | typeUnderline && typeStrike = fontUnderline . fontStrike
                    | typeUnderline = fontUnderline
                    | typeStrike = fontStrike
                    | otherwise = id
                  customStyle = fontSize typeSize . applyWeight . applyItalic . applyDeco . fillW
                  previewTxt = if T.null sampleText then "Type specimen preview..." else sampleText
              panelWith (padAll 10 . fillW) $
                labelWith customStyle previewTxt
              separator
              heading "Type Scale"
              typeScale
              separator
              heading "Weights & Styles"
              weightsStyles
              separator
              heading "Color & Highlights"
              colorHighlights
              separator
              heading "Rich Text"
              richTextSample

            ----------------------------------------------------- List ---------
            List -> do
              heading "Tree"
              -- tree: pass a selection index, get the clicked one back.
              scroll2DWith (fixedH 300 . fillW) $
                setTreeSel =<< tree "demo" demoTree treeSel
              separator
              heading "Searchable list"
              muted "Type to filter. The debounced search commits on a pause; the filtered list is cached and only recomputed when the committed query changes."
              (qResp, qVal) <- searchInput' "Filter people (name, role, city…)" searchText
              setSearchText qVal
              when (respChanged qResp) $ do
                setSearchQuery qVal
                setPeopleMatches (peopleMatching qVal)
              rowWith (tight . gap gapInline . fillW . alignMid) $ do
                muted ("Committed: " <> (if T.null searchQuery then "(none)" else searchQuery))
                flex
                muted
                  ( "Matches: "
                      <> T.pack (show (length peopleMatches))
                  )
              scroll2DWith (padAll 6 . fixedH 168 . fillW) $
                columnWith (tight . gap gapMicro . fillW) $
                  if null peopleMatches
                    then void (muted "No matches.")
                    else
                      for_ peopleMatches $ \p ->
                        labelWith (tight . fillW) (personRowLabel p)

            ---------------------------------------------------- Table ---------
            Table -> do
              heading "Table"
              muted "Click a header to sort. Drag a header to reorder."
              muted "Drag a header edge to resize. Right-click a header to hide."
              -- tableWith re-renders every frame; keep the sort state in a hook
              -- (useTableSort) and mirror changes back into it.
              tableResp <-
                tableWith
                  (fixedH 280)
                  "people"
                  colPeople
                  demoPeople
                  tableSortVal
              let nextSort = tableSort tableResp
              when (respChanged tableResp) (setTableSort nextSort)
              separator
              kv "Sorted by" (tableColumnLabel nextSort)
              kv "Order" (tableSortDirText nextSort)
              kv "Hidden" (tableHiddenLabel (tableHiddenIndices tableResp))

            ---------------------------------------------------- Panes ---------
            Panes -> do
              heading "Pane Grid"
              muted "Drag a divider to resize. Drop a pane on another pane's center to swap them, on its edge to split it, or on the grid's outer edge to restructure the grid."
              muted "+ splits vertically, = splits horizontally, x closes, M maximizes, R restores. Arrow keys move between panes while the grid is focused."
              headersOn <- checkbox "Pane headers" showPaneHeaders
              setShowPaneHeaders headersOn
              pgr <- paneGrid (demoPaneGridCfg headersOn)
              separator
              kv "Panes" (T.pack (show (pgrPaneCount pgr)))
              kv "Focused" (T.pack (show (pgrFocusedPane pgr)))
              kv "Maximized" (T.pack (show (pgrMaximizedPane pgr)))

            ------------------------------------------------ Plots ---------
            Plots -> do
              heading "Plots"
              muted "Auto ticks, shared scales, and decimation."
              -- chart data lives in "DemoData" (plus the §Plots section below).
              responsiveRowCol 760 (tight . gap 16 . fillW) $ do
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Sine + cosine"
                  void $ plot (minH 240 . fillW) sineCosineChart
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Weekly counts"
                  void $ barChart (minH 240 . fillW) weeklyBars
              responsiveRowCol 760 (tight . gap 16 . fillW) $ do
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Sleep vs focus"
                  void $ plot (minH 240 . fillW) sleepFocusChart
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Area"
                  void $ areaChart (minH 240 . fillW) areaDemo
              columnWith (tight . gap gapMicro . fillW) $ do
                muted "Drawing"
                ps <- uiPlotStyle
                void $ diagram (fillW . maxH 200) (drawingSample ps)

            ------------------------------------------- Diagnostics ---------
            Diagnostics -> do
              heading "Diagnostics"
              mapM_ (uncurry kv) . dtSummary =<< debugText =<< askSdlDebug
              kv "Last drop event" (orDash dropRaw)
          setActiveTab newTab

  -------------------------------------------------------------- overlays ---
  -- Debug window: a plain draggable window opened by the toolbar toggle.
  when debugOpen $ do
    rows <- debugText =<< askSdlDebug
    (win, _) <- window True "Debug" (debugBody rows)
    when (respClicked win) (setDebug False)
  -- About modal: modal gives (response, _); clicking anywhere or pressing Esc
  -- sets respClicked on the response, which closes it.
  (aboutResp, _) <-
    modal aboutOpen "About" $ do
      heading "nano-ui"
      muted "Immediate-mode GUI for Haskell."
      muted "Esc closes this dialog, then the app."
      rowWith (gap gapInline . fillW) $ do
        flex
        whenM (button "Close") (setAbout False)
  when (respClicked aboutResp) (setAbout False)

------------------------------------------------------------------------------
-- §4  Controls-tab helpers
------------------------------------------------------------------------------

-- | Keep a caption close to its field; the enclosing column spaces field groups.
demoField :: T.Text -> NanoUI a -> NanoUI a
demoField caption widget =
  columnWith (tight . gap 4 . fillW) $ do
    labelWith (tight . fontMuted . fillW) caption
    widget

-- | Dashed-out empty values in the State readout.
orDash :: T.Text -> T.Text
orDash s = if T.null s then "-" else s

------------------------------------------------------------------------------
-- §5  Typography demo data
------------------------------------------------------------------------------

-- | Static gallery rows; used by the Typography tab. Demonstrates the @font*@
-- style combinators on plain labels.
typeScale :: NanoUI ()
typeScale =
  columnWith (tight . gap gapMicro . fillW) $
    for_
      [ ("32px", fontSize 32 . fontBold, "Display Headline")
      , ("24px", fontSize 24 . fontSemiBold, "Page Section Title")
      , ("18px", fontSize 18 . fontMedium, "Card Subtitle & Highlights")
      , ("16px", fontSize 16, "Standard body text (16px base line height)")
      , ("12px", fontSize 12 . fontMuted, "Auxiliary caption, footnote, or timestamp")
      ]
      $ \(size, style, sample) ->
        rowWith (tight . gap gapInline . alignMid . fillW) $ do
          labelWith (tight . alignMid . fixedW 60 . fontMono . fontMuted) size
          labelWith (alignMid . style) sample

weightsStyles :: NanoUI ()
weightsStyles =
  columnWith (tight . gap gapMicro . fillW) $
    for_
      [ ("Light", fontLight, sphinx)
      , ("Normal", id, sphinx)
      , ("Medium", fontMedium, sphinx)
      , ("SemiBold", fontSemiBold, sphinx)
      , ("Bold", fontBold, sphinx)
      , ("ExtraBold", fontExtraBold, sphinx)
      , ("Black", fontBlack, sphinx)
      , ("Italic", fontItalic, "Slanted synthetic italic font style.")
      , ("Underline", fontUnderline, "Underlined emphasis and interactive links.")
      , ("Strike", fontStrike, "Completed tasks and deprecated pricing.")
      , ("Both", fontUnderline . fontStrike, "Both underline and strikethrough lines.")
      ]
      $ \(name, style, sample) ->
        rowWith (tight . gap gapInline . fillW) $ do
          labelWith (tight . fixedW 96 . fontMono . fontMuted) name
          labelWith style sample
 where
  sphinx = "Sphinx of black quartz, judge my vow."

-- | Mixed styles and links in one wrapped paragraph.
richTextSample :: NanoUI ()
richTextSample = do
  (lastLink, setLastLink) <- useState ("none yet" :: T.Text)
  target <-
    richTextWith fillW
      [ "A paragraph can mix ", strong "bold", ", ", emphasis "italic", ", "
      , inlineCode "monospace", " and ", inlineWith (fontSemiBold . fontColor (colorRGBA 229 192 123 255)) "coloured"
      , " pieces, ", inlineWith (fontSize 20) "larger", " ones on the same baseline, and links such as "
      , hyperlink "documentation" "the documentation", " or ", hyperlink "changelog" "the changelog"
      , ". It wraps at the width of its column."
      ]
  for_ target setLastLink
  labelWith fontMuted ("Last link clicked: " <> lastLink)

colorHighlights :: NanoUI ()
colorHighlights =
  columnWith (tight . gap gapMicro . fillW) $ do
    rowWith (tight . gap gapInline . fillW) $
      for_
        [ (colorRGBA 224 108 117 255, "Crimson Red")
        , (colorRGBA 152 195 121 255, "Emerald Green")
        , (colorRGBA 229 192 123 255, "Amber Gold")
        , (colorRGBA 86 182 194 255, "Glacier Cyan")
        , (colorRGBA 198 120 221 255, "Orchid Violet")
        ]
        $ \(color, name) -> labelWith (fontBold . fontColor color) name
    rowWith (tight . gap gapInline . fillW . alignMid) $ do
      muted "Sale example:"
      labelWith (fontStrike . fontMuted) "$129.00"
      labelWith (fontSize 18 . fontBold . fontColor (colorRGBA 152 195 121 255)) "$79.00"
      labelWith (fontSize 12 . fontItalic . fontColor (colorRGBA 229 192 123 255)) "(Save 38%)"

------------------------------------------------------------------------------
-- §6  List & Table demo data
------------------------------------------------------------------------------

-- | Case-folded haystack used to filter 'demoPeople'.
personSearchText :: DemoPerson -> T.Text
personSearchText p =
  T.toCaseFold $
    T.intercalate
      " "
      [ demoPersonName p
      , demoPersonDept p
      , demoPersonCity p
      , demoPersonRole p
      ]

-- | Filter the people list on a committed search query. Callers memoize the
-- result (see the Searchable list demo) so the fold is not re-run every frame.
peopleMatching :: T.Text -> [DemoPerson]
peopleMatching raw
  | T.null raw = demoPeople
  | otherwise =
      let q = T.toCaseFold raw
       in filter (\p -> q `T.isInfixOf` personSearchText p) demoPeople

personRowLabel :: DemoPerson -> T.Text
personRowLabel p =
  demoPersonName p
    <> " - "
    <> demoPersonRole p
    <> ", "
    <> demoPersonCity p
    <> " ("
    <> T.pack (show (demoPersonAge p))
    <> ")"

-- | Column names for human-readable sort / hidden readouts.
demoTableColumnLabels :: [T.Text]
demoTableColumnLabels = ["Name", "Dept", "Age", "City", "Role"]

tableHiddenLabel :: [Int] -> T.Text
tableHiddenLabel [] = "none"
tableHiddenLabel hidden =
  T.intercalate
    ", "
    [ demoTableColumnLabels !! i
    | i <- hidden
    , i >= 0 && i < length demoTableColumnLabels
    ]

tableColumnLabel :: SortCol -> T.Text
tableColumnLabel s =
  let idx = sortColIndex s
   in if idx >= 0 && idx < length demoTableColumnLabels
        then demoTableColumnLabels !! idx
        else "-"

tableSortDirText :: SortCol -> T.Text
tableSortDirText s =
  case sortColDir s of
    SortAsc -> "ascending"
    SortDesc -> "descending"

------------------------------------------------------------------------------
-- §7  Pane grid demo
------------------------------------------------------------------------------

demoPaneGridCfg :: Bool -> PaneGridConfig NanoUIEs
demoPaneGridCfg showHeader =
  defaultPaneGridConfig
    { pgLayout = fillW . fixedH 380
    , pgSpacing = 4
    , pgMinSize = 60
    , pgLeeway = 6
    , pgViewPane = demoPaneView showHeader
    }

demoPaneTitle :: Word64 -> Bool -> T.Text
demoPaneTitle pid maximized =
  "Pane "
    <> T.pack (show pid)
    <> if maximized then "  (maximized)" else ""

-- | The header is just the pane's own content, so it is entirely optional:
-- 'showHeader' 'False' drops it and the pane becomes a bare canvas body. The
-- whole pane is still a drag handle either way ('pvDraggable'), so a headerless
-- pane can be grabbed anywhere to reorder it.
demoPaneHeader :: Word64 -> Bool -> PaneGridCtx NanoUIEs -> NanoUI ()
demoPaneHeader pid maximized pctx =
  rowWith (tight . gap gapMicro . alignMid . fillW) $ do
    box (alignMid . fixedWH 3 16) demoAccent
    labelWith (tight . alignMid . fontMedium) (demoPaneTitle pid maximized)
    flex
    whenM (button "+") (void (pgcSplit pctx AxisV))
    whenM (button "=") (void (pgcSplit pctx AxisH))
    whenM (button (if maximized then "R" else "M")) (if maximized then pgcRestore pctx else pgcMaximize pctx)
    whenM (button "x") (pgcClose pctx)

-- | Each pane is a card, so panes stay distinct with the headers off.
demoPaneView :: Bool -> Word64 -> PaneGridCtx NanoUIEs -> NanoUI PaneView
demoPaneView showHeader pid pctx = do
  let maximized = pgcMaximized pctx
  panelWith (padAll gapLayout . gap gapLayout . grow) $ do
    when showHeader $ do
      demoPaneHeader pid maximized pctx
      separator
    void $ muted ("Contents of " <> T.pack (show pid) <> ". Drag the pane to move or split it.")
  pure
    PaneView
      { pvTitle = demoPaneTitle pid maximized
      , pvDraggable = True
      , pvDragPick = Nothing
      }

------------------------------------------------------------------------------
-- §8  Plots demo data
------------------------------------------------------------------------------

-- Hours slept (X) vs focus score (Y).
sleepFocus :: [(Double, Double)]
sleepFocus =
  [ (4.0, 3.0)
  , (5.5, 4.5)
  , (6.0, 6.0)
  , (6.5, 7.5)
  , (7.0, 8.0)
  , (7.5, 8.5)
  , (8.0, 7.5)
  , (9.0, 6.5)
  ]

sleepFocusChart :: Chart
sleepFocusChart =
  withGrid GridBoth $
    withLegend LegendRight $
      withYAxis "focus" $
        withXAxis "hours slept" $
          chart [scatter "focus" sleepFocus, line "trend" sleepFocus]

areaDemo :: [(Double, Double)]
areaDemo = [(x, abs (sin x)) | x <- [0, 0.05 .. (2 * pi)]]

drawingSample :: PlotStyle -> Diagram B
drawingSample ps =
  (circle 0.45 # fc (plotFill ps) # lw none)
    <> (circle 0.28 # fc (plotInk ps) # lw none)
    <> (fromVertices [p2 (-0.5, -0.5), p2 (0.5, 0.5)] # lc (plotGrid ps) # lwO 1.5)

------------------------------------------------------------------------------
-- §9  Debug window content
------------------------------------------------------------------------------

type DebugRows = SmallArray (T.Text, T.Text)

-- | A debug sample formatted for the toolbar, the Diagnostics tab and the
-- Debug window.
data DebugText = DebugText
  { dtFps :: !T.Text
  , dtSummary :: !DebugRows
  , dtFrame :: !DebugRows
  , dtDraw :: !DebugRows
  , dtDisplay :: !DebugRows
  , dtRuntime :: !DebugRows
  }

-- The backend samples at 4 Hz. Share the formatted text between samples
-- instead of running printf for every field on every continuous frame.
data CachedDebugText = CachedDebugText !SdlDebugSnapshot !DebugText

debugText :: SdlDebugSnapshot -> NanoUI DebugText
debugText s = do
  ctx <- askContext
  liftIO $ do
    cached <- askHostIO ctx
    case cached of
      Just (CachedDebugText previous text) | previous == s -> pure text
      _ -> do
        let c = dbgCore s
            text =
              DebugText
                { dtFps =
                    if dbgPresentFps c > 0
                      then T.pack (printf "%4.0f FPS / %5.2f ms" (dbgPresentFps c) (dbgFrameMs c))
                      else ""
                , dtSummary = summaryRows s
                , dtFrame = frameRows s
                , dtDraw = drawRows s
                , dtDisplay = displayRows s
                , dtRuntime = smallArrayFromList (formatCoreRtsRows c)
                }
        setHost ctx (CachedDebugText s text)
        pure text

debugBody :: DebugText -> NanoUI ()
debugBody text =
  columnWith (tight . gap 4 . minW 300 . fillW) $ do
    debugSection "Frame" (dtFrame text)
    separator
    debugSection "Draw" (dtDraw text)
    separator
    debugSection "Display" (dtDisplay text)
    separator
    debugSection "Runtime" (dtRuntime text)

debugSection :: T.Text -> DebugRows -> NanoUI ()
debugSection title rows = do
  heading title
  mapM_ (\(k, v) -> kvMono k v) rows

summaryRows :: SdlDebugSnapshot -> DebugRows
summaryRows s =
  let c = dbgCore s
      haskellMs = dbgUiMs c + dbgRenderMs c
   in smallArrayFromList
        [ ("Present FPS", T.pack (printf "%.1f fps" (dbgPresentFps c)))
        , ("Display", T.pack (printf "%d Hz" (dbgRefreshHz s)))
        , ("Loop FPS", T.pack (printf "%.1f fps" (dbgLoopFps c)))
        , ("Frame Time", T.pack (printf "%.2f ms" (dbgFrameMs c)))
        , ("Haskell Time", T.pack (printf "%.2f ms (UI: %.2f, Render: %.2f)" haskellMs (dbgUiMs c) (dbgRenderMs c)))
        , ("SDL Present", T.pack (printf "%.2f ms" (dbgPresentMs c)))
        , ("Draw Calls", T.pack (show (dbgCmds c)))
        , ("Vertices / Indices", T.pack (printf "%d / %d" (dbgVerts c) (dbgIndices c)))
        , ("Renderer", dbgRenderer s <> if dbgVsync s then " (vsync on)" else " (vsync off)")
        ]

frameRows :: SdlDebugSnapshot -> DebugRows
frameRows s =
  let c = dbgCore s
      haskellMs = dbgUiMs c + dbgRenderMs c
   in smallArrayFromList
        [ ("present", T.pack (printf "%6.1f fps" (dbgPresentFps c)))
        , ("loop", T.pack (printf "%6.1f fps" (dbgLoopFps c)))
        , ("frame cpu", T.pack (printf "%7.2f ms" (dbgFrameMs c)))
        , ("haskell", T.pack (printf "%7.2f ms" haskellMs))
        , ("  ui", T.pack (printf "%7.2f ms" (dbgUiMs c)))
        , ("  render", T.pack (printf "%7.2f ms" (dbgRenderMs c)))
        , ("sdl present", T.pack (printf "%7.2f ms" (dbgPresentMs c)))
        , ("draws", T.pack (printf "%10d" (dbgPresents c)))
        , ("skips", T.pack (printf "%10d" (dbgSkips c)))
        ]

drawRows :: SdlDebugSnapshot -> DebugRows
drawRows s =
  let c = dbgCore s
   in smallArrayFromList
        [ ("verts", T.pack (printf "%10d" (dbgVerts c)))
        , ("indices", T.pack (printf "%10d" (dbgIndices c)))
        , ("cmds", T.pack (printf "%10d" (dbgCmds c)))
        ]

displayRows :: SdlDebugSnapshot -> DebugRows
displayRows s =
  let c = dbgCore s
   in smallArrayFromList
        [ ("window", T.pack (printf "%4.0fx%-5.0f" (dbgWinW c) (dbgWinH c)))
        , ("scale", T.pack (printf "%10.2f" (dbgScale s)))
        , ("mouse", T.pack (printf "%4.0f, %-4.0f" (dbgMouseX c) (dbgMouseY c)))
        , ( "renderer"
          , dbgRenderer s <> if dbgVsync s then "  vsync on" else "  vsync off"
          )
        , ("font", T.pack (dbgFontPath s))
        ]

------------------------------------------------------------------------------
-- §10  CLI plumbing
------------------------------------------------------------------------------

data DemoConfig = DemoConfig
  { cfgVsync :: !Bool
  , cfgContinuous :: !Bool
  , cfgFullscreen :: !Bool
  , cfgBorderless :: !Bool
  , cfgAlwaysOnTop :: !Bool
  , cfgWidth :: !(Maybe Float)
  , cfgHeight :: !(Maybe Float)
  , cfgHelp :: !Bool
  }

defaultDemoConfig :: DemoConfig
defaultDemoConfig =
  DemoConfig
    { cfgVsync = True
    , cfgContinuous = False
    , cfgFullscreen = False
    , cfgBorderless = False
    , cfgAlwaysOnTop = False
    , cfgWidth = Nothing
    , cfgHeight = Nothing
    , cfgHelp = False
    }

options :: [OptDescr (DemoConfig -> DemoConfig)]
options =
  [ Option ['v'] ["vsync"] (ReqArg (\s cfg -> cfg { cfgVsync = s `elem` ["true", "True", "1"] }) "BOOL") "Enable or disable vsync (true/false, default: true)"
  , Option ['c', 'b', 'f'] ["continuous", "benchmark", "fps"] (NoArg (\cfg -> cfg { cfgContinuous = True, cfgVsync = False })) "Continuous unthrottled rendering, to show uncapped FPS (disables vsync)"
  , Option ['F'] ["fullscreen"] (NoArg (\cfg -> cfg { cfgFullscreen = True })) "Launch window in fullscreen mode"
  , Option [] ["borderless"] (NoArg (\cfg -> cfg { cfgBorderless = True })) "Launch borderless window"
  , Option ['t'] ["always-on-top"] (NoArg (\cfg -> cfg { cfgAlwaysOnTop = True })) "Keep window always on top"
  , Option ['W'] ["width"] (ReqArg (\s cfg -> cfg { cfgWidth = readMaybe s }) "PX") "Initial window width in pixels (default: 1280)"
  , Option ['H'] ["height"] (ReqArg (\s cfg -> cfg { cfgHeight = readMaybe s }) "PX") "Initial window height in pixels (default: 800)"
  , Option ['h', '?'] ["help"] (NoArg (\cfg -> cfg { cfgHelp = True })) "Show help and command-line options"
  ]
