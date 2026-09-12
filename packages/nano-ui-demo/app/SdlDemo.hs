-- | nano-ui widget cookbook.
--
-- The whole showcase is one function, 'demoUi'. Every tab is a different
-- widget family; jump to the family you care about.
--
-- Every interactive widget follows the same immediate-mode shape:
--
-- @
--   (resp, newVal) <- widget label currentVal   -- draw it, get what changed
--   when (respClicked resp) (doSomething)       -- react to gestures
--   setX newVal                                 -- write it back to state
-- @
--
-- The widget's value outlives the frame only because you persist it. State
-- lives in hooks created by the @use*@ functions. Hooks must be called in the
-- same order every frame, so all of them sit together at the top of 'demoUi'
-- — even for tabs that are currently hidden.
--
-- Style is expressed as layout-style functions threaded through the container
-- widget: columnWith (padAll 6 . gap 8 . fillW) $...  Text widgets compose
-- font styles the same way (fontBold, fontSize n, fontMuted, ...).
--
-- Families, by tab:
--
--   * Controls     — button, checkbox, slider, select, comboBox,
--                    boundedRadioFieldset, colorPicker, textInput, textArea,
--                    button + tooltip, contextMenu, file dialogs, dropZone
--   * Graphics     — image gallery + progressBar driven by a pulsing value
--   * Typography   — label / labelEx + the @font*@ style combinators
--   * List         — tree, searchField
--   * Table        — tableCfg (needs useTableSort)
--   * Panes        — paneGrid
--   * Plots        — plot, barChart, areaChart, diagram (data at the bottom)
--   * Diagnostics  — debug readouts from the SDL backend
--
-- The entry point is 'main' (§1) with a small CLI; the argument plumbing is the
-- last section of this file. The automated UI test lives in its own module,
-- "SdlSelftest".

module SdlDemo
    ( main
    , demoImages
    , demoUi
    ) where

import Control.Monad (unless, void, when)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Word (Word64)
import Effectful (Eff, type (:>))
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Debug (CoreDebugSnapshot (..), formatCoreRtsRows)
import NanoUI.Diagrams
import NanoUI.Monad (askInput)
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
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Read as T.Read
import qualified Data.Vector as V
import qualified SdlSelftest

import DemoData
  ( DemoPerson (..)
  , colPeople
  , demoPeople
  , demoTree
  , sineCosineChart
  , weeklyBars
  )

------------------------------------------------------------------------------
-- §1  App entry (main)
------------------------------------------------------------------------------

-- | Run @cabal run -fsdl nano-ui-sdl-demo@ for the windowed app, or
-- @cabal run -fsdl nano-ui-sdl-demo -- --selftest@ for the headless UI test
-- (defined in "SdlSelftest").
main :: IO ()
main = do
  args <- getArgs
  if "--selftest" `elem` args
    then SdlSelftest.selftest demoImages demoUi
    else do
      let cfg = parseArgs args
      if cfgHelp cfg
        then putStr (usageInfo "Usage: nano-ui-sdl-demo [OPTIONS]" options)
        else do
          let winSize = case (cfgWidth cfg, cfgHeight cfg) of
                (Just w, Just h) -> Size w h
                (Just w, Nothing) -> Size w 800
                (Nothing, Just h) -> Size 1280 h
                (Nothing, Nothing) -> Size 1280 800
          runSdlApp
            defaultSdlOptions
              { sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
              , sdlAppImages = demoImages
              , sdlAppTheme = Just tomorrowNightMinDarkTheme
              , sdlAppVsync = cfgVsync cfg
              , sdlAppContinuous = cfgContinuous cfg
              , sdlWindowFullscreen = cfgFullscreen cfg
              , sdlWindowBorderless = cfgBorderless cfg
              , sdlWindowAlwaysOnTop = cfgAlwaysOnTop cfg
              , sdlWindowSize = winSize
              }
            demoUi

------------------------------------------------------------------------------
-- §2  Assets & shared look
------------------------------------------------------------------------------

-- | Three 32x32 images registered with the SDL context (see the Graphics tab
-- and the SdlSelftest image check). Pixel data is at the very bottom.
demoImages :: SmallArray RgbaImage
demoImages =
  smallArrayFromList
    [ RgbaImage (ImageId 1) 32 32 swatchPixels
    , RgbaImage (ImageId 2) 32 32 checkerPixels
    , RgbaImage (ImageId 3) 32 32 stripePixels
    ]

-- | Demo accent used across the state readout and pane headers.
demoAccent :: Color
demoAccent = colorRGBA 204 102 102 255

-- Spacing rhythm for the demo cards and columns.
gapLayout, gapInline, gapMicro, gapText :: Float
gapLayout = 6
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
-- once per process). The chosen family is pushed to the backend through
-- 'setSdlUiFont' and re-rendered on the next frame. The fallback list only
-- kicks in on systems where the scan finds no font files.
demoFontFamilies :: [T.Text]
demoFontFamilies =
  let families = map T.pack (unsafePerformIO listFontFamilies)
   in if null families
        then ["Inter", "Noto Sans", "Adwaita Sans", "Cantarell", "Liberation Sans", "FreeSans"]
        else families
{-# NOINLINE demoFontFamilies #-}

------------------------------------------------------------------------------
-- §3  The showcase UI
------------------------------------------------------------------------------

-- | The whole app. Composition, top to bottom:
--   1. state hooks          every frame, fixed order (see module header)
--   2. toolbar              brand, live FPS, OK / Cancel / About / Debug
--   3. two-column body      left: live state; right: tabbed demos
--   4. overlays             Debug window + About modal
debugOpenFromEnv :: Bool
debugOpenFromEnv = unsafePerformIO (isJust <$> lookupEnv "NANO_DEBUG_OPEN")
{-# NOINLINE debugOpenFromEnv #-}

demoUi :: NanoUI ()
demoUi = do
  ---------------------------------------------------------------- hooks ---
  -- Toolbar / overlays.
  (click, setClick) <- useText "" -- label of the last button / menu item clicked
  (aboutOpen, setAbout) <- useFlag False
  (debugOpen, setDebug) <- useFlag debugOpenFromEnv
  -- Controls tab.
  (checked, setChecked) <- useFlag False -- checkbox
  (vol, setVol) <- useText "50" -- slider, as text
  (quality, setQuality) <- useText "Medium" -- select
  (accentHex, setAccent) <- useText (colorPickerToHexA demoAccent) -- colorPickerRGBA
  (themeName, setThemeName) <- useText (themeDisplayName ThemeDefault) -- boundedRadioFieldset
  (fontChoice, setFontChoice) <- useText "Inter" -- comboBox
  (name, setName) <- useText "" -- textInput
  (notes, setNotes) <- useText "" -- textArea
  (dropLog, setDropLog) <- useText "" -- dropZone result, multi-line
  (dropHovering, setDropHovering) <- useFlag False -- drag-over state
  -- File dialog handles; results land in the paths below via useFileDialog.
  (openDlg, setOpenDlg) <- useState (Nothing :: Maybe FileDialogId)
  (saveDlg, setSaveDlg) <- useState (Nothing :: Maybe FileDialogId)
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
  (searchQuery, setSearchQuery) <- useText "" -- committed searchField value
  (peopleMatches, setPeopleMatches) <- useState demoPeople -- filtered rows
  (treeSel, setTreeSel) <- useText "0" -- tree selection index
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
  let rawDrop = T.intercalate " | " [T.pack (show (dropEventType ev)) <> " " <> dropEventData ev | ev <- V.toList (inputDrops rawInp)]
  when (not (T.null rawDrop)) (setDropRaw rawDrop)

  -------------------------------------------------------------- toolbar ---
  scrollWith (tight . grow) $
    columnWith (padAll 6 . gap gapLayout . fillW) $ do
      panelWith (padXY 14 10 . gap gapInline . fillW) $
        toolbar $ do
          columnWith (tight . gap gapText) $ do
            heading "nano-ui"
            muted "SDL3 demo"
          flex
          -- Live frame stats + the shared header buttons.
          snap <- askSdlDebug
          let c = dbgCore snap
              fpsText =
                if dbgPresentFps c > 0
                  then T.pack (printf "%4.0f FPS (%5.2f ms)" (dbgPresentFps c) (dbgFrameMs c))
                  else ""
          unless (T.null fpsText) $
            void (labelEx (tight . fontMono . fontMuted $ defaultLayout) fpsText)
          whenM (button "OK") (setClick "OK")
          whenM (button "Cancel") (setClick "Cancel")
          whenM (button "About") (setAbout True)
          whenM (button "Debug") (setDebug (not debugOpen))

      ----------------------------------------------------------- body ----
      responsiveRowCol 720 (tight . gap gapLayout . fillW $ defaultLayout) $ do
        -- Left: a live readout of every hook value above. Tweak a widget on the
        -- right and watch its line update — instant confirmation the write-back
        -- idiom worked.
        columnWith (tight . gap gapLayout . fillW) $ do
          card $ do
            heading "State"
            let accent = fromMaybe demoAccent (colorPickerFromHex accentHex)
            kv "Feature" (onOff checked)
            kv "Volume" vol
            kv "Quality" quality
            rowWith (tight . gap gapInline . alignMid . fillW) $ do
              box (fixedWH 20 20 defaultLayout) accent
              kv "Accent" accentHex
            kv "Theme" themeName
            kv "Font" fontChoice
            kv "Name" (orDash name)
            kv "Notes" (orDash notes)
            kv "Tree" treeSel
            kv "Table sort" (tableColumnLabel tableSortVal)
            kv "Table order" (tableSortDirText tableSortVal)
            kv "Clicked" (orDash click)
            kv "Open file" (orDash openPath)
            kv "Save file" (orDash savePath)
            kv "Folder" (orDash folderPath)
            kv "Dropped" (orDash (T.take 80 (firstDropLine dropLog)))
            muted "Click widgets or type in Name or Notes."
            muted "Esc closes About, then quits."

        -- Right: the tabbed widget demos. Each tab body below is one widget
        -- family; its state hooks all live at the top of demoUi.
        card $ do
          boundedTabs Controls (T.pack . show) $ \case
            ----------------------------------------------- Controls ---------
            -- Form widgets. The returned value is stored back through the hook;
            -- the State card on the left then shows it.
            Controls -> do
              heading "Controls"
              (_, cVal) <- checkbox "Feature" False
              setChecked cVal
              (_, vVal) <- rowWith (tight . gap 8 . alignMid . fillW) $ do
                void $ label "Volume"
                slider 0 100 50
              setVol (T.pack (show (round vVal :: Int)))
              let qualities = ["Low", "Medium", "High"]
              (_, qualityIdx) <- rowWith (tight . gap 8 . alignMid . fillW) $ do
                void $ label "Quality"
                select qualities 1
              setQuality (qualities !! qualityIdx)
              void $ label "Accent"
              (_, aVal) <- colorPickerRGBA demoAccent
              setAccent (colorPickerToHexA aVal)
              void $ label "Theme"
              (_, tVal) <- boundedRadioFieldset ThemeDefault themeDisplayName
              setThemeName (themeDisplayName tVal)
              setUiTheme (themeForChoice tVal)
              muted "Font: a combo box — type to filter (applies on Enter, a click, or losing focus; Esc reverts), scroll the list, hover or arrow to highlight."
              (fResp, fVal) <- comboBox "Font" demoFontFamilies fontChoice
              setFontChoice fVal
              when (respChanged fResp && not (T.null fVal)) $
                setSdlUiFont (FontSearch [T.unpack fVal])
              (_, nVal) <- rowWith (tight . gap 8 . alignMid . fillW) $ do
                void $ label "Name"
                textInput ""
              setName nVal
              void $ label "Notes"
              (_, notesVal) <- textArea "Edit me.\nSecond line."
              setNotes notesVal
              sep
              -- Popups & menus: act on respClicked of the item you want.
              heading "Popups & Menus"
              rowWith (tight . gap gapInline . fillW) $ do
                btnTip <- button' "Hover for Tooltip"
                tooltip btnTip "This is a floating tooltip widget!"
                btnMenu <- button' "Right-click Menu"
                void $ contextMenu btnMenu $ do
                  menuHeader "Context Menu"
                  menuSeparator
                  whenM (menuItemWithShortcut "Cut" "Ctrl+X") (setClick "Cut")
                  whenM (menuItemWithShortcut "Copy" "Ctrl+C") (setClick "Copy")
                  whenM (menuItemWithShortcut "Paste" "Ctrl+V") (setClick "Paste")
                  menuSeparator
                  menuItemDisabled "Disabled Option"
              sep
              -- File dialogs: ask for a modal dialog handle, store it, and poll
              -- it every frame via useFileDialog (defined below).
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
              sep
              -- Drag & drop: dropZone returns a target; onDrop reads its files
              -- and texts. dropHovering mirrors the hover state for styling.
              heading "Drag & Drop"
              muted "Drag a file or highlighted text from another app onto the zone."
              (_, _, dropTgt) <-
                dropZone (padXY 16 12 . gap gapText . fillW $ defaultLayout) $ do
                  columnWith (tight . gap gapText . fillW) $ do
                    rowWith (tight . gap gapInline . alignMid . fillW) $ do
                      void $ labelWith fontBold "Drop Zone"
                      flex
                      void $ labelEx (tight . fontMono . fontMuted $ defaultLayout) (if dropHovering then "hovering" else "idle")
                    void $ labelEx (tight . fontMuted . fillW $ defaultLayout) $
                      if dropHovering
                        then "Release to accept dropped files or text."
                        else "Files land here; text lands here too."
                    when (not (T.null dropLog)) $ do
                      sep
                      void $ labelEx (tight . fontMono . fillW $ defaultLayout) dropLog
              onDrop dropTgt $ do
                let keepFront n t = if T.length t <= n then t else T.take (n - 1) t <> "…"
                    keepEnd n t = if T.length t <= n then t else "…" <> T.takeEnd (n - 1) t
                    droppedLines =
                      [ "file:  " <> keepEnd 60 f | f <- dropFiles dropTgt ]
                        ++ [ "text:  " <> keepFront 60 t | t <- dropTexts dropTgt ]
                setDropLog (if null droppedLines then dropLog else T.intercalate "\n" droppedLines)
              when (dropHovered dropTgt && not dropHovering) (setDropHovering True)
              when (not (dropHovered dropTgt) && dropHovering) (setDropHovering False)

            ----------------------------------------------- Graphics ---------
            Graphics -> do
              heading "Graphics"
              sep
              -- Images registered from demoImages.
              rowWith (tight . gap gapInline . fillW) $ do
                thumb (ImageId 1) "Swatch"
                thumb (ImageId 2) "Checker"
                thumb (ImageId 3) "Stripe"
              sep
              -- A plain response-driven bar. pulse provides a smooth
              -- clock-driven 0-1 sweep and keepAnimating holds it live.
              muted "A single rounded bar, smoothly oscillating 0–100%."
              progResp <- progressBar =<< pulse 6
              void (keepAnimating progResp)

            --------------------------------------------- Typography ---------
            Typography -> do
              heading "Typography & Font Styling"
              muted "Font sizing, variable weights, synthetic slant, and text decorations."
              sep
              -- Live playground: type in the box, flip toggles, drag the size
              -- slider and watch the composed font style update the preview.
              heading "Live Playground"
              rowWith (tight . gap gapInline . fillW . alignMid) $ do
                void $ label "Preview text"
                (_, tVal) <- textInput sampleText
                setSampleText tVal
              rowWith (tight . gap gapInline . fillW . alignMid) $ do
                (_, bVal) <- checkbox "Bold" typeBold
                setTypeBold bVal
                (_, iVal) <- checkbox "Italic" typeItalic
                setTypeItalic iVal
                (_, uVal) <- checkbox "Underline" typeUnderline
                setTypeUnderline uVal
                (_, sVal) <- checkbox "Strike" typeStrike
                setTypeStrike sVal
              rowWith (tight . gap gapInline . fillW . alignMid) $ do
                void $ label "Size"
                (_, szVal) <- slider 12 40 typeSize
                setTypeSize szVal
                void $ labelEx (tight . fontMono . fontMuted $ defaultLayout) (T.pack (printf "%.0f px" szVal))
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
                void $ labelWith customStyle previewTxt
              sep
              heading "Type Scale"
              typeScale
              sep
              heading "Weights & Styles"
              weightsStyles
              sep
              heading "Color & Highlights"
              colorHighlights

            ----------------------------------------------------- List ---------
            List -> do
              heading "Tree"
              -- tree: pass a selection index, get the clicked one back.
              let sel0 =
                    case T.Read.decimal treeSel of
                      Right (n, _) -> n
                      Left _ -> 0
              scroll2DWith (fixedH 300 . fillW) $ do
                (_, sel) <- tree "demo" demoTree sel0
                setTreeSel (T.pack (show sel))
              sep
              heading "Searchable list"
              muted "Type to filter. The debounced search commits on a pause; the filtered list is cached and only recomputed when the committed query changes."
              (qResp, qVal) <- searchField "Filter people (name, role, city…)" ""
              when (respChanged qResp) $ do
                setSearchQuery qVal
                setPeopleMatches (peopleMatching qVal)
              rowWith (tight . gap gapInline . fillW . alignMid) $ do
                muted ("Committed: " <> (if T.null searchQuery then "—" else searchQuery))
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
                        void $ labelEx (tight . fillW $ defaultLayout) (personRowLabel p)

            ---------------------------------------------------- Table ---------
            Table -> do
              heading "Table"
              muted "Click a header to sort. Drag a header to reorder."
              muted "Drag a header edge to resize. Right-click a header to hide."
              -- tableCfg re-renders every frame; keep the sort state in a hook
              -- (useTableSort) and mirror changes back into it.
              tableResp <-
                tableCfg
                  demoTableCfg
                  (tight . fillW . fixedH 280 $ defaultLayout {layoutGap = 0})
                  "people"
                  colPeople
                  demoPeople
                  tableSortVal
              let nextSort = tableSort tableResp
              when (tableRespChanged tableResp) (setTableSort nextSort)
              sep
              kv "Sorted by" (tableColumnLabel nextSort)
              kv "Order" (tableSortDirText nextSort)
              kv "Hidden" (tableHiddenLabel (tableHiddenIndices tableResp))

            ---------------------------------------------------- Panes ---------
            Panes -> do
              heading "Pane Grid"
              muted "Drag a divider to resize. Drag a pane onto another pane to reorder:"
              muted "drop on its center to swap, on its edge to split it."
              muted "Drag a pane to the grid's outer edge to restructure at the top level."
              muted "+ splits vertically, = splits horizontally, x closes, M maximizes, R restores."
              muted "Arrow keys jump between panes while the grid is focused."
              (hdrResp, headersOn) <- checkbox "Pane headers" showPaneHeaders
              when (respChanged hdrResp) (setShowPaneHeaders headersOn)
              pgr <- paneGrid (demoPaneGridCfg headersOn)
              sep
              kv "Panes" (T.pack (show (pgrPaneCount pgr)))
              kv "Focused" (T.pack (show (pgrFocusedPane pgr)))
              kv "Maximized" (T.pack (show (pgrMaximizedPane pgr)))

            ------------------------------------------------ Plots ---------
            Plots -> do
              heading "Plots"
              muted "Auto ticks, shared scales, and decimation."
              -- chart data lives in "DemoData" (plus the §Plots section below).
              columnWith (tight . gap gapLayout . fillW) $ do
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Sine + cosine"
                  void $ plot (fillW defaultLayout) sineCosineChart
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Weekly counts"
                  void $ barChart (fillW defaultLayout) weeklyBars
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Sleep vs focus"
                  void $ plot (fillW defaultLayout) sleepFocusChart
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Area"
                  void $ areaChart (fillW defaultLayout) areaDemo
                columnWith (tight . gap gapMicro . fillW) $ do
                  muted "Drawing"
                  ps <- uiPlotStyle
                  void $ diagram (fillW $ defaultLayout {layoutMaxH = 200}) (drawingSample ps)

            ------------------------------------------- Diagnostics ---------
            Diagnostics -> do
              heading "Diagnostics"
              snap <- askSdlDebug
              let c = dbgCore snap
                  haskellMs = dbgUiMs c + dbgRenderMs c
              kv "Present FPS" (T.pack (printf "%.1f fps" (dbgPresentFps c)))
              kv "Display" (T.pack (printf "%d Hz" (dbgRefreshHz snap)))
              kv "Loop FPS" (T.pack (printf "%.1f fps" (dbgLoopFps c)))
              kv "Frame Time" (T.pack (printf "%.2f ms" (dbgFrameMs c)))
              kv "Haskell Time" (T.pack (printf "%.2f ms (UI: %.2f, Render: %.2f)" haskellMs (dbgUiMs c) (dbgRenderMs c)))
              kv "SDL Present" (T.pack (printf "%.2f ms" (dbgPresentMs c)))
              kv "Draw Calls" (T.pack (printf "%d" (dbgCmds c)))
              kv "Vertices / Indices" (T.pack (printf "%d / %d" (dbgVerts c) (dbgIndices c)))
              kv "Renderer" (dbgRenderer snap <> if dbgVsync snap then " (vsync on)" else " (vsync off)" <> T.pack (printf ", refresh %d Hz" (dbgRefreshHz snap)))
              kv "Last drop event" (orDash dropRaw)
              kv "Evaluation" "Zero-Cost Inactive Tabs"
              kv "State" "SrcLoc Preserved"

  -------------------------------------------------------------- overlays ---
  -- Debug window: a plain draggable window opened by the toolbar toggle.
  when debugOpen $ do
    snap <- askSdlDebug
    (win, _) <- window True "Debug" (debugBody snap)
    onClick win (setDebug False)
  -- About modal: modal gives (response, _); clicking anywhere or pressing Esc
  -- fires onClick on the response, which closes it.
  (aboutResp, _) <-
    modal aboutOpen "About" $ do
      heading "nano-ui"
      muted "Immediate-mode GUI for Haskell."
      muted "Esc closes this dialog, then the app."
      rowWith (gap gapInline . fillW) $ do
        flex
        whenM (button "Close") (setAbout False)
  onClick aboutResp (setAbout False)

------------------------------------------------------------------------------
-- §4  Controls-tab helpers
------------------------------------------------------------------------------

-- | Poll a pending dialog handle; on completion clear it and hand the chosen
-- paths to the caller. Anything other than 'FileDialogPending' dismisses the
-- handle, so each result is consumed exactly once.
useFileDialog ::
  Maybe FileDialogId ->
  (Maybe FileDialogId -> NanoUI ()) ->
  ([FilePath] -> NanoUI ()) ->
  NanoUI ()
useFileDialog mdid clear onPaths =
  for_ mdid $ \did ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected paths -> onPaths paths >> clear Nothing
      _done -> clear Nothing

onOff :: Bool -> T.Text
onOff True = "on"
onOff False = "off"

-- | Dashed-out empty values in the State readout.
orDash :: T.Text -> T.Text
orDash s = if T.null s then "-" else s

-- | First line of a multi-line log, for compact summary rows.
firstDropLine :: T.Text -> T.Text
firstDropLine = maybe "" id . listToMaybe . T.lines

-- | Image tile in the Graphics tab: the caption is muted under the sprite.
thumb :: ImageId -> T.Text -> NanoUI ()
thumb iid caption =
  columnWith (tight . gap gapMicro) $ do
    image_ (fixedWH 88 88 defaultLayout) iid
    muted caption

------------------------------------------------------------------------------
-- §5  Typography demo data
------------------------------------------------------------------------------

-- | Static gallery rows; used by the Typography tab. Demonstrates the @font*@
-- style combinators on plain labels.
typeScale :: NanoUI ()
typeScale =
  columnWith (tight . gap gapMicro . fillW) $ do
    rowWith (tight . gap gapInline . alignMid . fillW) $ do
      void $ labelEx (tight . fixedW 60 . fontMono . fontMuted $ defaultLayout) "32px"
      void $ labelWith (fontSize 32 . fontBold) "Display Headline"
    rowWith (tight . gap gapInline . alignMid . fillW) $ do
      void $ labelEx (tight . fixedW 60 . fontMono . fontMuted $ defaultLayout) "24px"
      void $ labelWith (fontSize 24 . fontSemiBold) "Page Section Title"
    rowWith (tight . gap gapInline . alignMid . fillW) $ do
      void $ labelEx (tight . fixedW 60 . fontMono . fontMuted $ defaultLayout) "18px"
      void $ labelWith (fontSize 18 . fontMedium) "Card Subtitle & Highlights"
    rowWith (tight . gap gapInline . alignMid . fillW) $ do
      void $ labelEx (tight . fixedW 60 . fontMono . fontMuted $ defaultLayout) "16px"
      void $ labelWith (fontSize 16) "Standard body text (16px base line height)"
    rowWith (tight . gap gapInline . alignMid . fillW) $ do
      void $ labelEx (tight . fixedW 60 . fontMono . fontMuted $ defaultLayout) "12px"
      void $ labelWith (fontSize 12 . fontMuted) "Auxiliary caption, footnote, or timestamp"

weightsStyles :: NanoUI ()
weightsStyles =
  columnWith (tight . gap gapMicro . fillW) $ do
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Light"
      void $ labelWith fontLight "Sphinx of black quartz, judge my vow."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Normal"
      void $ label "Sphinx of black quartz, judge my vow."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Medium"
      void $ labelWith fontMedium "Sphinx of black quartz, judge my vow."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "SemiBold"
      void $ labelWith fontSemiBold "Sphinx of black quartz, judge my vow."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Bold"
      void $ labelWith fontBold "Sphinx of black quartz, judge my vow."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "ExtraBold"
      void $ labelWith fontExtraBold "Sphinx of black quartz, judge my vow."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Black"
      void $ labelWith fontBlack "Sphinx of black quartz, judge my vow."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Italic"
      void $ labelWith fontItalic "Slanted synthetic italic font style."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Underline"
      void $ labelWith fontUnderline "Underlined emphasis and interactive links."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Strike"
      void $ labelWith fontStrike "Completed tasks and deprecated pricing."
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelEx (tight . fixedW 80 . fontMono . fontMuted $ defaultLayout) "Both"
      void $ labelWith (fontUnderline . fontStrike) "Both underline and strikethrough lines."

colorHighlights :: NanoUI ()
colorHighlights =
  columnWith (tight . gap gapMicro . fillW) $ do
    rowWith (tight . gap gapInline . fillW) $ do
      void $ labelWith (fontBold . fontColor (colorRGBA 224 108 117 255)) "Crimson Red"
      void $ labelWith (fontBold . fontColor (colorRGBA 152 195 121 255)) "Emerald Green"
      void $ labelWith (fontBold . fontColor (colorRGBA 229 192 123 255)) "Amber Gold"
      void $ labelWith (fontBold . fontColor (colorRGBA 86 182 194 255)) "Glacier Cyan"
      void $ labelWith (fontBold . fontColor (colorRGBA 198 120 221 255)) "Orchid Violet"
    rowWith (tight . gap gapInline . fillW . alignMid) $ do
      muted "Sale example:"
      void $ labelWith (fontStrike . fontMuted) "$129.00"
      void $ labelWith (fontSize 18 . fontBold . fontColor (colorRGBA 152 195 121 255)) "$79.00"
      void $ labelWith (fontSize 12 . fontItalic . fontColor (colorRGBA 229 192 123 255)) "(Save 38%)"

------------------------------------------------------------------------------
-- §6  List & Table demo data
------------------------------------------------------------------------------

demoTableCfg :: TableCfg
demoTableCfg = defaultTableCfg

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
    <> " — "
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

demoPaneGridCfg :: (Ui :> es) => Bool -> PaneGridConfig es
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

demoPaneBlurb :: Word64 -> T.Text
demoPaneBlurb pid = "Contents of " <> T.pack (show pid) <> ". Drag the pane to move or split it."

-- | The header is just the pane's own content, so it is entirely optional:
-- 'showHeader' 'False' drops it and the pane becomes a bare canvas body. The
-- whole pane is still a drag handle either way ('pvDraggable'), so a headerless
-- pane can be grabbed anywhere to reorder it.
demoPaneHeader :: (Ui :> es) => Word64 -> Bool -> PaneGridCtx es -> Eff es ()
demoPaneHeader pid maximized pctx =
  panelWith (padXY 8 5 . fillW) $
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      box (fixedWH 3 16 defaultLayout) demoAccent
      void $ labelWith (tight . fontBold . fontMuted) (demoPaneTitle pid maximized)
      flex
      whenM (button "+") (void (pgcSplit pctx AxisV))
      whenM (button "=") (void (pgcSplit pctx AxisH))
      whenM (button (if maximized then "R" else "M")) (if maximized then pgcRestore pctx else pgcMaximize pctx)
      whenM (button "x") (pgcClose pctx)

demoPaneView :: (Ui :> es) => Bool -> Word64 -> PaneGridCtx es -> Eff es PaneView
demoPaneView showHeader pid pctx = do
  let maximized = pgcMaximized pctx
  columnWith (tight . gap 6 . fillW) $ do
    when showHeader (demoPaneHeader pid maximized pctx)
    box (fillW defaultLayout) demoAccent
    void $ muted (demoPaneBlurb pid)
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

debugBody :: SdlDebugSnapshot -> NanoUI ()
debugBody s =
  columnWith (tight . gap 4 . minW 300 . fillW) $ do
    debugSection "Frame" (frameRows s)
    sep
    debugSection "Draw" (drawRows s)
    sep
    debugSection "Display" (displayRows s)
    sep
    debugSection "Runtime" (rtsRows s)

debugSection :: T.Text -> SmallArray (T.Text, T.Text) -> NanoUI ()
debugSection title rows = do
  heading title
  mapM_ (\(k, v) -> kvMono k v) rows

frameRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
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

drawRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
drawRows s =
  let c = dbgCore s
   in smallArrayFromList
        [ ("verts", T.pack (printf "%10d" (dbgVerts c)))
        , ("indices", T.pack (printf "%10d" (dbgIndices c)))
        , ("cmds", T.pack (printf "%10d" (dbgCmds c)))
        ]

displayRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
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

rtsRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
rtsRows s = smallArrayFromList (formatCoreRtsRows (dbgCore s))

------------------------------------------------------------------------------
-- §10  Image pixels (RGBA, row-major, 32x32)
------------------------------------------------------------------------------

swatchPixels, checkerPixels, stripePixels :: BS.ByteString
swatchPixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , chan <-
        [ fromIntegral (x * 255 `div` 31)
        , fromIntegral (y * 255 `div` 31)
        , 180
        , 255
        ]
    ]

checkerPixels =
  BS.pack
    [ chan
    | y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , let on = (x `div` 8 + y `div` 8) `mod` 2 == 0
    , chan <-
        if on
          then [240, 200, 80, 255]
          else [40, 50, 70, 255]
    ]

stripePixels =
  BS.pack
    [ chan
    | _y <- [0 .. 31] :: [Int]
    , x <- [0 .. 31] :: [Int]
    , let on = (x `div` 4) `mod` 2 == 0
    , chan <-
        if on
          then [80, 160, 220, 255]
          else [30, 40, 60, 255]
    ]

------------------------------------------------------------------------------
-- §11  CLI plumbing
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

readMaybeFloat :: String -> Maybe Float
readMaybeFloat s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

parseBool :: String -> Bool
parseBool s = s `elem` ["true", "True", "1"]

options :: [OptDescr (DemoConfig -> DemoConfig)]
options =
  [ Option ['v'] ["vsync"] (ReqArg (\s cfg -> cfg { cfgVsync = parseBool s }) "BOOL") "Enable or disable vsync (true/false, default: true)"
  , Option ['c'] ["continuous"] (NoArg (\cfg -> cfg { cfgContinuous = True, cfgVsync = False })) "Continuous unthrottled rendering (disables vsync)"
  , Option ['b'] ["benchmark"] (NoArg (\cfg -> cfg { cfgContinuous = True, cfgVsync = False })) "Benchmark mode: continuous rendering with vsync disabled"
  , Option ['f'] ["fps"] (NoArg (\cfg -> cfg { cfgContinuous = True, cfgVsync = False })) "Show uncapped FPS (continuous, vsync false)"
  , Option ['F'] ["fullscreen"] (NoArg (\cfg -> cfg { cfgFullscreen = True })) "Launch window in fullscreen mode"
  , Option [] ["borderless"] (NoArg (\cfg -> cfg { cfgBorderless = True })) "Launch borderless window"
  , Option ['t'] ["always-on-top"] (NoArg (\cfg -> cfg { cfgAlwaysOnTop = True })) "Keep window always on top"
  , Option ['W'] ["width"] (ReqArg (\s cfg -> cfg { cfgWidth = readMaybeFloat s }) "PX") "Initial window width in pixels (default: 1280)"
  , Option ['H'] ["height"] (ReqArg (\s cfg -> cfg { cfgHeight = readMaybeFloat s }) "PX") "Initial window height in pixels (default: 800)"
  , Option ['h', '?'] ["help"] (NoArg (\cfg -> cfg { cfgHelp = True })) "Show help and command-line options"
  ]

parseArgs :: [String] -> DemoConfig
parseArgs argv =
  case getOpt Permute options argv of
    (fs, _, _) -> foldl' (flip id) defaultDemoConfig fs
