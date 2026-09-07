{-# LANGUAGE OverloadedStrings #-}

module SdlDemo
    ( main
    , demoImages
    , demoUi
    , DemoTab (..)
    , DemoTheme (..)
    ) where

import Control.Monad (unless, void, when)
import Data.Foldable (foldlM, for_)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import GHC.Clock (getMonotonicTime)
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
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Debug (CoreDebugSnapshot (..), formatCoreRtsRows)
import NanoUI.Context (ctxResolveFont, ctxResolveMeasure, startAnimation)
import NanoUI.Monad (askContext, askInput)
import NanoUI.Diagrams
import NanoUI.Testing (Context, collectOverlayTextSpans, collectTextSpans, registerImage)
import NanoUI.Testing.Harness
  ( findExact
  , findHeader
  , findRightmost
  , hasText
  , requireSpan
  )
import NanoUI.Testing.Harness qualified as Harness
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg)
  , ArgOrder (Permute)
  , OptDescr (Option)
  , getOpt
  , usageInfo
  )
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Read as T.Read
import qualified Data.Vector as V

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

parseBool :: String -> Bool
parseBool s = s `elem` ["true", "True", "1"]

parseArgs :: [String] -> DemoConfig
parseArgs argv =
  case getOpt Permute options argv of
    (fs, _, _) -> foldl' (flip id) defaultDemoConfig fs

main :: IO ()
main = do
  args <- getArgs
  if "--selftest" `elem` args
    then selftest
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

------------------------------------------------------------------

data DemoTab
  = Controls
  | Typography
  | List
  | Table
  | Plots
  | Diagnostics
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

------------------------------------------------------------------

demoImages :: SmallArray RgbaImage
demoImages =
  smallArrayFromList
    [ RgbaImage (ImageId 1) 32 32 swatchPixels
    , RgbaImage (ImageId 2) 32 32 checkerPixels
    , RgbaImage (ImageId 3) 32 32 stripePixels
    ]

demoAccent :: Color
demoAccent = colorRGBA 204 102 102 255

-- Spacing scale for demo layout
gapLayout :: Float
gapLayout = 6

gapInline :: Float
gapInline = 12

gapMicro :: Float
gapMicro = 6

gapText :: Float
gapText = 4

demoUi :: NanoUI ()
demoUi = do
  (click, setClick) <- useText ""
  (aboutOpen, setAbout) <- useFlag False
  (debugOpen, setDebug) <- useFlag False
  (checked, setChecked) <- useFlag False
  (vol, setVol) <- useText "50"
  (quality, setQuality) <- useText "Medium"
  (accentHex, setAccent) <- useText (colorPickerToHex demoAccent)
  (themeName, setThemeName) <- useText (themeDisplayName TomorrowNightMin)
  (themeRadio, setThemeRadio) <- useText (themeDisplayName ThemeDefault)
  (name, setName) <- useText ""
  (notes, setNotes) <- useText ""
  (searchQuery, setSearchQuery) <- useText ""
  (peopleMatches, setPeopleMatches) <- useState demoPeople
  (treeSel, setTreeSel) <- useText "0"
  (tableSortVal, setTableSort) <- useTableSort (SortCol 0 SortAsc)
  (sampleText, setSampleText) <- useText "The quick brown fox jumps over the lazy dog"
  (typeSize, setTypeSize) <- useFloat 20.0
  (typeBold, setTypeBold) <- useFlag False
  (typeItalic, setTypeItalic) <- useFlag True
  (typeUnderline, setTypeUnderline) <- useFlag False
  (typeStrike, setTypeStrike) <- useFlag False
  (openPath, setOpenPath) <- useText ""
  (savePath, setSavePath) <- useText ""
  (folderPath, setFolderPath) <- useText ""
  (openDlg, setOpenDlg) <- useState (Nothing :: Maybe FileDialogId)
  (saveDlg, setSaveDlg) <- useState (Nothing :: Maybe FileDialogId)
  (folderDlg, setFolderDlg) <- useState (Nothing :: Maybe FileDialogId)
  useFileDialog openDlg setOpenDlg $ \paths ->
    setOpenPath (T.intercalate ", " (map T.pack paths))
  useFileDialog saveDlg setSaveDlg $ \paths ->
    setSavePath (maybe "" T.pack (listToMaybe paths))
  useFileDialog folderDlg setFolderDlg $ \paths ->
    setFolderPath (maybe "" T.pack (listToMaybe paths))
  (dropLog, setDropLog) <- useText ""
  (dropHovering, setDropHovering) <- useFlag False
  (dropRaw, setDropRaw) <- useText ""
  rawInp <- askInput
  let rawDrop = T.intercalate " | " [T.pack (show (dropEventType ev)) <> " " <> dropEventData ev | ev <- V.toList (inputDrops rawInp)]
  when (not (T.null rawDrop)) (setDropRaw rawDrop)
  scrollWith (tight . grow) $
    columnWith (padAll 6 . gap gapLayout . fillW) $ do
      panelWith (padXY 14 10 . gap gapInline . fillW) $
        toolbar $ do
          columnWith (tight . gap gapText) $ do
            heading "nano-ui"
            muted "SDL3 demo"
          flex
          snap <- askSdlDebug
          let c = dbgCore snap
              fpsText =
                if dbgPresentFps c > 0
                  then T.pack (printf "%4.0f FPS (%5.2f ms)" (dbgPresentFps c) (dbgFrameMs c))
                  else ""
          unless (T.null fpsText) $
            void (labelEx (tight . fontMono . fontMuted $ defaultLayout) fpsText)
          clickButton "OK" (setClick "OK")
          clickButton "Cancel" (setClick "Cancel")
          clickButton "About" (setAbout True)
          clickButton "Debug" (setDebug (not debugOpen))
      responsiveRowCol 720 (tight . gap gapLayout . fillW $ defaultLayout) $ do
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
            kv "Theme radio" themeRadio
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
          card $ do
            heading "Gallery"
            rowWith (tight . gap gapInline . fillW) $ do
              thumb (ImageId 1) "Swatch"
              thumb (ImageId 2) "Checker"
              thumb (ImageId 3) "Stripe"
            sep
            muted "Click widgets or type in Name or Notes."
            muted "Esc closes About, then quits."
        card $ do
          boundedTabs Controls (T.pack . show) $ \case
            Controls -> do
              heading "Controls"
              (_, cVal) <- checkbox "Feature" False
              setChecked cVal
              (_, vVal) <- slider "Volume" 0 100 50
              setVol (T.pack (show (round vVal :: Int)))
              let qualities = ["Low", "Medium", "High"]
              (_, qualityIdx) <- select "Quality" qualities 1
              setQuality (qualities !! qualityIdx)
              (_, aVal) <- colorPicker "Accent" demoAccent
              setAccent (colorPickerToHex aVal)
              (_, tVal) <- boundedSelect "Theme" TomorrowNightMin themeDisplayName
              setThemeName (themeDisplayName tVal)
              setUiTheme (themeForChoice tVal)
              (_, trVal) <- boundedRadioFieldset "Theme (radio)" ThemeDefault themeDisplayName
              setThemeRadio (themeDisplayName trVal)
              (_, nVal) <- textInput "Name" ""
              setName nVal
              (_, notesVal) <- textArea "Notes" "Edit me.\nSecond line."
              setNotes notesVal
              sep
              heading "Popups & Menus"
              rowWith (tight . gap gapInline . fillW) $ do
                btnTip <- button "Hover for Tooltip"
                tooltip btnTip "This is a floating tooltip widget!"
                btnMenu <- button "Right-click Menu"
                void $ contextMenu btnMenu $ do
                  menuHeader "Context Menu"
                  menuSeparator
                  cut <- menuItemWithShortcut "Cut" "Ctrl+X"
                  copy <- menuItemWithShortcut "Copy" "Ctrl+C"
                  paste <- menuItemWithShortcut "Paste" "Ctrl+V"
                  menuSeparator
                  menuItemDisabled "Disabled Option"
                  when (respClicked cut) (setClick "Cut")
                  when (respClicked copy) (setClick "Copy")
                  when (respClicked paste) (setClick "Paste")
              sep
              heading "File Dialogs"
              rowWith (tight . gap gapInline . fillW) $ do
                openBtn <- button "Open File…"
                saveBtn <- button "Save File…"
                folderBtn <- button "Browse Folder…"
                when (respClicked openBtn) $ do
                  mdid <- askOpenFileDialog defaultFileDialogOptions { dialogAllowMany = True }
                  setOpenDlg mdid
                when (respClicked saveBtn) $ do
                  mdid <- askSaveFileDialog defaultFileDialogOptions
                  setSaveDlg mdid
                when (respClicked folderBtn) $ do
                  mdid <- askOpenFolderDialog defaultFileDialogOptions
                  setFolderDlg mdid
              sep
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
              sep
              heading "Progress"
              muted "A single rounded bar, smoothly oscillating 0–100%."
              ctx <- askContext
              now <- uiIO (realToFrac <$> getMonotonicTime)
              progResp <- progressBar (0.5 + 0.5 * sin (2 * pi * now / 6))
              uiIO $ startAnimation ctx (respId progResp) 0 1 1e9
            Typography -> do
              heading "Typography & Font Styling"
              muted "Font sizing, variable weights, synthetic slant, and text decorations."
              sep
              heading "Live Playground"
              rowWith (tight . gap gapInline . fillW) $ do
                (_, tVal) <- textInput "Preview text" sampleText
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
                (_, szVal) <- slider "Size" 12 40 typeSize
                setTypeSize szVal
                void $ labelEx (tight . fontMono . fontMuted $ defaultLayout) (T.pack (printf "%.0f px" szVal))
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
              sep
              heading "Weights & Styles"
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
              sep
              heading "Color & Highlights"
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
              sep
            List -> do
              heading "Tree"
              let sel0 =
                    case T.Read.decimal treeSel of
                      Right (n, _) -> n
                      Left _ -> 0
                  demoTree =
                    [ TreeItem
                        "src"
                        [ TreeItem "Main.hs" []
                        , TreeItem
                            "NanoUI"
                            [ TreeItem "Widgets.hs" []
                            , TreeItem "Frame.hs" []
                            ]
                        ]
                    , TreeItem
                        "test"
                        [ TreeItem "Main.hs" []
                        ]
                    , TreeItem "README.md" []
                    ]
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
            Table -> do
              heading "Table"
              muted "Click a header to sort. Drag a header to reorder."
              muted "Drag a header edge to resize. Right-click a header to hide."
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
            Plots -> do
              heading "Plots"
              muted "Auto ticks, shared scales, and decimation."
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
            Diagnostics -> do
              heading "Diagnostics"
              snap <- askSdlDebug
              let c = dbgCore snap
                  haskellMs = dbgUiMs c + dbgRenderMs c
              kv "Present FPS" (T.pack (printf "%.1f fps" (dbgPresentFps c)))
              kv "Loop FPS" (T.pack (printf "%.1f fps" (dbgLoopFps c)))
              kv "Frame Time" (T.pack (printf "%.2f ms" (dbgFrameMs c)))
              kv "Haskell Time" (T.pack (printf "%.2f ms (UI: %.2f, Render: %.2f)" haskellMs (dbgUiMs c) (dbgRenderMs c)))
              kv "SDL Present" (T.pack (printf "%.2f ms" (dbgPresentMs c)))
              kv "Draw Calls" (T.pack (printf "%d" (dbgCmds c)))
              kv "Vertices / Indices" (T.pack (printf "%d / %d" (dbgVerts c) (dbgIndices c)))
              kv "Renderer" (dbgRenderer snap <> if dbgVsync snap then " (vsync on)" else " (vsync off)")
              kv "Last drop event" (orDash dropRaw)
              kv "Evaluation" "Zero-Cost Inactive Tabs"
              kv "State" "SrcLoc Preserved"
  when debugOpen $ do
    snap <- askSdlDebug
    (win, _) <- window True "Debug" (debugBody snap)
    onClick win (setDebug False)
  (aboutResp, _) <-
    modal aboutOpen "About" $ do
      heading "nano-ui"
      muted "Immediate-mode GUI for Haskell."
      muted "Esc closes this dialog, then the app."
      rowWith (gap gapInline . fillW) $ do
        flex
        clickButton "Close" (setAbout False)
  onClick aboutResp (setAbout False)

sineCosineChart :: Chart
sineCosineChart =
  withDecimate True $
    withGrid GridBoth $
      withLegend LegendRight $
        withYAxis "y" $
          withXAxis "x" $
            chart
              [ line "sin(x)" [(x, sin x) | x <- [0, 0.05 .. (2 * pi)]]
              , line "cos(x)" [(x, cos x) | x <- [0, 0.05 .. (2 * pi)]]
              ]

weeklyBars :: [(T.Text, Double)]
weeklyBars =
  [ ("Mon", 2)
  , ("Tue", 5)
  , ("Wed", 4)
  , ("Thu", 7)
  , ("Fri", 3)
  ]

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

onOff :: Bool -> T.Text
onOff True = "on"
onOff False = "off"

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

orDash :: T.Text -> T.Text
orDash s = if T.null s then "-" else s

-- | First line of a multi-line log, for compact summary rows.
firstDropLine :: T.Text -> T.Text
firstDropLine = maybe "" id . listToMaybe . T.lines

data DemoPerson = DemoPerson
  { demoPersonName :: !T.Text
  , demoPersonDept :: !T.Text
  , demoPersonAge :: !Int
  , demoPersonCity :: !T.Text
  , demoPersonRole :: !T.Text
  }
  deriving (Eq, Show)

colPeople :: Colonnade Headed DemoPerson T.Text
colPeople =
  mconcat
    [ headed "Name" demoPersonName
    , headed "Dept" demoPersonDept
    , headed "Age" (T.pack . show . demoPersonAge)
    , headed "City" demoPersonCity
    , headed "Role" demoPersonRole
    ]

demoTableCfg :: TableCfg
demoTableCfg = defaultTableCfg

demoPeople :: [DemoPerson]
demoPeople =
  [ DemoPerson "David" "Eng" 63 "Austin" "Staff"
  , DemoPerson "Ava" "Design" 34 "Berlin" "Lead"
  , DemoPerson "Sonia" "Eng" 12 "Lisbon" "Intern"
  , DemoPerson "Maya" "Ops" 41 "Tokyo" "Manager"
  , DemoPerson "Leo" "Design" 28 "Paris" "IC"
  , DemoPerson "Noah" "Eng" 37 "Seoul" "Staff"
  , DemoPerson "Iris" "Ops" 19 "Austin" "IC"
  , DemoPerson "Jules" "Sales" 45 "London" "Manager"
  , DemoPerson "Priya" "Eng" 31 "Bengaluru" "Lead"
  , DemoPerson "Chen" "Design" 26 "Shanghai" "IC"
  , DemoPerson "Omar" "Ops" 52 "Cairo" "Lead"
  , DemoPerson "Elena" "Sales" 39 "Madrid" "Staff"
  , DemoPerson "Kai" "Eng" 23 "Oslo" "IC"
  , DemoPerson "Ruth" "Ops" 47 "Boston" "Staff"
  ]

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

thumb :: ImageId -> T.Text -> NanoUI ()
thumb iid caption =
  columnWith (tight . gap gapMicro) $ do
    image_ (fixedWH 88 88 defaultLayout) iid
    muted caption

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

-- Hidden SDL window: click through demoUi the same path as the interactive demo.
selftest :: IO ()
selftest = do
  ctx0 <- newSdlContext
  ok <-
    foldlM
      ( \acc img ->
          if acc
            then
              registerImage
                ctx0
                (rgbaImageId img)
                (rgbaImageWidth img)
                (rgbaImageHeight img)
                (rgbaImagePixels img)
            else pure False
      )
      True
      demoImages
  unless ok $ fail "selftest: registerImage failed"
  withSdl
    defaultSdlOptions
      { sdlWindowHidden = True
      , sdlWindowSize = Size 1280 800
      , sdlWindowResizable = False
      }
    ctx0
    $ \ctx env -> do
    (fmNorm16, _) <- ctxResolveFont ctx 16.0 WeightNormal FontStyleNormal FontRegular
    (fmItal16, _) <- ctxResolveFont ctx 16.0 WeightNormal FontStyleItalic FontRegular
    (wNorm, _) <- ctxResolveMeasure ctx 16.0 WeightNormal FontStyleNormal FontRegular "Slanted synthetic italic font style."
    (wItal, _) <- ctxResolveMeasure ctx 16.0 WeightNormal FontStyleItalic FontRegular "Slanted synthetic italic font style."
    let runNorm = lineWidth fmNorm16 "Slanted synthetic italic font style."
        runItal = lineWidth fmItal16 "Slanted synthetic italic font style."
    when (abs (runNorm - wNorm) > 0.01) $
      fail $ printf "selftest: shaped width mismatch for normal sentence: measure=%.2f, fmRun=%.2f" wNorm runNorm
    when (abs (runItal - wItal) > 0.01) $
      fail $ printf "selftest: shaped width mismatch for italic sentence: measure=%.2f, fmRun=%.2f" wItal runItal
    putStrLn $ printf "MEASURE string: norm=%.1f, ital=%.1f" wNorm wItal
    let bracketTo :: String -> IO ()
        bracketTo tag = do
          (w, _) <- ctxResolveMeasure ctx 20.0 WeightNormal FontStyleNormal FontRegular "To"
          putStrLn $ printf "  [bracket %s] width(To)@20 = %.1f" tag w
    bracketTo "start"
    -- Verify the shaped run path (fmRun / pushText) matches SDL3_ttf string
    -- measurement.  This catches regressions where per-glyph fallback would
    -- ignore GPOS kerning for pairs like To, AV, and fi.
    (fmNorm20, _) <- ctxResolveFont ctx 20.0 WeightNormal FontStyleNormal FontRegular
    (fmItal20, _) <- ctxResolveFont ctx 20.0 WeightNormal FontStyleItalic FontRegular
    let checkRun :: String -> FontMetrics -> FontStyle -> String -> IO ()
        checkRun tag fm st pair = do
          (wab, _) <- ctxResolveMeasure ctx 20.0 WeightNormal st FontRegular (T.pack pair)
          let runW = lineWidth fm (T.pack pair)
          when (abs (runW - wab) > 0.01) $
            fail $ printf "selftest: %s shaped width mismatch for '%s': measure=%.2f, fmRun=%.2f" tag pair wab runW
    checkRun "norm" fmNorm20 FontStyleNormal "To"
    checkRun "ital" fmItal20 FontStyleItalic "To"
    checkRun "norm" fmNorm20 FontStyleNormal "AV"
    checkRun "ital" fmItal20 FontStyleItalic "AV"
    checkRun "norm" fmNorm20 FontStyleNormal "fi"
    checkRun "ital" fmItal20 FontStyleItalic "fi"
    let sentence = "The quick brown fox jumps over the lazy dog"
    putStrLn "--- Kerning queries (Normal vs Italic) ---"
    let pairs = zip (T.unpack sentence) (drop 1 (T.unpack sentence))
    for_ pairs $ \(c1, c2) -> do
      kN <- queryFontKerning env 20.0 WeightNormal FontStyleNormal FontRegular c1 c2
      kI <- queryFontKerning env 20.0 WeightNormal FontStyleItalic FontRegular c1 c2
      when (kN /= 0 || kI /= 0) $
        putStrLn $ printf "Kerning '%c''%c': norm=%d, ital=%d" c1 c2 kN kI
    putStrLn "--- Pair width probes (string-level GPOS kerning) ---"
    let kernPairs = [('T', 'o'), ('W', 'e'), ('A', 'V'), ('T', 'a'), ('f', 'i'), ('r', 'y'), ('l', 'y'), ('F', 'o')]
    for_ kernPairs $ \(a, b) ->
      for_ [(FontStyleNormal, "norm" :: String), (FontStyleItalic, "ital")] $ \(st, tag) -> do
        (wa, _) <- ctxResolveMeasure ctx 20.0 WeightNormal st FontRegular (T.singleton a)
        (wb, _) <- ctxResolveMeasure ctx 20.0 WeightNormal st FontRegular (T.singleton b)
        (wab, _) <- ctxResolveMeasure ctx 20.0 WeightNormal st FontRegular (T.pack [a, b])
        putStrLn $ printf "width(%c)=%5.1f width(%c)=%5.1f width(%c%c)=%5.1f kern=%+5.1f [%s]"
          a wa b wb a b wab (wab - wa - wb) tag
    bracketTo "after width probes"
    putStrLn "--- Shaped pair kerning (40pt raw px) ---"
    let probePairs = [('r', ' '), (' ', 't'), ('e', ' '), (' ', 'l'), ('o', 'v'), ('v', 'e'), ('r', 't'), ('T', 'o'), ('A', 'V'), ('W', 'e'), ('P', 'a'), (' ', 'T'), ('y', ' '), ('f', 'i')]
    for_ probePairs $ \(a, b) -> do
      k <- queryFontPairKerning env 40.0 WeightNormal FontStyleNormal FontRegular a b
      putStrLn $ printf "  pairKern('%c',''%c') = %d" a b k
    bracketTo "after pairKern"
    putStrLn "--- Debug pair internals ---"
    for_ [16.0, 20.0, 24.0, 32.0, 40.0, 64.0] $ \sz -> do
      putStrLn $ printf "size %.0f:" sz
      debugFontPair env sz WeightNormal FontStyleNormal FontRegular 'T' 'o'
    putStrLn "--- Shaped layout dump ---"
    dumpFontLayout env 40.0 WeightNormal FontStyleNormal FontRegular "r the ovt"
    void $ saveFontRenderText env 20.0 WeightNormal FontStyleItalic FontRegular sentence
      "C:\\Users\\zach\\.gemini\\antigravity\\brain\\72382fd0-e1b2-4a85-8ac3-abd001b9f58d\\sdl_native_italic.bmp"
    let idle =
          emptyInput
            { inputWindowSize = Size 1280 800
            , inputMousePos = V2 640 400
            }
    (ctx', base) <- syncDisplay ctx env idle
    void (sdlDrawFrame ctx' demoUi env base True)
    spans0 <- collectTextSpans ctx'
    unless (hasText "Feature" spans0) $ fail "selftest: Controls body missing"
    clickTab ctx' env base "Table"
    spansTable <- collectTextSpans ctx'
    unless (hasText "David" spansTable) $ fail "selftest: table body missing after Table tab"
    hdr <- requireSpan "selftest: Name header" (findHeader "Name" spansTable)
    clickPos ctx' env base hdr
    spansSorted <- collectTextSpans ctx'
    unless (hasText "descending" spansSorted) $ fail "selftest: header click did not toggle sort"
    dept <- requireSpan "selftest: Dept header" (findHeader "Dept" spansSorted)
    dragPos ctx' env base dept (V2 (v2X dept + 180) (v2Y dept))
    spansDrag <- collectTextSpans ctx'
    unless (hasText "Sonia" spansDrag) $ fail "selftest: table missing after header drag"
    clickTab ctx' env base "List"
    spansTree <- collectTextSpans ctx'
    unless (hasText "src" spansTree) $ fail "selftest: tree missing after List tab"
    readme <- requireSpan "selftest: README.md" (findExact "README.md" spansTree)
    clickPos ctx' env base readme
    spansSel <- collectTextSpans ctx'
    unless (hasText "7" spansSel) $ fail "selftest: tree click did not select README.md"
    clickTab ctx' env base "Typography"
    spansType <- collectTextSpans ctx'
    unless (hasText "Live Playground" spansType) $ fail "selftest: typography missing after Typography tab"
    drawOnce ctx' env (base {inputScroll = V2 0 (-350)})
    drawOnce ctx' env base
    void $ saveScreenshot env "C:\\Users\\zach\\.gemini\\antigravity\\brain\\72382fd0-e1b2-4a85-8ac3-abd001b9f58d\\typography_styles.bmp"
    sizeSpan <- requireSpan "selftest: Size slider" (findRightmost "Size" spansType)
    for_ [20, 60, 100, 140, 180, 50, 120, -60, -100, 0 :: Float] $ \dx -> do
      dragPos ctx' env base sizeSpan (V2 (v2X sizeSpan + dx) (v2Y sizeSpan))
    spansTypeAfter <- collectTextSpans ctx'
    unless (hasText "Live Playground" spansTypeAfter) $ fail "selftest: typography missing after size changes"
    clickTab ctx' env base "Controls"
    spansCtl <- collectTextSpans ctx'
    unless (hasText "Feature" spansCtl) $ fail "selftest: Controls missing after tab back"
    feat0 <- requireSpan "selftest: Feature checkbox" (findRightmost "Feature" spansCtl)
    clickPos ctx' env base feat0
    spansOn <- collectTextSpans ctx'
    unless (hasText "on" spansOn) $ fail "selftest: checkbox did not turn Feature on"
    clickPos ctx' env base feat0
    spansOff <- collectTextSpans ctx'
    unless (hasText "off" spansOff) $ fail "selftest: checkbox did not turn Feature off"
    clickPos ctx' env base feat0
    spansOn2 <- collectTextSpans ctx'
    unless (hasText "on" spansOn2) $ fail "selftest: checkbox did not turn Feature on again"
    themeBtn <- requireSpan "selftest: Theme select" (findRightmost "Theme" spansOn2)
    clickPos ctx' env base themeBtn
    spansOverlay <- collectOverlayTextSpans ctx' base
    lightOpt <- requireSpan "selftest: Tomorrow Light option" (findExact "Tomorrow Light" spansOverlay)
    clickPos ctx' env base lightOpt
    spansTheme <- collectTextSpans ctx'
    unless (hasText "Tomorrow Light" spansTheme) $ fail "selftest: select did not pick Tomorrow Light"
    th <- getTheme ctx'
    unless (th == tomorrowMinLightTheme) $ fail "selftest: context theme was not updated to Tomorrow Light"
    vol <- requireSpan "selftest: Volume slider" (findRightmost "Volume" spansTheme)
    clickPos ctx' env base (V2 (v2X vol + 80) (v2Y vol))
    about <- requireSpan "selftest: About button" (findExact "About" spansTheme)
    clickPos ctx' env base about
    spansModal <- collectOverlayTextSpans ctx' base
    unless (hasText "Immediate-mode" spansModal) $ fail "selftest: About modal missing"
    unless (hasText "Close" spansModal) $ fail "selftest: About Close button missing"
    drawOnce ctx' env (base {inputKeys = inputKeysFromList [KeyEscape]})
    drawOnce ctx' env base
    spansClosed <- collectOverlayTextSpans ctx' base
    when (hasText "Immediate-mode" spansClosed) $ fail "selftest: Escape did not dismiss About"
    spansLatest <- collectTextSpans ctx'
    debugBtn <- requireSpan "selftest: Debug button" (findExact "Debug" spansLatest)
    clickPos ctx' env base debugBtn
    spansDebug <- collectOverlayTextSpans ctx' base
    unless (hasText "Frame" spansDebug) $ fail "selftest: Debug window missing"
    unless (hasText "Runtime" spansDebug) $ fail "selftest: Debug Runtime section missing"
  putStrLn "selftest: ok"


drawOnce :: Context -> SdlEnv -> Input -> IO ()
drawOnce ctx env inp = void (sdlDrawFrame ctx demoUi env inp False)

clickPos :: Context -> SdlEnv -> Input -> V2 -> IO ()
clickPos ctx env = Harness.clickPos (drawOnce ctx env)

clickTab :: Context -> SdlEnv -> Input -> T.Text -> IO ()
clickTab ctx env = Harness.clickTab collectTextSpans (drawOnce ctx env) ctx

dragPos :: Context -> SdlEnv -> Input -> V2 -> V2 -> IO ()
dragPos ctx env = Harness.dragPos (drawOnce ctx env)
