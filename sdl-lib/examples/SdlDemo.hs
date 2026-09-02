{-# LANGUAGE OverloadedStrings #-}

module SdlDemo
    ( main
    , demoImages
    , demoUi
    , DemoTab (..)
    ) where

import Control.Monad (unless, void, when)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
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
import NanoUI.Backend.Sdl (RgbaImage (..), SdlDebugSnapshot (..), askSdlDebug, SdlOptions (..), defaultSdlOptions, runSdlApp)
import NanoUI.Diagrams
import NanoUI.Testing (Context, collectOverlayTextSpans, collectTextSpans, registerImage)
import NanoUI.Backend.Sdl (SdlEnv, newSdlContext, sdlDrawFrame, syncDisplay, withSdl)
import System.Console.GetOpt
  ( ArgDescr (ReqArg)
  , ArgOrder (Permute)
  , OptDescr (Option)
  , getOpt
  )
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Read as T.Read

options :: [OptDescr Bool]
options =
  [ Option [] ["vsync"] (ReqArg parseVsync "BOOL") "Enable or disable vsync (true/false)"
  ]

parseVsync :: String -> Bool
parseVsync s = s `elem` ["true", "True", "1"]

parseArgs :: [String] -> Bool
parseArgs argv =
  case getOpt Permute options argv of
    (flags, _, _) -> last (True : flags)

main :: IO ()
main = do
  args <- getArgs
  if "--selftest" `elem` args
    then selftest
    else
      runSdlApp
        defaultSdlOptions
          { sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
          , sdlAppImages = demoImages
          , sdlAppVsync = parseArgs args
          }
        demoUi

------------------------------------------------------------------

data DemoTab
  = Controls
  | List
  | Table
  | Plots
  | Diagnostics
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Theme = Light | Dark | System deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

demoUi :: NanoUI ()
demoUi = do
  (readClick, setClick) <- useText ""
  (readAbout, setAbout) <- useFlag False
  (readDebug, setDebug) <- useFlag False
  (readChecked, setChecked) <- useFlag False
  (readVol, setVol) <- useText "50"
  (readQuality, setQuality) <- useText "Medium"
  (readAccent, setAccent) <- useText (colorPickerToHex demoAccent)
  (readTheme, setTheme) <- useText (T.pack (show Dark))
  (readName, setName) <- useText ""
  (readNotes, setNotes) <- useText ""
  (readTreeSel, setTreeSel) <- useText "0"
  (readTableSort, setTableSort) <- useTableSort (SortCol 0 SortAsc)
  debugOpen <- readDebug
  aboutOpen <- readAbout
  scroll (tight (grow defaultLayout)) $
    column (padAll 8 . gap 8 . fillW $ defaultLayout) $ do
      panel (padXY 14 10 . gap 8 . fillW $ defaultLayout) $
        toolbar $ do
          column (tight . gap 4 $ defaultLayout) $ do
            heading "nano-ui"
            muted "SDL3 demo"
          flex
          clickButton "OK" (setClick "OK")
          clickButton "Cancel" (setClick "Cancel")
          clickButton "About" (setAbout True)
          clickButton "Debug" (setDebug (not debugOpen))
      row (tight . gap 8 . wrap . fillW $ defaultLayout) $ do
        column (tight . gap 8 . fillW $ defaultLayout) $ do
          card $ do
            heading "State"
            checked <- readChecked
            vol <- readVol
            quality <- readQuality
            accentHex <- readAccent
            theme <- readTheme
            name <- readName
            notes <- readNotes
            treeSel <- readTreeSel
            tableSort <- readTableSort
            let accent = fromMaybe demoAccent (colorPickerFromHex accentHex)
            kv "Feature" (onOff checked)
            kv "Volume" vol
            kv "Quality" quality
            row (tight . gap 8 . alignMid . fillW $ defaultLayout) $ do
              box (fixedWH 20 20 defaultLayout) accent
              kv "Accent" accentHex
            kv "Theme" theme
            kv "Name" (orDash name)
            kv "Notes" (orDash notes)
            kv "Tree" treeSel
            kv "Table sort" (tableColumnLabel tableSort)
            kv "Table order" (tableSortDirText tableSort)
            click <- readClick
            kv "Clicked" (orDash click)
          card $ do
            heading "Gallery"
            row (tight . gap 10 . wrap $ defaultLayout) $ do
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
              (_, checked) <- checkbox "Feature" False
              setChecked checked
              (_, vol) <- slider "Volume" 0 100 50
              setVol (T.pack (show (round vol :: Int)))
              let qualities = ["Low", "Medium", "High"]
              (_, qualityIdx) <- select "Quality" qualities 1
              setQuality (qualities !! qualityIdx)
              (_, accent) <- colorPicker "Accent" demoAccent
              setAccent (colorPickerToHex accent)
              (_, theme) <- boundedRadioFieldset "Theme" Dark (T.pack . show)
              setTheme (T.pack (show theme))
              (_, name) <- textInput "Name" ""
              setName name
              (_, notes) <- textArea "Notes" "Edit me.\nSecond line."
              setNotes notes
              sep
              heading "Popups & Menus"
              row (tight . gap 8 . fillW $ defaultLayout) $ do
                btnTip <- button "Hover for Tooltip"
                tooltip "This is a floating tooltip widget!" btnTip
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
            List -> do
              heading "Tree"
              selTxt <- readTreeSel
              let sel0 =
                    case T.Read.decimal selTxt of
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
              scroll (padAll 6 . fixedH 240 . fillW $ defaultLayout) $ do
                (_, sel) <- tree "demo" demoTree sel0
                setTreeSel (T.pack (show sel))
            Table -> do
              heading "Table"
              muted "Click a header to sort. Drag a header to reorder."
              muted "Drag a header edge to resize. Right-click a header to hide."
              sort <- readTableSort
              (tableResp, nextSort) <-
                tableCfg
                  demoTableCfg
                  (tight . fillW . fixedH 280 $ defaultLayout {layoutGap = 0})
                  "people"
                  colPeople
                  demoPeople
                  sort
              when (tableRespChanged tableResp) (setTableSort nextSort)
              sep
              kv "Sorted by" (tableColumnLabel nextSort)
              kv "Order" (tableSortDirText nextSort)
              kv "Hidden" (tableHiddenLabel (tableHiddenIndices tableResp))
            Plots -> do
              heading "Plots"
              muted "Auto ticks, shared scales, and decimation."
              column (tight . gap 14 . fillW $ defaultLayout) $ do
                column (tight . gap 4 . fillW $ defaultLayout) $ do
                  muted "Sine + cosine"
                  void $ plot (fillW defaultLayout) sineCosineChart
                column (tight . gap 4 . fillW $ defaultLayout) $ do
                  muted "Weekly counts"
                  void $ barChart (fillW defaultLayout) weeklyBars
                column (tight . gap 4 . fillW $ defaultLayout) $ do
                  muted "Sleep vs focus"
                  void $ plot (fillW defaultLayout) sleepFocusChart
                column (tight . gap 4 . fillW $ defaultLayout) $ do
                  muted "Area"
                  void $ areaChart (fillW defaultLayout) areaDemo
                column (tight . gap 4 . fillW $ defaultLayout) $ do
                  muted "Drawing"
                  ps <- uiPlotStyle
                  void $ diagram (fillW $ defaultLayout {layoutMaxH = 200}) (drawingSample ps)
            Diagnostics -> do
              heading "Diagnostics"
              kv "Renderer" "SDL3 Pinned Vertex Arena"
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
      row (gap 6 (fillW defaultLayout)) $ do
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
            withTitle "Trig" $
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
          withTitle "Sleep vs focus" $
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

orDash :: T.Text -> T.Text
orDash s = if T.null s then "-" else s

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
debugBody s = do
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
  mapM_ (\(k, v) -> kv k (monoFontMarker <> v)) rows

clipField :: Int -> T.Text -> T.Text
clipField n s =
  if T.length s > n
    then T.take (max 0 (n - 3)) s <> "..."
    else s

frameRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
frameRows s =
  let haskellMs = dbgUiMs s + dbgRenderMs s
   in smallArrayFromList
        [ ("present", T.pack (printf "%.1f fps" (dbgPresentFps s)))
        , ("loop", T.pack (printf "%.1f fps" (dbgLoopFps s)))
        , ("frame cpu", T.pack (printf "%.2f ms" (dbgFrameMs s)))
        , ("haskell", T.pack (printf "%.2f ms" haskellMs))
        , ("  ui", T.pack (printf "%.2f ms" (dbgUiMs s)))
        , ("  render", T.pack (printf "%.2f ms" (dbgRenderMs s)))
        , ("sdl present", T.pack (printf "%.2f ms" (dbgPresentMs s)))
        , ("draws", T.pack (printf "%d" (dbgPresents s)))
        , ("skips", T.pack (printf "%d" (dbgSkips s)))
        ]

drawRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
drawRows s =
  smallArrayFromList
    [ ("verts", T.pack (printf "%d" (dbgVerts s)))
    , ("indices", T.pack (printf "%d" (dbgIndices s)))
    , ("cmds", T.pack (printf "%d" (dbgCmds s)))
    ]

displayRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
displayRows s =
  smallArrayFromList
    [ ("window", T.pack (printf "%.0fx%.0f" (dbgWinW s) (dbgWinH s)))
    , ("scale", T.pack (printf "%.2f" (dbgScale s)))
    , ("mouse", T.pack (printf "%.0f, %.0f" (dbgMouseX s) (dbgMouseY s)))
    , ( "renderer"
      , clipField 36 (dbgRenderer s <> if dbgVsync s then "  vsync on" else "  vsync off")
      )
    , ("font", clipField 36 (T.pack (dbgFontPath s)))
    ]

rtsRows :: SdlDebugSnapshot -> SmallArray (T.Text, T.Text)
rtsRows s
  | not (dbgRtsOn s) =
      smallArrayFromList
        [ ("rts", "stats off (need +RTS -T)")
        , ("haskell", T.pack (printf "%d cap / %d cpu" (dbgCaps s) (dbgCpus s)))
        ]
  | otherwise =
      smallArrayFromList
        [ ("haskell", T.pack (printf "%d cap / %d cpu" (dbgCaps s) (dbgCpus s)))
        , ("gc total", T.pack (printf "%d" (dbgGcs s)))
        , ("gc major", T.pack (printf "%d" (dbgMajorGcs s)))
        , ("last gen", T.pack (printf "%d" (dbgLastGcGen s)))
        , ("last gc", T.pack (printf "%.2f ms" (dbgLastGcMs s)))
        , ("heap live", T.pack (printf "%.1f MiB" (dbgLiveMb s)))
        , ("heap alloc", T.pack (printf "%.1f MiB" (dbgAllocMb s)))
        , ("copied", T.pack (printf "%.1f MiB" (dbgCopiedMb s)))
        , ("rss max", T.pack (printf "%.1f MiB" (dbgMaxMemMb s)))
        , ("gc time", T.pack (printf "%.1f%%" (dbgGcPct s)))
        ]

thumb :: ImageId -> T.Text -> NanoUI ()
thumb iid caption =
  column (tight . gap 6 $ defaultLayout) $ do
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
    clickTab ctx' env base "Controls"
    spansCtl <- collectTextSpans ctx'
    unless (hasText "Feature" spansCtl) $ fail "selftest: Controls missing after tab back"
    feat0 <- requireSpan "selftest: Feature checkbox" (findRightmost "Feature" spansCtl)
    clickPos ctx' env base feat0
    spansOn <- collectTextSpans ctx'
    unless (hasText "on" spansOn) $ fail "selftest: checkbox did not turn Feature on"
    light <- requireSpan "selftest: Light radio" (findExact "Light" spansOn)
    clickPos ctx' env base light
    spansTheme <- collectTextSpans ctx'
    unless (hasText "Light" spansTheme) $ fail "selftest: radio did not select Light"
    vol <- requireSpan "selftest: Volume slider" (findRightmost "Volume" spansTheme)
    clickPos ctx' env base (V2 (v2X vol + 80) (v2Y vol))
    about <- requireSpan "selftest: About button" (findExact "About" spansTheme)
    clickPos ctx' env base about
    spansModal <- collectOverlayTextSpans ctx' base
    unless (hasText "Immediate-mode" spansModal) $ fail "selftest: About modal missing"
    drawOnce ctx' env (base {inputKeys = inputKeysFromList [KeyEscape]})
    drawOnce ctx' env base
    spansClosed <- collectOverlayTextSpans ctx' base
    when (hasText "Immediate-mode" spansClosed) $ fail "selftest: Escape did not dismiss About"
    spansLatest <- collectTextSpans ctx'
    debugBtn <- requireSpan "selftest: Debug button" (findExact "Debug" spansLatest)
    clickPos ctx' env base debugBtn
    spansDebug <- collectOverlayTextSpans ctx' base
    unless (hasText "Frame" spansDebug) $ fail "selftest: Debug window missing"
  putStrLn "selftest: ok"

type DemoSpan = (Rect, T.Text, Color, Color, Rect)

spanCenter :: Rect -> V2
spanCenter (Rect x y w h) = V2 (x + w / 2) (y + h / 2)

hasText :: T.Text -> [DemoSpan] -> Bool
hasText needle = any (\(_, txt, _, _, _) -> needle `T.isInfixOf` txt)

spanLabel :: T.Text -> T.Text
spanLabel txt = T.dropWhile (`elem` ['\x01', '\x02', '\x05']) (T.strip txt)

findExact :: T.Text -> [DemoSpan] -> Maybe V2
findExact needle spans =
  pickRight
    [ (x, spanCenter r)
    | (r@(Rect x _ w h), txt, _, _, _) <- spans
    , w > 1 && h > 1
    , spanLabel txt == needle
    ]

findHeader :: T.Text -> [DemoSpan] -> Maybe V2
findHeader needle spans =
  let marked =
        [ (x, spanCenter r)
        | (r@(Rect x _ w h), txt, _, _, _) <- spans
        , w > 1 && h > 1
        , T.isPrefixOf (needle <> " ") (spanLabel txt)
        ]
      exact =
        [ (x, spanCenter r)
        | (r@(Rect x _ w h), txt, _, _, _) <- spans
        , w > 1 && h > 1
        , spanLabel txt == needle
        ]
   in pickRight (if null marked then exact else marked)

findRightmost :: T.Text -> [DemoSpan] -> Maybe V2
findRightmost needle spans =
  pickRight [(x, spanCenter r) | (r@(Rect x _ _ _), txt, _, _, _) <- spans, needle `T.isInfixOf` txt]

pickRight :: [(Float, V2)] -> Maybe V2
pickRight [] = Nothing
pickRight (p : ps) = Just (go p ps)
 where
  go acc [] = snd acc
  go acc@(ax, _) (q@(qx, _) : qs) = go (if qx >= ax then q else acc) qs

requireSpan :: String -> Maybe V2 -> IO V2
requireSpan msg = maybe (fail msg) pure

clickAt :: Input -> V2 -> (Input, Input, Input)
clickAt base pos =
  let press = base {inputMousePos = pos, inputMouseDown = True, inputMousePressed = True}
      hold = press {inputMousePressed = False}
      release = hold {inputMouseDown = False, inputMouseReleased = True}
   in (press, hold, release)

drawOnce :: Context -> SdlEnv -> Input -> IO ()
drawOnce ctx env inp = void (sdlDrawFrame ctx demoUi env inp False)

clickPos :: Context -> SdlEnv -> Input -> V2 -> IO ()
clickPos ctx env base pos = do
  let (press, hold, release) = clickAt base pos
  mapM_ (drawOnce ctx env) [press, hold, release, base, base]

clickTab :: Context -> SdlEnv -> Input -> T.Text -> IO ()
clickTab ctx env base name = do
  spans <- collectTextSpans ctx
  pos <- requireSpan ("selftest: tab " <> T.unpack name) (findExact name spans)
  clickPos ctx env base pos

dragPos :: Context -> SdlEnv -> Input -> V2 -> V2 -> IO ()
dragPos ctx env base from to = do
  let press = base {inputMousePos = from, inputMouseDown = True, inputMousePressed = True}
      hold = press {inputMousePressed = False, inputMousePos = to}
      release = hold {inputMouseDown = False, inputMouseReleased = True, inputMousePos = to}
  mapM_ (drawOnce ctx env) [press, hold, release, base, base]
