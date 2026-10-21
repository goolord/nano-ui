{-# LANGUAGE RecordWildCards #-}

-- | The nano-ui widget cookbook: one tab per widget family. Each tab is a
-- function below that keeps its state next to the widgets using it. For
-- short programs about one topic each (state, layout, forms, shortcuts,
-- background work, ...), see this package's @examples@ directory.
--
-- Input widgets mutate a 'Var' in place: @slider 0 100 volume@ draws and
-- updates @volume@, and @readVar volume@ reads its current value. 'useVar'
-- allocates call-site-scoped mutable state inside the view; larger or shared
-- resources (a list, an image handle, a plot cache) are allocated once in
-- 'newDemo'. The values shown in the State pane are declared once above both
-- the pane and the tabs so they share the same 'Var's.
--
-- The toolbar opens an About modal (F1) and a Debug window (F12) with the
-- SDL backend's frame statistics and the layout overlay; the Diagnostics tab
-- shows the headline numbers. The @nano-ui-demo-test@ suite drives this demo.
module Demo
  ( main
  , DemoState
  , newDemo
  , demoUi
  ) where

import Codec.Picture qualified as JP
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, unless, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Diagrams.Prelude (Diagram, circle, fc, fromVertices, lc, lw, lwO, none, p2, (#))
import NanoUI
import NanoUI.Adornment qualified as A
import NanoUI.Backend.Sdl
import NanoUI.Diagrams
import NanoUI.Path qualified as P
import NanoUI.Shortcut
import Paths_nano_ui_demo (getDataFileName)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (Permute), OptDescr (..), getOpt, usageInfo)
import System.Environment (getArgs, lookupEnv)
import Text.Printf (printf)
import Text.Read (readMaybe)

import DemoData
import SdlRecord qualified

-- Running ---------------------------------------------------------------------

-- | Run @cabal run nano-ui-sdl-demo -- --help@ for the options. @--record DIR@
-- records the README video's frames instead (see "SdlRecord").
main :: IO ()
main = do
  args <- getArgs
  case dropWhile (/= "--record") args of
    _ : dir : _ -> SdlRecord.record dir . demoUi =<< newDemo
    _ -> do
      let (updates, _, _) = getOpt Permute options args
      case sequence updates of
        Nothing -> putStr (usageInfo "Usage: nano-ui-sdl-demo [OPTIONS]" options)
        Just fs -> runSdlApp (foldl' (flip id) demoOptions fs) . demoUi =<< newDemo

demoOptions :: SdlOptions
demoOptions =
  defaultSdlOptions
    { sdlAppShouldQuit = pressedOnceIn KeyEscape
    , sdlAppTheme = Just defaultTheme
    , sdlWindowSettings = defaultWindowSettings {wsSize = Size 1280 800, wsMinSize = Just (Size 480 360)}
    }

-- | Each option's change to 'demoOptions'; 'Nothing' asks for the help text.
options :: [OptDescr (Maybe (SdlOptions -> SdlOptions))]
options =
  [ Option "v" ["vsync"] (ReqArg (\s -> Just (\o -> o {sdlAppVsync = s `elem` ["true", "True", "1"]})) "BOOL")
      "Turn vsync on or off (default: true)"
  , Option "cbf" ["continuous", "benchmark", "fps"] (NoArg (Just (\o -> o {sdlAppContinuous = True, sdlAppVsync = False})))
      "Render continuously without vsync, to show uncapped FPS"
  , Option "" ["explain"] (NoArg (Just (\o -> o {sdlExplainLayout = True})))
      "Start with the layout overlay on"
  , Option "F" ["fullscreen"] (NoArg (Just (onWindow (\w -> w {wsMode = Fullscreen}))))
      "Start fullscreen"
  , Option "W" ["width"] (ReqArg (resize (\px sz -> sz {sizeW = px})) "PX") "Initial window width (default: 1280)"
  , Option "H" ["height"] (ReqArg (resize (\px sz -> sz {sizeH = px})) "PX") "Initial window height (default: 800)"
  , Option "h?" ["help"] (NoArg Nothing) "Show this help"
  ]
  where
    onWindow f o = o {sdlWindowSettings = f (sdlWindowSettings o)}
    resize f s = Just (maybe id (\px -> onWindow (\w -> w {wsSize = f px (wsSize w)})) (readMaybe s))

-- State -----------------------------------------------------------------------

-- | What the tabs keep beyond their positional Vars, made once by 'newDemo'.
data DemoState = DemoState
  { demoAccentColor :: !(Var Color)
  , demoOpenDialog :: !(Var (Maybe FileDialogId))
  , demoDocs :: !(Var [Int])
  , demoIcons :: !(Either String Icons)
  , demoGifTask :: !(Task () (Either String [GifFrame]))
  , demoSwatchImages :: ![ImageHandle Text]
  , demoLandscapeImage :: !(ImageHandle Text)
  , demoPlotCache :: !PlotCache
  , demoFontFamilies :: ![Text]
  , demoDebugAtStart :: !Bool
  -- ^ @NANO_DEBUG_OPEN@ opens the Debug window at start.
  }

data Icons = Icons {iconClock, iconCheck, iconStar, iconFace :: !Svg}

-- | One frame of an animated GIF: its image handle, size and RGBA pixels.
data GifFrame = GifFrame !(ImageHandle ()) !Int !Int !BS.ByteString

-- | Make a demo's state. Call it once, before the session, and draw it with
-- 'demoUi'.
newDemo :: IO DemoState
newDemo = do
  demoAccentColor <- newVar (colorRGBA 204 102 102 255)
  demoOpenDialog <- newVar Nothing
  demoDocs <- newVar [1, 2, 3]
  demoIcons <- loadIcons
  demoGifTask <- newTask
  demoSwatchImages <- mapM (const newImageHandle) demoSwatches
  demoLandscapeImage <- newImageHandle
  demoPlotCache <- newPlotCache
  families <- map T.pack <$> listFontFamilies
  let demoFontFamilies = if null families then ["Inter"] else families
  demoDebugAtStart <- isJust <$> lookupEnv "NANO_DEBUG_OPEN"
  pure DemoState {..}

loadIcons :: IO (Either String Icons)
loadIcons = do
  svgs <- mapM (\name -> loadSvg =<< getDataFileName ("data/icons/" <> name <> ".svg")) ["clock", "check", "star", "face"]
  pure $ case sequence svgs of
    Right [clock, check, star, face] -> Right (Icons clock check star face)
    Right _ -> Left "expected four icons"
    Left err -> Left err

-- | Decode a GIF with JuicyPixels into its frames, each with an image handle
-- of its own, or an error message. It runs on a task's worker thread, so
-- the pixels are decoded there and registering them only copies.
loadGif :: FilePath -> IO (Either String [GifFrame])
loadGif path = do
  result <- try $ do
    images <- either fail pure . JP.decodeGifImages =<< BS.readFile path
    when (null images) (fail "the file has no frames")
    forM images $ \img -> do
      let rgba = JP.convertRGBA8 img
          (fp, n) = VS.unsafeToForeignPtr0 (JP.imageData rgba)
      pixels <- evaluate (BSI.fromForeignPtr0 fp n)
      owner <- newImageHandle
      pure (GifFrame owner (JP.imageWidth rgba) (JP.imageHeight rgba) pixels)
  pure (either (\e -> Left (displayException (e :: SomeException))) Right result)

-- The shell -------------------------------------------------------------------

data DemoTab = Controls | Graphics | Typography | List | Table | Tabs | Panes | Plots | Diagnostics
  deriving (Bounded, Enum, Show)

-- | The values the tabs edit and the State pane shows.
data Values = Values
  { valMuted :: !(Var Bool)
  , valVolume :: !(Var Float)
  , valQuality :: !(Var Int)
  , valTheme :: !(Var Int)
  , valFont, valName, valNotes :: !(Var Text)
  , valCount, valMask :: !(Var Double)
  , valPicked, valOpened, valDropped :: !(Var Text)
  , valTreeItem :: !(Var Int)
  , valQuery :: !(Var Text)
  -- ^ The search, committed after a pause in typing.
  , valSort :: !(Var SortCol)
  }

useValues :: NanoUI Values
useValues = do
  valMuted <- useVar False
  valVolume <- useVar 50
  valQuality <- useVar 1
  valTheme <- useVar 0
  valFont <- useVar "Inter"
  valName <- useVar ""
  valNotes <- useVar "Edit me.\nSecond line."
  valCount <- useVar 12
  valMask <- useVar 0xC0FF
  valPicked <- useVar ""
  valOpened <- useVar ""
  valDropped <- useVar ""
  valTreeItem <- useVar 0
  valQuery <- useVar ""
  valSort <- useVar (SortCol 0 SortAsc)
  pure Values {..}

-- | The whole app: a toolbar, the State pane beside the tabbed card (above
-- it in a narrow window), the Debug window and the About modal.
demoUi :: DemoState -> NanoUI ()
demoUi st = do
  aboutOpen <- useVar False
  debugOpen <- useVar (demoDebugAtStart st)
  activeTab <- useEnumVar Controls
  vals <- useValues
  wide <- (>= 1000) <$> windowWidth
  scrollWith (padAll 10 . grow) $
    columnWith (tight . gap 10 . fillW) $ do
      panelWith (padXY 16 12 . fillW) $
        rowWith (tight . gap 12 . alignMid . fillW) $ do
          labelWith (tight . alignMid . fontMedium . fontSize 22) "nano-ui"
          labelWith (tight . alignMid . fontMuted) "SDL3 / Widget cookbook"
          flex
          -- F1 and F12 do what the buttons do.
          about <- button "About"
          f1 <- shortcut (key (KeyF 1))
          when (about || f1) (writeVar aboutOpen True)
          debug <- button "Debug"
          f12 <- shortcut (key (KeyF 12))
          when (debug || f12) (modifyVar debugOpen not)
      responsiveRowCol 1000 (tight . gap 10 . fillW) $ do
        panelWith (padAll 16 . gap 8 . if wide then fixedW 280 else fillW) $
          statePane st vals wide
        -- Only the selected tab's body runs.
        panelWith (padAll 16 . gap 12 . fillW) $
          enumTabs activeTab (tabView st vals)
  window debugOpen "Debug" debugBody
  modal aboutOpen "About" $ do
    heading "nano-ui"
    muted "Immediate-mode GUI for Haskell."
    muted "Esc closes this dialog, then the app."
    rowWith (gap 12 . fillW) $ do
      flex
      whenM (button "Close") (writeVar aboutOpen False)

tabView :: DemoState -> Values -> DemoTab -> NanoUI ()
tabView st vals = \case
  Controls -> controlsTab st vals
  Graphics -> graphicsTab st
  Typography -> typographyTab
  List -> listTab vals
  Table -> tableTab (valSort vals)
  Tabs -> tabsTab (demoDocs st)
  Panes -> panesTab
  Plots -> plotsTab (demoPlotCache st)
  Diagnostics -> diagnosticsTab

-- | Every value the tabs edit, read where it is shown: edit a control and its
-- row changes on the same frame. A narrow window folds it behind a button.
statePane :: DemoState -> Values -> Bool -> NanoUI ()
statePane st Values {..} wide = do
  open <- useVar False
  shown <- (wide ||) <$> readVar open
  rowWith (tight . gap 12 . alignMid . fillW) $ do
    labelWith (tight . alignMid . fontMedium) "State"
    flex
    unless wide $
      whenM (button (if shown then "Hide values" else "Show values")) (modifyVar open not)
  when shown $ do
    let value name render var = kv name . render =<< readVar var
    muted "Live widget values"
    separator
    value "Mute" (\m -> if m then "on" else "off") valMuted
    value "Volume" (T.pack . show . (round :: Float -> Int)) valVolume
    value "Quality" (qualities !!) valQuality
    -- The swatch sits beside the value, so the key lines up with the rows.
    rowWith (tight . gap 4 . alignMid . fillW) $ do
      accent <- readVar (demoAccentColor st)
      labelWith (tight . alignMid . fontMuted . minW 88) "Accent"
      flex
      box (alignMid . fixedWH 14 14) accent
      labelWith (tight . alignMid) (colorToHexA accent)
    separator
    value "Theme" (fst . (themes !!)) valTheme
    value "Font" brief valFont
    value "Name" brief valName
    value "Notes" brief valNotes
    value "Count" (T.pack . show . (round :: Double -> Int)) valCount
    value "Mask" (T.pack . printf "0x%04X" . (round :: Double -> Int)) valMask
    separator
    value "Picked" brief valPicked
    value "Opened" brief valOpened
    value "Dropped" brief valDropped
    separator
    value "Tree item" (T.pack . show) valTreeItem
    value "Search" brief valQuery
    value "Table sort" (\(SortCol col dir) -> fromMaybe "-" (columnName col) <> if dir == SortAsc then " (asc)" else " (desc)") valSort
    separator
    muted "Edit a control to see its value here."
  where
    -- The first line of a text, short enough for the pane, or a dash.
    brief t = maybe "-" (T.take 28) (listToMaybe (filter (not . T.null) (T.lines t)))

-- | A caption over a widget, as a form field has.
field :: Text -> NanoUI a -> NanoUI a
field caption widget =
  columnWith (tight . gap 4 . fillW) $ do
    labelWith (tight . fontMuted . fillW) caption
    widget

-- | A caption over a chart or drawing.
figure :: Text -> NanoUI a -> NanoUI ()
figure caption = void . field caption

-- | A constructor's name without its three-letter prefix: @TabTop@ is "Top".
enumLabel :: Show a => a -> Text
enumLabel = T.drop 3 . T.pack . show

-- Controls --------------------------------------------------------------------

controlsTab :: DemoState -> Values -> NanoUI ()
controlsTab st vals@Values {..} = do
  responsiveRowCol 760 (tight . gap 24 . fillW) $ do
    columnWith (tight . gap 10 . fillW) $ do
      basicControls vals
      separator
      heading "Text input"
      field "Name" (textInputConfigured defaultTextInputConfig {ticPlaceholder = "Enter name"} valName)
      field "Notes" (textArea valNotes)
      separator
      -- Arrow keys or the stepper step the value; Shift steps by ten.
      heading "Numbers"
      rowWith (tight . gap 12 . fillW) $ do
        field "Count (0-100)" (numericInputConfigured defaultNumericInputConfig {nicMin = 0, nicMax = 100} valCount)
        field "Mask (hex)" (numericInputConfigured defaultNumericInputConfig {nicMin = 0, nicMax = 0xFFFF, nicHex = True} valMask)
    columnWith (tight . gap 10 . fillW) (appearanceControls st valTheme valFont)
  separator
  popupsAndMenus valPicked
  separator
  fileDialog (demoOpenDialog st) valOpened
  separator
  dragAndDrop valDropped

qualities :: [Text]
qualities = ["Low", "Medium", "High"]

basicControls :: Values -> NanoUI ()
basicControls Values {..} = do
  heading "Controls"
  checkbox "Mute" valMuted
  mute <- readVar valMuted
  columnWith (tight . gap 4 . fillW) $ do
    vol <- readVar valVolume
    kv "Volume" (if mute then "muted" else T.pack (show (round vol :: Int)))
    -- A disabled widget keeps its layout and value but takes no input.
    disabledWhen mute (slider 0 100 valVolume)
  field "Quality" (selectWith fillW qualities valQuality)

-- | Themes by name, each with how to apply it. Setting the theme on every
-- frame costs nothing while it stays the same.
themes :: [(Text, NanoUI ())]
themes =
  [ ("Default", setUiTheme defaultTheme)
  , ("Tomorrow Night Min", setUiTheme tomorrowNightMinDarkTheme)
  , ("Tomorrow Light", setUiTheme tomorrowMinLightTheme)
  , ("Tomorrow at Midnight Min", setUiTheme tomorrowMidnightMinDarkTheme)
  , ("Follow system", setUiTheme . defaultThemeFor =<< systemAppearance)
  ]

appearanceControls :: DemoState -> Var Int -> Var Text -> NanoUI ()
appearanceControls st theme font = do
  heading "Appearance"
  field "Theme" (radio (map fst themes) theme)
  snd . (themes !!) =<< readVar theme
  fontResp <- field "Font" (comboBox' "Font" (demoFontFamilies st) font)
  tooltip fontResp "Type to filter; Enter applies, Esc reverts."
  when (respChanged fontResp) $ do
    family <- readVar font
    unless (T.null family) (setSdlUiFont (FontSearch [T.unpack family]))
  separator
  heading "Accent"
  muted "Choose a color or enter an exact value."
  colorPickerRGBA (demoAccentColor st)

-- | Tooltips, and a context menu whose rows act like buttons.
popupsAndMenus :: Var Text -> NanoUI ()
popupsAndMenus picked = do
  heading "Popups & Menus"
  rowWith (tight . gap 12 . alignMid . fillW) $ do
    tip <- button' "Hover for Tooltip"
    tooltip tip "This is a floating tooltip widget!"
    follow <- button' "Tooltip at Pointer"
    tooltipConfigured defaultTooltipConfig {tooltipPlacement = PlacementAtCursor} follow "This one follows the pointer."
    menuBtn <- button' "Right-click Menu"
    void $ contextMenu menuBtn $ do
      menuHeader "Context Menu"
      menuSeparator
      whenM (menuItemShortcut "Cut" (ctrl <> key 'x')) (writeVar picked "Cut")
      whenM (menuItemShortcut "Copy" (ctrl <> key 'c')) (writeVar picked "Copy")
      whenM (menuItemShortcut "Paste" (ctrl <> key 'v')) (writeVar picked "Paste")
      menuSeparator
      menuItemDisabled "Disabled Option"
    choice <- readVar picked
    unless (T.null choice) (muted ("Picked " <> choice))

-- | A native file dialog runs outside the view: ask for one, keep its handle,
-- and 'useFileDialog' polls it until it answers.
fileDialog :: Var (Maybe FileDialogId) -> Var Text -> NanoUI ()
fileDialog dialog opened = do
  useFileDialog dialog (writeVar opened . T.intercalate ", " . map T.pack)
  heading "File Dialogs"
  whenM (button "Open File…") $
    writeVar dialog . Just =<< askOpenFileDialog defaultFileDialogOptions {dialogAllowMany = True}
  paths <- readVar opened
  kv "Opened" (if T.null paths then "-" else paths)

-- | A drop zone reports the files and text dropped on it.
dragAndDrop :: Var Text -> NanoUI ()
dragAndDrop dropped = do
  heading "Drag & Drop"
  (_, _, target) <- dropZone (padXY 16 12 . gap 4 . fillW) $ do
    muted "Drop a file or highlighted text from another app here."
    received <- readVar dropped
    unless (T.null received) (labelWith (tight . fontMono . fillW) received)
  when (dropReceived target) $
    writeVar dropped . T.intercalate "\n" $
      map ("file: " <>) (dropFiles target) <> map ("text: " <>) (dropTexts target)

-- Graphics --------------------------------------------------------------------

graphicsTab :: DemoState -> NanoUI ()
graphicsTab st = do
  heading "Graphics"
  separator
  swatchGallery st
  separator
  imageFits (demoLandscapeImage st)
  separator
  iconsAndAdornments (demoIcons st)
  separator
  animatedGif (demoGifTask st)
  separator
  pulsingBar

-- | Generated RGBA images, registered only while this tab shows, with a
-- badge layered on each one's corner.
swatchGallery :: DemoState -> NanoUI ()
swatchGallery st =
  rowWith (wrap . lineAlign LinesCenter . tight . gap 12 . fillW) $
    for_ (zip (demoSwatchImages st) demoSwatches) $ \(owner, (caption, pixels)) -> do
      registered <- useImageRgba owner caption 32 32 pixels
      for_ registered $ \iid ->
        columnWith (tight . gap 6) $ do
          layersWith tight $ do
            image (fixedWH 88 88) iid
            panelWith (alignEnd . alignTop . padXY 4 1) $
              labelWith (tight . fontMono . fontSize 11) "32px"
          muted caption

-- | A wide image in each content fit, cropped, then turned, faded and zoomed
-- by sliders, then filling the width at its own aspect.
imageFits :: ImageHandle Text -> NanoUI ()
imageFits owner = do
  angleVar <- useVar (30 :: Float) -- degrees
  fadeVar <- useVar (1 :: Float)
  scaleVar <- useVar (1 :: Float)
  landscape <- useImageRgba owner "landscape" 96 48 demoLandscape
  -- The scope takes one id whether or not the image is registered.
  scope . for_ landscape $ \iid -> columnWith (tight . gap 4 . fillW) $ do
    let thumb cfg caption = columnWith (tight . gap 6) $ do
          imageConfigured cfg {icLayout = fixedWH 72 72} iid
          muted caption
    rowWith (wrap . tight . gap 12 . fillW) $ do
      for_ [minBound .. maxBound] $ \fit -> thumb defaultImageConfig {icFit = fit} (enumLabel fit)
      thumb defaultImageConfig {icFit = FitCover, icCrop = Just (Rect 0 0 48 48)} "Crop"
    rowWith (tight . gap 12 . alignMid . fillW) $ do
      turn <- readVar angleVar
      fade <- readVar fadeVar
      zoom <- readVar scaleVar
      imageConfigured
        defaultImageConfig
          { icLayout = fixedWH 96 96
          , icFit = FitContain
          , icRotation = RotateSolid (turn * pi / 180)
          , icOpacity = fade
          , icScale = zoom
          }
        iid
      columnWith (tight . gap 4 . fillW) $ do
        kv "Turn" (T.pack (printf "%.0f deg" turn))
        slider 0 360 angleVar
        kv "Opacity" (T.pack (printf "%.2f" fade))
        slider 0 1 fadeVar
        kv "Zoom" (T.pack (printf "%.2fx" zoom))
        slider 0.5 3 scaleVar
    imageConfigured defaultImageConfig {icLayout = fillW . maxW 360} iid

-- | SVG icons, then adornments: icons, short texts and views drawn inside a
-- button beside its label, or inside a field before or after its value. A
-- press on one is a press on the widget, except on a control, which takes
-- its own.
iconsAndAdornments :: Either String Icons -> NanoUI ()
iconsAndAdornments (Left err) = muted ("Could not load the icons: " <> T.pack err)
iconsAndAdornments (Right Icons {..}) = do
  saving <- useVar False
  weight <- useVar ""
  secret <- useVar ""
  secretShown <- useVar False
  -- A one-colour icon takes the text colour, or a fontColor.
  tint <- themeAccent <$> uiTheme
  rowWith (tight . gap 12 . alignMid) $
    for_ [iconClock, iconCheck, iconStar, iconFace] $ \icon -> do
      svgIcon 20 icon
      svgIconWith (fixedWH 32 32 . fontColor tint) icon
  rowWith (tight . gap 12 . alignMid) $ do
    -- Any view can be a button's content; a spinner animates while shown.
    isSaving <- readVar saving
    whenM (buttonContent (if isSaving then spinnerWith' id 14 >> label "Saving" else svgIcon 16 iconCheck >> label "Save")) $
      modifyVar saving not
    void (buttonConfigured defaultButtonConfig {bcAdornments = A.trailing (A.icon iconStar)} "Star")
    void (iconButton iconClock "")
    textInputConfigured
      defaultTextInputConfig
        { ticPlaceholder = "Weight"
        , ticAdornments = A.leading (A.icon iconClock) <> A.trailing (A.affix "kg")
        , ticLayout = fixedW 200 (ticLayout defaultTextInputConfig)
        }
      weight
    -- A control: the Show button takes its own presses, and the field keeps
    -- its focus and caret.
    shown <- readVar secretShown
    textInputConfigured
      defaultTextInputConfig
        { ticPlaceholder = "Password"
        , ticPassword = not shown
        , ticAdornments = A.trailing (A.control (whenM (buttonWith tight (if shown then "Hide" else "Show")) (modifyVar secretShown not)))
        , ticLayout = fixedW 220 (ticLayout defaultTextInputConfig)
        }
      secret

-- | An animated GIF, decoded on a worker thread ('useTask') while this tab
-- shows. Each frame is its own image; the clock picks one every 100 ms, and
-- the view wakes for the next one only while the GIF is on screen.
animatedGif :: Task () (Either String [GifFrame]) -> NanoUI ()
animatedGif task = do
  decoded <- useTask task () (loadGif =<< getDataFileName "data/lick.gif")
  scope $ case decoded of
    Nothing -> muted "Loading lick.gif..."
    Just (Left err) -> muted ("Could not load lick.gif: " <> T.pack err)
    Just (Right gif) -> do
      registered <- mapM (\(GifFrame owner w h pixels) -> useImageRgba owner () w h pixels) gif
      case sequence registered of
        Nothing -> muted "The image atlas has no room for lick.gif."
        Just frames -> do
          t <- uiTime
          let tick = floor (t * 10) :: Int
          (vis, ()) <- sensorWith (gap 6) $ do
            image (fixedWH 150 150) (frames !! (tick `mod` length frames))
            muted "lick.gif"
          when (visVisible vis) (wakeAfter (fromIntegral (tick + 1) / 10 - t))

-- | A bar sweeping 0-100% on the clock. Clock-driven drawing asks for frames
-- with 'keepAnimating', here only while the bar is on screen.
pulsingBar :: NanoUI ()
pulsingBar = do
  muted "A rounded bar, sweeping 0-100%."
  (vis, sweep) <- sensor (progressBarWith' id 12 =<< pulse 6)
  when (visVisible vis) (keepAnimating sweep)

-- Typography ------------------------------------------------------------------

typographyTab :: NanoUI ()
typographyTab = do
  sample <- useVar "The quick brown fox jumps over the lazy dog"
  sizeVar <- useVar (20 :: Float)
  isBold <- useVar False
  isItalic <- useVar True
  isUnderlined <- useVar False
  isStruck <- useVar False
  heading "Typography & Font Styling"
  muted "Sizes, weights, slant and decorations are style modifiers, composed like layout ones."
  separator
  heading "Live Playground"
  columnWith (tight . gap 12 . fillW) $ do
    muted "Preview text"
    textInput sample
  rowWith (tight . gap 12 . fillW . alignMid) $ do
    checkbox "Bold" isBold
    checkbox "Italic" isItalic
    checkbox "Underline" isUnderlined
    checkbox "Strike" isStruck
  size <- readVar sizeVar
  columnWith (tight . gap 4 . fillW) $ do
    kv "Size" (T.pack (printf "%.0f px" size))
    slider 12 40 sizeVar
  let ifOn var style = (\on -> if on then style else id) <$> readVar var
  bold' <- ifOn isBold fontBold
  italic' <- ifOn isItalic fontItalic
  underline' <- ifOn isUnderlined fontUnderline
  strike' <- ifOn isStruck fontStrike
  text <- readVar sample
  panelWith (padAll 10 . fillW) $
    labelWith (fontSize size . bold' . italic' . underline' . strike' . fillW) $
      if T.null text then "Type specimen preview..." else text
  separator
  heading "Type Scale"
  styleRows 60
    [ ("32px", fontSize 32 . fontBold, "Display Headline")
    , ("24px", fontSize 24 . fontSemiBold, "Page Section Title")
    , ("18px", fontSize 18 . fontMedium, "Card Subtitle & Highlights")
    , ("16px", fontSize 16, "Standard body text (16px base line height)")
    , ("12px", fontSize 12 . fontMuted, "Auxiliary caption, footnote, or timestamp")
    ]
  separator
  heading "Weights"
  let sphinx = "Sphinx of black quartz, judge my vow."
  styleRows 96
    [ ("Light", fontLight, sphinx)
    , ("Normal", id, sphinx)
    , ("Medium", fontMedium, sphinx)
    , ("SemiBold", fontSemiBold, sphinx)
    , ("Bold", fontBold, sphinx)
    , ("ExtraBold", fontExtraBold, sphinx)
    , ("Black", fontBlack, sphinx)
    ]
  separator
  heading "Rich Text"
  richTextSample

-- | Rows of a name, in a column this wide, and a sample in that style.
styleRows :: Float -> [(Text, LayoutModifier, Text)] -> NanoUI ()
styleRows nameW rows =
  columnWith (tight . gap 6 . fillW) $
    for_ rows $ \(name, style, sample) ->
      rowWith (tight . gap 12 . alignMid . fillW) $ do
        labelWith (tight . alignMid . fixedW nameW . fontMono . fontMuted) name
        labelWith (alignMid . style) sample

-- | Mixed styles and links in one wrapped paragraph.
richTextSample :: NanoUI ()
richTextSample = do
  lastLink <- useVar "none yet"
  clicked <-
    richTextWith fillW
      [ "A paragraph can mix ", strong "bold", ", ", emphasis "italic", ", "
      , inlineCode "monospace", " and ", inlineWith (fontSemiBold . fontColor (colorRGBA 229 192 123 255)) "coloured"
      , " pieces, ", inlineWith (fontSize 20) "larger", " ones on the same baseline, and links such as "
      , hyperlink "documentation" "the documentation", " or ", hyperlink "changelog" "the changelog"
      , ". It wraps at the width of its column."
      ]
  for_ clicked (writeVar lastLink)
  labelWith fontMuted . ("Last link clicked: " <>) =<< readVar lastLink

-- List and Table --------------------------------------------------------------

listTab :: Values -> NanoUI ()
listTab Values {valTreeItem = selected, valQuery = query} = do
  search <- useVar "" -- the field's text
  heading "Tree"
  scroll2DWith (fixedH 300 . fillW) (tree "demo" demoTree selected)
  muted . ("Selected item " <>) . T.pack . show =<< readVar selected
  separator
  heading "Searchable list"
  muted "Type to filter, or press Ctrl+F to jump here. The search commits after a pause in typing."
  searchResp <- searchInput' "Filter people (name, role, city…)" search
  whenM (shortcut (ctrl <> key 'f')) (requestFocus (respId searchResp))
  when (respChanged searchResp) (writeVar query =<< readVar search)
  committed <- readVar query
  let matches = filter (matchesQuery committed) demoPeople
  rowWith (tight . gap 12 . fillW . alignMid) $ do
    muted ("Committed: " <> if T.null committed then "(none)" else committed)
    flex
    muted ("Matches: " <> T.pack (show (length matches)))
  scroll2DWith (padAll 6 . fixedH 168 . fillW) $
    columnWith (tight . gap 6 . fillW) $
      if null matches
        then muted "No matches."
        else for_ matches (labelWith (tight . fillW) . personRow)

-- | Whether a person's name, department, city or role contains the query,
-- ignoring case.
matchesQuery :: Text -> DemoPerson -> Bool
matchesQuery q p =
  T.toCaseFold q `T.isInfixOf` T.toCaseFold (T.unwords [demoPersonName p, demoPersonDept p, demoPersonCity p, demoPersonRole p])

personRow :: DemoPerson -> Text
personRow p =
  demoPersonName p <> " - " <> demoPersonRole p <> ", " <> demoPersonCity p <> " (" <> T.pack (show (demoPersonAge p)) <> ")"

tableTab :: Var SortCol -> NanoUI ()
tableTab sortBy = do
  heading "Table"
  muted "Click a header to sort. Drag a header to reorder."
  muted "Drag a header edge to resize. Right-click a header to hide."
  people <- tableWith' (fixedH 280) "people" colPeople demoPeople sortBy
  SortCol col dir <- readVar sortBy
  separator
  kv "Sorted by" (fromMaybe "-" (columnName col))
  kv "Order" (if dir == SortAsc then "ascending" else "descending")
  kv "Hidden" $ case mapMaybe columnName (tableHiddenIndices people) of
    [] -> "none"
    hidden -> T.intercalate ", " hidden

-- | A people column's name, by its index.
columnName :: Int -> Maybe Text
columnName i = if i < 0 then Nothing else fst <$> listToMaybe (drop i peopleColumns)

-- Tabs and Panes --------------------------------------------------------------

-- | Tabs keep the selected key in a Var. Remove a tab yourself when
-- 'tabClosed' names it.
tabsTab :: Var [Int] -> NanoUI ()
tabsTab docsVar = do
  look <- useEnumVar TabContained
  place <- useEnumVar TabTop
  active <- useVar (1 :: Int)
  heading "Tabs"
  muted "A middle click also closes a tab. Only the selected tab's body runs."
  -- Two header-only strips pick the look of the one below.
  let segmented = defaultTabsConfig {tabsStyle = TabSegmented}
  responsiveRowCol 760 (tight . gap 12 . fillW) $ do
    field "Style" (boundedTabBarConfigured segmented enumLabel look)
    field "Placement" (boundedTabBarConfigured segmented enumLabel place)
  docs <- readVar docsVar
  let fresh = 1 + maximum (3 : docs)
      -- Actions on the whole strip go after its headers.
      newDoc = whenM (styled subtle (buttonWith (tight . fixedWH 28 28) "+")) $ do
        writeVar docsVar (docs <> [fresh])
        writeVar active fresh
      docTab d = closableTab d (docName d) (muted ("The body of " <> docName d <> ", which runs only while its tab is selected."))
      problems = (tab 0 "Problems" (muted "Three problems.")) {tabBadge = Just "3"}
  cfg <- TabsConfig <$> readVar look <*> readVar place <*> pure newDoc
  tabResp <- tabsConfigured' cfg active (problems : map docTab docs)
  -- Closing the selected document selects the one before it.
  for_ (tabClosed tabResp) $ \d -> do
    let (before, after) = break (== d) docs
    writeVar docsVar (before <> drop 1 after)
    when (tabActive tabResp == d) $
      writeVar active (fromMaybe 0 (listToMaybe (reverse before)))
  where
    docName = \case
      1 -> "Main.hs"
      2 -> "Tabs.hs"
      3 -> "README.md"
      d -> "Untitled-" <> T.pack (show d) <> ".hs"

panesTab :: NanoUI ()
panesTab = do
  headers <- useVar True
  heading "Pane Grid"
  muted "Drag a divider to resize. Drop a pane on another pane's center to swap them, on its edge to split it, or on the grid's outer edge to restructure the grid."
  muted "+ splits vertically, = splits horizontally, x closes, M maximizes, R restores. Arrow keys move between panes while the grid is focused."
  checkbox "Pane headers" headers
  showHeaders <- readVar headers
  panes <-
    paneGrid
      defaultPaneGridConfig
        { pgLayout = fillW . fixedH 380
        , pgSpacing = 4
        , pgMinSize = 60
        , pgLeeway = 6
        , pgViewPane = paneView showHeaders
        }
  separator
  kv "Panes" (T.pack (show (pgrPaneCount panes)))
  kv "Focused" (T.pack (show (pgrFocusedPane panes)))
  kv "Maximized" (T.pack (show (pgrMaximizedPane panes)))

-- | A pane: a card under an optional header of controls. Either way the
-- whole pane is a drag handle.
paneView :: Bool -> Word64 -> PaneGridCtx -> NanoUI PaneView
paneView showHeader pid pane = do
  let maximized = pgcMaximized pane
      title = "Pane " <> T.pack (show pid) <> if maximized then "  (maximized)" else ""
  panelWith (padAll 10 . gap 10 . grow) $ do
    when showHeader $ do
      rowWith (tight . gap 6 . alignMid . fillW) $ do
        box (alignMid . fixedWH 3 16) . themeAccent =<< uiTheme
        labelWith (tight . alignMid . fontMedium) title
        flex
        whenM (button "+") (void (pgcSplit pane AxisV))
        whenM (button "=") (void (pgcSplit pane AxisH))
        whenM (button (if maximized then "R" else "M")) (if maximized then pgcRestore pane else pgcMaximize pane)
        whenM (button "x") (pgcClose pane)
      separator
    muted ("Contents of " <> T.pack (show pid) <> ". Drag the pane to move or split it.")
  pure PaneView {pvTitle = title, pvDraggable = True}

-- Plots -----------------------------------------------------------------------

plotsTab :: PlotCache -> NanoUI ()
plotsTab cache = do
  heading "Plots"
  muted "Auto ticks, shared scales, and decimation."
  responsiveRowCol 760 (tight . gap 16 . fillW) $ do
    -- A crosshair cursor for reading values off the plot.
    figure "Sine + cosine" (withCursorShape UiCursorCrosshair (plot cache (minH 240 . fillW) sineCosineChart))
    figure "Weekly counts" (barChart cache (minH 240 . fillW) weeklyBars)
  responsiveRowCol 760 (tight . gap 16 . fillW) $ do
    figure "Sleep vs focus" (plot cache (minH 240 . fillW) sleepFocusChart)
    figure "Area" (areaChart cache (minH 240 . fillW) areaData)
  figure "Drawing" (diagram (fillW . maxH 200) . drawingSample =<< uiPlotStyle)
  -- The canvas draws from the theme's series colours and its rect alone, so
  -- it is keyed by those colours: its paths are triangulated once, not on
  -- every frame.
  figure "Canvas paths" $ do
    theme <- uiTheme
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = fixedWH 360 120 defaultLayout
        , canvasContent = contentKeyOf (map (keyPart . colorToWord32) (themeSeries theme))
        }
      (pathSample theme)

drawingSample :: PlotStyle -> Diagram B
drawingSample ps =
  (circle 0.45 # fc (plotFill ps) # lw none)
    <> (circle 0.28 # fc (plotInk ps) # lw none)
    <> (fromVertices [p2 (-0.5, -0.5), p2 (0.5, 0.5)] # lc (plotGrid ps) # lwO 1.5)

-- | Paths from "NanoUI.Path": a pie chart; a turned star with an even-odd
-- hole, a gradient fill and a round-joined outline; and a dashed curve with
-- round caps.
pathSample :: Theme -> Rect -> CanvasM ()
pathSample theme (Rect x y _ h) = do
  let r = h / 2 - 8
      centre = V2 (x + h / 2) (y + h / 2)
      shares = [0.4, 0.25, 0.2, 0.15]
      starts = scanl (+) (-pi / 2) (map (* (2 * pi)) shares)
  for_ (zip3 starts shares (themeSeries theme)) $ \(a0, share, col) ->
    drawPath (P.moveTo centre <> P.arc centre r a0 (2 * pi * share) <> P.close) col
  -- The star is built around the origin, then turned and moved into place.
  let star =
        P.polygon
          [ V2 (k * cos a) (k * sin a)
          | i <- [0 .. 9 :: Int]
          , let a = pi * fromIntegral i / 5 - pi / 2
                k = if even i then r else r * 0.45
          ]
  withTransform (P.translate (x + h * 1.5) (y + h / 2) <> P.rotate (pi / 12)) $ do
    drawPathWith
      P.EvenOdd
      (star <> P.circle (V2 0 0) (r * 0.2))
      (P.Linear (V2 0 (-r)) (V2 0 r) [(0, themeYellow theme), (1, themeOrange theme)])
    drawStrokePathWith (P.stroke 1.5) {P.strokeJoin = P.RoundJoin} star (P.Solid (themeOrange theme))
  drawStrokePathWith
    (P.stroke 3) {P.strokeCap = P.RoundCap, P.strokeDash = [10, 7]}
    (P.moveTo (V2 (x + 250) (y + h - 16)) <> P.cubicTo (V2 (x + 280) (y - 20)) (V2 (x + 310) (y + h + 20)) (V2 (x + 350) (y + 16)))
    (P.Solid (themeAccent theme))

-- The Debug window ------------------------------------------------------------

-- | The SDL backend's frame statistics, which it samples four times a second
-- while something asks for them (here, while the window is open), and the
-- layout overlay.
debugBody :: NanoUI ()
debugBody =
  columnWith (tight . gap 4 . minW 300 . fillW) $ do
    stats <- askSdlDebug
    scope . for_ stats $ \s ->
      for_ (debugSections s) $ \(title, rows) -> do
        heading title
        mapM_ (uncurry kvMono) rows
        separator
    -- The overlay outlines every layout node; the rows describe the one under
    -- the pointer. Its switch lives in the context, so the checkbox edits a
    -- copy.
    heading "Layout"
    on <- explainingLayout
    (_, explain) <- withValue on (checkbox "Outline layout nodes")
    explainLayout explain
    scope $ when explain (mapM_ (uncurry kvMono) . formatExplainRows =<< explainedNode)

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

-- Diagnostics -----------------------------------------------------------------

-- | The headline frame statistics, without opening the Debug window. Asking
-- for them keeps the backend sampling, so only while this tab shows.
diagnosticsTab :: NanoUI ()
diagnosticsTab = do
  heading "Diagnostics"
  muted "Sampled four times a second while this tab shows. F12 opens the Debug window with the rest."
  separator
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
