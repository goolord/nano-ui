module RgfwDemoCommon
  ( Model (..)
  , appView
  , initialModel
  , main
  , physScaleFor
  , themeForChoice
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
  ( Input (..)
  , NanoUI
  , Size (..)
  , Theme
  , boundedRadio
  , button
  , button'
  , checkbox
  , contextMenu
  , fillH
  , fillW
  , fixedH
  , fixedW
  , flex
  , gap
  , gridWith
  , label
  , menuHeader
  , menuItem
  , menuItemDisabled
  , menuItemShortcut
  , menuSeparator
  , padAll
  , panelWith
  , respClicked
  , rowWith
  , separator
  , slider
  , tab
  , tabBar
  , textArea'
  , textInput
  , tomorrowMidnightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  , whenM
  , window
  )
import NanoUI.Monad (askInput)
import NanoUI.Shortcut (ctrl, key)
import NanoUI.Backend.Rgfw
  ( RgfwOptions (..)
  , askRgfwDebug
  , debugWindowBody
  , defaultRgfwOptions
  , runRgfwAppReduceCustom
  )
import NanoUI.Emit qualified as Emit

data TabChoice
  = TabControls
  | TabGallery
  | TabAbout
  | TabDiagnostics
  deriving (Bounded, Enum, Eq, Show)

data ThemeChoice
  = ThemeNight
  | ThemeLight
  | ThemeMidnight
  deriving (Bounded, Enum, Eq, Show)

data ProfileChoice
  = ProfileFast
  | ProfileBalanced
  | ProfileQuality
  deriving (Bounded, Enum, Eq, Show)

-- | In the order the scale button cycles through them.
data DpiScaleChoice
  = DpiScaleAuto
  | DpiScale1
  | DpiScale15
  | DpiScale2
  | DpiScale3
  | DpiScale05
  deriving (Bounded, Enum, Eq, Show)

formatDpiScale :: DpiScaleChoice -> Text
formatDpiScale DpiScaleAuto = "Auto (OS)"
formatDpiScale sc = T.pack (show (physScaleFor sc)) <> "x"

physScaleFor :: DpiScaleChoice -> Float
physScaleFor DpiScaleAuto = 0.0 -- 0.0 means: use the DPI reported by the OS by default
physScaleFor DpiScale05   = 0.5
physScaleFor DpiScale1    = 1.0
physScaleFor DpiScale15   = 1.5
physScaleFor DpiScale2    = 2.0
physScaleFor DpiScale3    = 3.0

data Msg
  = SetTab !TabChoice
  | CycleTheme
  | CycleScale
  | Increment
  | Decrement
  | Reset
  | ToggleTurbo !Bool
  | SetVolume !Float
  | SetOpacity !Float
  | SetInputText !Text
  | SetNotesText !Text
  | SetProfile !ProfileChoice
  | ClearNotes
  | ToggleDebug !Bool
  deriving (Eq, Show)

data Model = Model
  { activeTab    :: !TabChoice
  , currentTheme :: !ThemeChoice
  , dpiScale     :: !DpiScaleChoice
  , counter      :: !Int
  , turboOn      :: !Bool
  , volumeVal    :: !Float
  , opacityVal   :: !Float
  , textVal      :: !Text
  , notesVal     :: !Text
  , profileOpt   :: !ProfileChoice
  , totalClicks  :: !Int
  , debugOpen    :: !Bool
  }
  deriving (Eq, Show)

initialModel :: Model
initialModel =
  Model
    { activeTab    = TabControls
    , currentTheme = ThemeNight
    , dpiScale     = DpiScaleAuto
    , counter      = 42
    , turboOn      = True
    , volumeVal    = 0.72
    , opacityVal   = 0.90
    , textVal      = "nano-ui rgfw edition"
    , notesVal     = "Edit me.\nSecond line."
    , profileOpt   = ProfileBalanced
    , totalClicks  = 0
    , debugOpen    = False
    }

themeForChoice :: ThemeChoice -> Theme
themeForChoice ThemeNight    = tomorrowNightMinDarkTheme
themeForChoice ThemeLight    = tomorrowMinLightTheme
themeForChoice ThemeMidnight = tomorrowMidnightMinDarkTheme

nextEnum :: (Eq a, Enum a, Bounded a) => a -> a
nextEnum x
  | x == maxBound = minBound
  | otherwise     = succ x

update :: Msg -> Model -> Model
update msg m =
  let !m' = m {totalClicks = totalClicks m + 1}
   in case msg of
        SetTab t         -> m' {activeTab = t}
        CycleTheme       -> m' {currentTheme = nextEnum (currentTheme m)}
        CycleScale       -> m' {dpiScale = nextEnum (dpiScale m)}
        Increment        -> m' {counter = counter m + 1}
        Decrement        -> m' {counter = counter m - 1}
        Reset            -> m' {counter = 0}
        ToggleTurbo b    -> m' {turboOn = b}
        SetVolume v      -> m' {volumeVal = max 0 (min 1 v)}
        SetOpacity v     -> m' {opacityVal = max 0 (min 1 v)}
        SetInputText t   -> m' {textVal = t}
        SetNotesText t   -> m' {notesVal = t}
        SetProfile p     -> m' {profileOpt = p}
        ClearNotes       -> m' {notesVal = ""}
        ToggleDebug b    -> m' {debugOpen = b}

appView :: Model -> NanoUI ()
appView m = do
  panelWith (padAll 12 . gap 8 . fillW . fillH) $ do
    rowWith (gap 8 . fixedH 24 . fillW) $ do
      label "nano-ui on RGFW"
      flex

      Emit.emitWhen
        ( button
            ( case currentTheme m of
                ThemeNight -> "[Theme: Tomorrow Night]"
                ThemeLight -> "[Theme: Tomorrow Light]"
                ThemeMidnight -> "[Theme: Midnight Black]"
            )
        )
        CycleTheme

      Emit.emitWhen
        (button ("[" <> formatDpiScale (dpiScale m) <> " DPI Scale]"))
        CycleScale

      Emit.emitWhen
        (button (if debugOpen m then "[Debug: ON]" else "[Debug: OFF]"))
        (ToggleDebug (not (debugOpen m)))

    nextTab <-
      tabBar
        (activeTab m)
        [ tab TabControls "Controls" ()
        , tab TabGallery "Unicode Gallery" ()
        , tab TabAbout "About" ()
        , tab TabDiagnostics "Diagnostics" ()
        ]
    when (nextTab /= activeTab m) (Emit.emit (SetTab nextTab))

    separator

    case activeTab m of
      TabControls -> viewControlsTab m
      TabGallery -> viewGalleryTab
      TabAbout -> viewAboutTab
      TabDiagnostics -> viewDiagnosticsTab m

    when (debugOpen m) $ do
      snap <- askRgfwDebug
      (win, _) <- window True "Debug" (debugWindowBody snap)
      when (respClicked win) (Emit.emit (ToggleDebug False))

viewControlsTab :: Model -> NanoUI ()
viewControlsTab m = do
  gridWith 2 (gap 12 . fillW . fillH) $ do
    panelWith (padAll 10 . gap 6 . fixedW 380 . fillH) $ do
      label "Controls"
      separator

      gridWith 4 (gap 6 . fixedH 22 . fillW) $ do
        label ("Counter: " <> T.pack (show (counter m)))
        Emit.emitWhen (button " +1 ") Increment
        Emit.emitWhen (button " -1 ") Decrement
        Emit.emitWhen (button " Reset ") Reset

      gridWith 1 (gap 6 . fixedH 20) $ do
        Emit.emitChanged (checkbox "Turbo mode") (turboOn m) ToggleTurbo

      gridWith 2 (gap 6 . fixedH 22) $ do
        label "Context Menu:"
        menuBtn <- button' "Right-click Me"
        void $ contextMenu menuBtn $ do
          menuHeader "Edit Actions"
          menuSeparator
          whenM
            (menuItemShortcut "Cut" (ctrl <> key 'x'))
            (Emit.emit (SetNotesText "Cut text to clipboard"))
          whenM
            (menuItemShortcut "Copy" (ctrl <> key 'c'))
            (Emit.emit (SetNotesText "Copied text to clipboard"))
          whenM
            (menuItemShortcut "Paste" (ctrl <> key 'v'))
            (Emit.emit (SetNotesText "Pasted text from clipboard"))
          menuSeparator
          menuHeader "System"
          whenM (menuItem "Reset Counter") (Emit.emit Reset)
          menuItemDisabled "Disabled Command"

      gridWith 1 (gap 2) $ do
        let
          volPct = round (volumeVal m * 100) :: Int
        label ("Master Volume: " <> T.pack (show volPct) <> "%")
        Emit.emitChanged (slider 0 1) (volumeVal m) SetVolume

      gridWith 1 (gap 2) $ do
        let
          opPct = round (opacityVal m * 100) :: Int
        label ("Surface Opacity: " <> T.pack (show opPct) <> "%")
        Emit.emitChanged (slider 0 1) (opacityVal m) SetOpacity

      gridWith 1 (gap 2) $ do
        label "Single-line Text Input:"
        Emit.emitChanged textInput (textVal m) SetInputText

      gridWith 1 (gap 2) $ do
        gridWith 2 (gap 4 . fixedH 18) $ do
          label "Multi-line Notes Field:"
          Emit.emitWhen (button "Clear") ClearNotes
        Emit.emitEdited textArea' (notesVal m) SetNotesText

      gridWith 1 (gap 2) $ do
        label "Preset:"
        radVal <-
          boundedRadio
            ( \case
                ProfileFast -> "Fast (Low Latency)"
                ProfileBalanced -> "Balanced (Standard)"
                ProfileQuality -> "Quality (High Detail)"
            )
            (profileOpt m)
        when (radVal /= profileOpt m) (Emit.emit (SetProfile radVal))

    panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
      label "State"
      separator

      gridWith 2 (gap 6) $ do
        label "Active Theme:"
        label (T.pack (show (currentTheme m)))
        let
          scText = case dpiScale m of
            DpiScaleAuto -> "Auto (OS reported)"
            sc ->
              formatDpiScale sc <> " (" <> T.pack (show (physScaleFor sc)) <> "x)"
        label "DPI Scale:"
        label scText
        label "Counter Value:"
        label (T.pack (show (counter m)))
        label "Turbo Mode:"
        label (if turboOn m then "[ENABLED]" else "[DISABLED]")
        label "Volume Slider:"
        label (T.pack (show (round (volumeVal m * 100) :: Int)) <> "%")
        label "Opacity Slider:"
        label (T.pack (show (round (opacityVal m * 100) :: Int)) <> "%")
        label "Profile Selected:"
        label (T.pack (show (profileOpt m)))
        label "Text Input:"
        label (textVal m)
        label "Total Clicks:"
        label (T.pack (show (totalClicks m)))

      separator

      label "Block-character bars:"
      gridWith 2 (gap 4) $ do
        let
          makeBar pct =
            let
              filled = max 0 (min 20 (pct `div` 5))
              empty = 20 - filled
             in
              T.replicate filled "█" <> T.replicate empty "░"
          volPct = round (volumeVal m * 100) :: Int
          opPct = round (opacityVal m * 100) :: Int
        label "Master Volume:"
        label ("[" <> makeBar volPct <> "] " <> T.pack (show volPct) <> "%")
        label "Surface Opacity:"
        label ("[" <> makeBar opPct <> "] " <> T.pack (show opPct) <> "%")

-- | A sample of the glyphs in the bundled Cozette font.
viewGalleryTab :: NanoUI ()
viewGalleryTab = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    label "Cozette glyphs"
    separator

    gridWith 1 (gap 8) $ do
      gridWith 1 (gap 2) $ do
        label "Printable ASCII:"
        label "!\"#$%&'()*+,-./0123456789:;<=>?"
        label "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
        label "`abcdefghijklmnopqrstuvwxyz{|}~"

      separator

      gridWith 1 (gap 2) $ do
        label "Greek:"
        label "Δ Ω Σ α β γ δ ε θ λ μ π ρ τ ω"

      gridWith 1 (gap 2) $ do
        label "Math and logic:"
        label "± × ÷ √ ∞ ≤ ≥ ≠ ≈ ≡ ∀ ∃ ∈ ∉ ∧ ∨ ∂ ∇"

      gridWith 1 (gap 2) $ do
        label "Box drawing:"
        label "┌───┬───┐  ╔═══╦═══╗  ┏━━━┳━━━┓"
        label "│ A │ B │  ║ X ║ Y ║  ┃ 1 ┃ 2 ┃"
        label "├───┼───┤  ╠═══╬═══╣  ┣━━━╋━━━┫"
        label "│ C │ D │  ║ Z ║ W ║  ┃ 3 ┃ 4 ┃"
        label "└───┴───┘  ╚═══╩═══╝  ┗━━━┻━━━┛"

      gridWith 1 (gap 2) $ do
        label "Blocks and shades:"
        label "█ ▓ ▒ ░ ▀ ▄ ▌ ▐ ▖ ▗ ▘ ▙ ▚ ▛ ▜ ▝ ▞ ▟"

      gridWith 1 (gap 2) $ do
        label "Keys:"
        label "⏎ Enter  ⇥ Tab  ⌃ Ctrl  ⌥ Alt  ⌘ Cmd  ⌫ Bksp  ⎋ Esc"

      separator

      gridWith 1 (gap 4) $ do
        label "Icon buttons:"
        gridWith 4 (gap 4 . fixedH 24 . fillW) $
          mapM_
            (void . button)
            [ "\xF002 Search", "\xF004 Health", "\xF005 Star", "\xF00C Check"
            , "\xF00D Close", "\xF013 Settings", "\xF01E Reload", "\xF026 Mute"
            , "\xF028 Sound", "\xF04B Play", "\xF04C Pause", "\xF04D Stop"
            , "\xF188 Debug", "\xF11B Gamepad", "⏎ Enter", "⎋ Esc"
            ]

viewAboutTab :: NanoUI ()
viewAboutTab =
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    label "About"
    separator
    label "Text uses the bundled Cozette bitmap font, so no font files are needed."
    label "Themes are made square: corner radius 0 and 1px borders, so drawn boxes match hit boxes."
    label "The scale button cycles Auto, 1x, 1.5x, 2x, 3x and 0.5x; Auto follows the monitor."

viewDiagnosticsTab :: Model -> NanoUI ()
viewDiagnosticsTab m = do
  Size w h <- inputWindowSize <$> askInput
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    label "Diagnostics"
    separator

    gridWith 2 (gap 4) $ do
      label "Window:"
      label
        ( T.pack (show (round w :: Int))
            <> " x "
            <> T.pack (show (round h :: Int))
            <> " logical px"
        )
      label "Scale:"
      label (formatDpiScale (dpiScale m))
      label "Renderer:"
      label "OpenGL 3.2 core"
      label "Active Tab:"
      label (T.pack (show (activeTab m)))
      label "Current Theme:"
      label (T.pack (show (currentTheme m)))
      label "Interaction Clicks:"
      label (T.pack (show (totalClicks m)))

    separator

    Emit.emitWhen
      (button (if debugOpen m then "[Close Debug Window]" else "[Open Debug Window]"))
      (ToggleDebug (not (debugOpen m)))

main :: IO ()
main = do
  let opts =
        defaultRgfwOptions
          { optTitle  = "nano-ui RGFW demo"
          , optWidth  = 1680
          , optHeight = 1040
          , optTheme  = tomorrowNightMinDarkTheme
          , optScale  = 0.0 -- 0.0 uses the DPI reported by the OS by default
          }
  runRgfwAppReduceCustom opts (\m -> (themeForChoice (currentTheme m), physScaleFor (dpiScale m))) update initialModel appView
