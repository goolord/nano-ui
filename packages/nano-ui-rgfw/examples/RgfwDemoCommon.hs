{-# LANGUAGE OverloadedStrings #-}

module RgfwDemoCommon where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
  ( NanoUI
  , boundedRadio
  , button
  , button'
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
  , rowWith
  , separator
  , tab
  , tabBar
  , whenM
  , window
  , respClicked
  )
import NanoUI.Backend.Rgfw
  ( RgfwOptions (..)
  , RgfwTheme (..)
  , askRgfwDebug
  , debugWindowBody
  , defaultDarkTheme
  , defaultRgfwOptions
  , runRgfwAppReduceCustom
  , tomorrowMidnightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  )
import NanoUI.Emit qualified as Emit

data TabChoice
  = TabControls
  | TabGallery
  | TabArchitecture
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

data DpiScaleChoice
  = DpiScaleAuto
  | DpiScale05
  | DpiScale1
  | DpiScale15
  | DpiScale2
  | DpiScale3
  deriving (Bounded, Enum, Eq, Show)

formatDpiScale :: DpiScaleChoice -> Text
formatDpiScale DpiScaleAuto = "Auto (OS)"
formatDpiScale DpiScale05   = "0.5x"
formatDpiScale DpiScale1    = "1.0x"
formatDpiScale DpiScale15   = "1.5x"
formatDpiScale DpiScale2    = "2.0x"
formatDpiScale DpiScale3    = "3.0x"

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
    , notesVal     = "Lean single-pass backend\nBitmap Cozette typography\nTomorrow Min themes"
    , profileOpt   = ProfileBalanced
    , totalClicks  = 0
    , debugOpen    = False
    }

themeForChoice :: ThemeChoice -> RgfwTheme
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
        CycleScale       ->
          let nextSc = case dpiScale m of
                DpiScaleAuto -> DpiScale1
                DpiScale1    -> DpiScale15
                DpiScale15   -> DpiScale2
                DpiScale2    -> DpiScale3
                DpiScale3    -> DpiScale05
                DpiScale05   -> DpiScaleAuto
           in m' {dpiScale = nextSc}
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
    -- Top Bar: Title, Theme, DPI Scale
    rowWith (gap 8 . fixedH 24 . fillW) $ do
      label "NANO-UI // RGFW LEAN BACKEND"
      flex

      Emit.button (case currentTheme m of
        ThemeNight    -> "[Theme: Tomorrow Night]"
        ThemeLight    -> "[Theme: Tomorrow Light]"
        ThemeMidnight -> "[Theme: Midnight Black]") CycleTheme

      Emit.button ("[" <> formatDpiScale (dpiScale m) <> " DPI Scale]") CycleScale

      Emit.button (if debugOpen m then "[Debug: ON]" else "[Debug: OFF]") (ToggleDebug (not (debugOpen m)))

    -- Tab Bar
    nextTab <-
      tabBar
        (activeTab m)
        [ tab TabControls "Controls" ()
        , tab TabGallery "Unicode Gallery" ()
        , tab TabArchitecture "Architecture" ()
        , tab TabDiagnostics "Diagnostics" ()
        ]
    when (nextTab /= activeTab m) (Emit.emit (SetTab nextTab))

    separator

    -- Main Content based on active tab
    case activeTab m of
      TabControls      -> viewControlsTab m
      TabGallery       -> viewGalleryTab
      TabArchitecture  -> viewArchitectureTab
      TabDiagnostics   -> viewDiagnosticsTab m

    -- Floating Debug Window
    when (debugOpen m) $ do
      snap <- askRgfwDebug
      (win, _) <- window True "Debug Diagnostics" (debugWindowBody snap)
      when (respClicked win) (Emit.emit (ToggleDebug False))

-- | Tab 1: Controls
viewControlsTab :: Model -> NanoUI ()
viewControlsTab m = do
  gridWith 2 (gap 12 . fillW . fillH) $ do
    -- Left Column: Interactive Form Controls
    panelWith (padAll 10 . gap 6 . fixedW 380 . fillH) $ do
      label "WIDGET CONTROLS"
      separator

      -- Counter
      gridWith 4 (gap 6 . fixedH 22 . fillW) $ do
        label ("Counter: " <> T.pack (show (counter m)))
        Emit.button " +1 " Increment
        Emit.button " -1 " Decrement
        Emit.button " Reset " Reset

      -- Checkbox
      gridWith 1 (gap 6 . fixedH 20) $ do
        Emit.checkbox "Enable turbo execution mode" (turboOn m) ToggleTurbo

      -- Context Menu
      gridWith 2 (gap 6 . fixedH 22) $ do
        label "Context Menu:"
        menuBtn <- button' "Right-click Me"
        void $ contextMenu menuBtn $ do
          menuHeader "Edit Actions"
          menuSeparator
          whenM (menuItemShortcut "Cut" "Ctrl+X") (Emit.emit (SetNotesText "Cut text to clipboard"))
          whenM (menuItemShortcut "Copy" "Ctrl+C") (Emit.emit (SetNotesText "Copied text to clipboard"))
          whenM (menuItemShortcut "Paste" "Ctrl+V") (Emit.emit (SetNotesText "Pasted text from clipboard"))
          menuSeparator
          menuHeader "System"
          whenM (menuItem "Reset Counter") (Emit.emit Reset)
          menuItemDisabled "Disabled Command"

      -- Sliders
      gridWith 1 (gap 2) $ do
        let volPct = round (volumeVal m * 100) :: Int
        label ("Master Volume: " <> T.pack (show volPct) <> "%")
        Emit.slider 0 1 (volumeVal m) SetVolume

      gridWith 1 (gap 2) $ do
        let opPct = round (opacityVal m * 100) :: Int
        label ("Surface Opacity: " <> T.pack (show opPct) <> "%")
        Emit.slider 0 1 (opacityVal m) SetOpacity

      -- Text Input
      gridWith 1 (gap 2) $ do
        label "Single-line Text Input:"
        Emit.textInput (textVal m) SetInputText

      -- Text Area
      gridWith 1 (gap 2) $ do
        gridWith 2 (gap 4 . fixedH 18) $ do
          label "Multi-line Notes Field:"
          Emit.button "Clear" ClearNotes
        Emit.textArea (notesVal m) SetNotesText

      -- Radio Buttons
      gridWith 1 (gap 2) $ do
        label "Preset:"
        radVal <- boundedRadio (\case
          ProfileFast     -> "Fast (Low Latency)"
          ProfileBalanced -> "Balanced (Standard)"
          ProfileQuality  -> "Quality (High Detail)") (profileOpt m)
        when (radVal /= profileOpt m) (Emit.emit (SetProfile radVal))

    -- Right Column: Live State Inspector & Visualizer
    panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
      label "STATE INSPECTOR & METERS"
      separator

      gridWith 2 (gap 6) $ do
        label "Active Theme:"
        label (T.pack (show (currentTheme m)))
        let scText = case dpiScale m of
              DpiScaleAuto -> "Auto (OS reported)"
              sc           -> formatDpiScale sc <> " (" <> T.pack (show (physScaleFor sc)) <> "x)"
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

      label "Live Unicode Progress Bars:"
      gridWith 2 (gap 4) $ do
        let makeBar pct =
              let filled = max 0 (min 20 (pct `div` 5))
                  empty  = 20 - filled
               in T.replicate filled "█" <> T.replicate empty "░"
            volPct = round (volumeVal m * 100) :: Int
            opPct  = round (opacityVal m * 100) :: Int
        label "Master Volume:"
        label ("[" <> makeBar volPct <> "] " <> T.pack (show volPct) <> "%")
        label "Surface Opacity:"
        label ("[" <> makeBar opPct  <> "] " <> T.pack (show opPct) <> "%")

-- | Tab 2: Unicode & Icon Gallery
viewGalleryTab :: NanoUI ()
viewGalleryTab = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    label "COZETTE EMBEDDED BITMAP FONT // UNICODE SHOWCASE"
    separator

    gridWith 1 (gap 8) $ do
      gridWith 1 (gap 2) $ do
        label "ASCII Printable Characters:"
        label "!\"#$%&'()*+,-./0123456789:;<=>?"
        label "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
        label "`abcdefghijklmnopqrstuvwxyz{|}~"

      separator

      gridWith 1 (gap 2) $ do
        label "Greek Letters & Physics Variables:"
        label "Δ Ω Σ α β γ δ ε θ λ μ π ρ τ ω"

      gridWith 1 (gap 2) $ do
        label "Mathematical & Logic Operators:"
        label "± × ÷ √ ∞ ≤ ≥ ≠ ≈ ≡ ∀ ∃ ∈ ∉ ∧ ∨ ∂ ∇"

      gridWith 1 (gap 2) $ do
        label "Box Drawing & Frame Elements:"
        label "┌───┬───┐  ╔═══╦═══╗  ┏━━━┳━━━┓"
        label "│ A │ B │  ║ X ║ Y ║  ┃ 1 ┃ 2 ┃"
        label "├───┼───┤  ╠═══╬═══╣  ┣━━━╋━━━┫"
        label "│ C │ D │  ║ Z ║ W ║  ┃ 3 ┃ 4 ┃"
        label "└───┴───┘  ╚═══╩═══╝  ┗━━━┻━━━┛"

      gridWith 1 (gap 2) $ do
        label "Block Elements & Shading Meters:"
        label "█ ▓ ▒ ░ ▀ ▄ ▌ ▐ ▖ ▗ ▘ ▙ ▚ ▛ ▜ ▝ ▞ ▟"

      gridWith 1 (gap 2) $ do
        label "Keycaps & Modifiers:"
        label "⏎ Enter  ⇥ Tab  ⌃ Ctrl  ⌥ Alt  ⌘ Cmd  ⌫ Bksp  ⎋ Esc"

      separator

      gridWith 1 (gap 4) $ do
        label "Nerd Font & UI Icon Buttons (4-Column Native Grid):"
        gridWith 4 (gap 4 . fixedH 24 . fillW) $ do
          void $ button "\xF002 Search"
          void $ button "\xF004 Health"
          void $ button "\xF005 Star"
          void $ button "\xF00C Check"
          void $ button "\xF00D Close"
          void $ button "\xF013 Settings"
          void $ button "\xF01E Reload"
          void $ button "\xF026 Mute"
          void $ button "\xF028 Sound"
          void $ button "\xF04B Play"
          void $ button "\xF04C Pause"
          void $ button "\xF04D Stop"
          void $ button "\xF188 Debug"
          void $ button "\xF11B Gamepad"
          void $ button "⏎ Enter"
          void $ button "⎋ Esc"

-- | Tab 3: Architecture
viewArchitectureTab :: NanoUI ()
viewArchitectureTab = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    label "LEAN BACKEND ARCHITECTURE & DESIGN PRINCIPLES"
    separator

    gridWith 1 (gap 6) $ do
      label "1. Single-Pass O(N) Linear Grid & Flex Layout Engine:"
      label "   - Native multi-column 2D grids (gridWith N) with automatic column & row distribution."
      label "   - Zero backtracking, zero flex equations, zero quadratic passes."
      label "   - Direct contiguous allocation in unboxed PrimArray."
      label "   - Strictly clamps child nodes to parent remaining bounds."

      separator

      label "2. Exact 1:1 Collision-Box Theming:"
      label "   - Visual geometry matches collision/hit-test bounds exactly."
      label "   - Zero rounded corners, zero soft drop-shadows, zero bloat."
      label "   - Pure color themes: Tomorrow Min Light, Night, and Midnight."

      separator

      label "3. Embedded Cozette Bitmap Typography:"
      label "   - 18,492-byte pruned OpenType bitmap font (.otb) embedded in binary."
      label "   - 921 custom glyphs (ASCII, Greek, Math, Box, Powerline, Nerd icons)."
      label "   - Uniform 6px cell width, 13px line height, 10px ascent."
      label "   - 1-bit glyph blitter bakes an OpenGL atlas, zero FreeType dependency."

      separator

      label "4. Integer DPI Scaling:"
      label "   - Integer scaling factors (1x, 2x, 3x...)."
      label "   - Logical UI coordinates mapped with exact integer floor division."
      label "   - High-throughput nearest-neighbor pixel replication preserving crispness."

-- | Tab 4: Diagnostics
viewDiagnosticsTab :: Model -> NanoUI ()
viewDiagnosticsTab m = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    label "SYSTEM DIAGNOSTICS & TELEMETRY"
    separator

    gridWith 1 (gap 6) $ do
      label "Window & Surface Telemetry (2-Column Property Grid):"
      let !sc = dpiScale m
          !physScale = physScaleFor sc
          !physW = 1680 :: Int
          !physH = 1040 :: Int
          !effScale = if physScale > 0.0 then physScale else 1.0
          !logW = round (fromIntegral physW / effScale) :: Int
          !logH = round (fromIntegral physH / effScale) :: Int

      gridWith 2 (gap 4) $ do
        label "Physical Window Size:"
        label (T.pack (show physW) <> " x " <> T.pack (show physH) <> " px")
        label "DPI Scale Choice:"
        label (formatDpiScale sc <> (if physScale <= 0.0 then " (OS Native DPI)" else " (" <> T.pack (show physScale) <> "x DPI)"))
        label "Logical Viewport Size:"
        label (T.pack (show logW) <> " x " <> T.pack (show logH) <> " px")
        label "Framebuffer Bit Depth:"
        label "32-bit RGBA (OpenGL 3.2 core)"
        label "Framebuffer Memory:"
        label (T.pack (show (physW * physH * 4 `div` 1024)) <> " KB")
        label "Target Frame Rate:"
        label "120 FPS max pacing"

      separator

      gridWith 2 (gap 4) $ do
        label "Active Tab:"
        label (T.pack (show (activeTab m)))
        label "Current Theme:"
        label (T.pack (show (currentTheme m)))
        label "Interaction Clicks:"
        label (T.pack (show (totalClicks m)))
        label "Compiler Toolchain:"
        label "Zig C Compiler (zig cc)"
        label "Layout Paradigm:"
        label "Native Multi-Column 2D Grid"

      separator

      label "Floating Diagnostics Window:"
      Emit.button (if debugOpen m then "[Close Debug Window]" else "[Open Floating Debug Window (FPS, Timing, Arena, RTS)]") (ToggleDebug (not (debugOpen m)))

main :: IO ()
main = do
  let opts =
        defaultRgfwOptions
          { optTitle  = "nano-ui [RGFW Lean Backend // Tomorrow Min]"
          , optWidth  = 1680
          , optHeight = 1040
          , optTheme  = defaultDarkTheme
          , optScale  = 0.0 -- 0.0 uses the DPI reported by the OS by default
          }
  runRgfwAppReduceCustom opts (\m -> (themeForChoice (currentTheme m), physScaleFor (dpiScale m))) update initialModel appView
