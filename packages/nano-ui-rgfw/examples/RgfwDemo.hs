{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
  ( NanoUI
  , boundedRadioFieldset
  , button
  , checkbox
  , contextMenu
  , emit
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
  , menuItemWithShortcut
  , menuSeparator
  , onClick
  , padAll
  , panelWith
  , rowWith
  , separator
  , slider
  , textArea
  , textInput
  , window
  , respClicked
  , respChanged
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
        SetTab tab       -> m' {activeTab = tab}
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
    rowWith (gap 8 . fixedH 24) $ do
      void $ label "NANO-UI // RGFW LEAN BACKEND"
      void $ flex

      themeBtn <- button $ case currentTheme m of
        ThemeNight    -> "[Theme: Tomorrow Night]"
        ThemeLight    -> "[Theme: Tomorrow Light]"
        ThemeMidnight -> "[Theme: Midnight Black]"
      when (respClicked themeBtn) (emit CycleTheme)

      scaleBtn <- button ("[" <> formatDpiScale (dpiScale m) <> " DPI Scale]")
      when (respClicked scaleBtn) (emit CycleScale)

      debugBtn <- button (if debugOpen m then "[Debug: ON]" else "[Debug: OFF]")
      when (respClicked debugBtn) (emit (ToggleDebug (not (debugOpen m))))

    -- Tab Bar
    gridWith 4 (gap 4 . fixedH 24) $ do
      let mkTab tab title = do
            let isActive = activeTab m == tab
                tag = if isActive then "tab:active:" else "tab:"
            btn <- button (tag <> title)
            when (respClicked btn) (emit (SetTab tab))
      mkTab TabControls "Controls"
      mkTab TabGallery "Unicode Gallery"
      mkTab TabArchitecture "Architecture"
      mkTab TabDiagnostics "Diagnostics"

    void $ separator

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
      onClick win (emit (ToggleDebug False))

-- | Tab 1: Controls
viewControlsTab :: Model -> NanoUI ()
viewControlsTab m = do
  gridWith 2 (gap 12 . fillW . fillH) $ do
    -- Left Column: Interactive Form Controls
    panelWith (padAll 10 . gap 6 . fixedW 380 . fillH) $ do
      void $ label "WIDGET CONTROLS"
      void $ separator

      -- Counter
      gridWith 4 (gap 6 . fixedH 22) $ do
        void $ label ("Counter: " <> T.pack (show (counter m)))
        incBtn <- button " +1 "
        when (respClicked incBtn) (emit Increment)
        decBtn <- button " -1 "
        when (respClicked decBtn) (emit Decrement)
        rstBtn <- button " Reset "
        when (respClicked rstBtn) (emit Reset)

      -- Checkbox
      gridWith 1 (gap 6 . fixedH 20) $ do
        (cbResp, cbVal) <- checkbox "Enable turbo execution mode" (turboOn m)
        when (respClicked cbResp || respChanged cbResp) (emit (ToggleTurbo cbVal))

      -- Context Menu
      gridWith 2 (gap 6 . fixedH 22) $ do
        void $ label "Context Menu:"
        menuBtn <- button "Right-click Me"
        void $ contextMenu menuBtn $ do
          menuHeader "Edit Actions"
          menuSeparator
          cCut <- menuItemWithShortcut "Cut" "Ctrl+X"
          when (respClicked cCut) (emit (SetNotesText "Cut text to clipboard"))
          cCopy <- menuItemWithShortcut "Copy" "Ctrl+C"
          when (respClicked cCopy) (emit (SetNotesText "Copied text to clipboard"))
          cPaste <- menuItemWithShortcut "Paste" "Ctrl+V"
          when (respClicked cPaste) (emit (SetNotesText "Pasted text from clipboard"))
          menuSeparator
          menuHeader "System"
          cReset <- menuItem "Reset Counter"
          when (respClicked cReset) (emit Reset)
          menuItemDisabled "Disabled Command"

      -- Sliders
      gridWith 1 (gap 2) $ do
        let volPct = round (volumeVal m * 100) :: Int
        void $ label ("Master Volume: " <> T.pack (show volPct) <> "%")
        (slResp, slVal) <- slider "Vol" 0 1 (volumeVal m)
        when (respChanged slResp) (emit (SetVolume slVal))

      gridWith 1 (gap 2) $ do
        let opPct = round (opacityVal m * 100) :: Int
        void $ label ("Surface Opacity: " <> T.pack (show opPct) <> "%")
        (opResp, opVal) <- slider "Opacity" 0 1 (opacityVal m)
        when (respChanged opResp) (emit (SetOpacity opVal))

      -- Text Input
      gridWith 1 (gap 2) $ do
        void $ label "Single-line Text Input:"
        (tiResp, tiVal) <- textInput "Input" (textVal m)
        when (respChanged tiResp) (emit (SetInputText tiVal))

      -- Text Area
      gridWith 1 (gap 2) $ do
        gridWith 2 (gap 4 . fixedH 18) $ do
          void $ label "Multi-line Notes Field:"
          clrBtn <- button "Clear"
          when (respClicked clrBtn) (emit ClearNotes)
        (taResp, taVal) <- textArea "Notes" (notesVal m)
        when (respChanged taResp) (emit (SetNotesText taVal))

      -- Radio Buttons
      gridWith 1 (gap 2) $ do
        (radResp, radVal) <- boundedRadioFieldset "Preset" (profileOpt m) $ \case
          ProfileFast     -> "Fast (Low Latency)"
          ProfileBalanced -> "Balanced (Standard)"
          ProfileQuality  -> "Quality (High Detail)"
        when (respChanged radResp) (emit (SetProfile radVal))

    -- Right Column: Live State Inspector & Visualizer
    panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
      void $ label "STATE INSPECTOR & METERS"
      void $ separator

      gridWith 2 (gap 6) $ do
        void $ label "Active Theme:"
        void $ label (T.pack (show (currentTheme m)))
        let scText = case dpiScale m of
              DpiScaleAuto -> "Auto (OS reported)"
              sc           -> formatDpiScale sc <> " (" <> T.pack (show (physScaleFor sc)) <> "x)"
        void $ label "DPI Scale:"
        void $ label scText
        void $ label "Counter Value:"
        void $ label (T.pack (show (counter m)))
        void $ label "Turbo Mode:"
        void $ label (if turboOn m then "[ENABLED]" else "[DISABLED]")
        void $ label "Volume Slider:"
        void $ label (T.pack (show (round (volumeVal m * 100) :: Int)) <> "%")
        void $ label "Opacity Slider:"
        void $ label (T.pack (show (round (opacityVal m * 100) :: Int)) <> "%")
        void $ label "Profile Selected:"
        void $ label (T.pack (show (profileOpt m)))
        void $ label "Text Input:"
        void $ label (textVal m)
        void $ label "Total Clicks:"
        void $ label (T.pack (show (totalClicks m)))

      void $ separator

      void $ label "Live Unicode Progress Bars:"
      gridWith 2 (gap 4) $ do
        let makeBar pct =
              let filled = max 0 (min 20 (pct `div` 5))
                  empty  = 20 - filled
               in T.replicate filled "█" <> T.replicate empty "░"
            volPct = round (volumeVal m * 100) :: Int
            opPct  = round (opacityVal m * 100) :: Int
        void $ label "Master Volume:"
        void $ label ("[" <> makeBar volPct <> "] " <> T.pack (show volPct) <> "%")
        void $ label "Surface Opacity:"
        void $ label ("[" <> makeBar opPct  <> "] " <> T.pack (show opPct) <> "%")

-- | Tab 2: Unicode & Icon Gallery
viewGalleryTab :: NanoUI ()
viewGalleryTab = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    void $ label "COZETTE EMBEDDED BITMAP FONT // UNICODE SHOWCASE"
    void $ separator

    gridWith 1 (gap 8) $ do
      gridWith 1 (gap 2) $ do
        void $ label "ASCII Printable Characters:"
        void $ label "!\"#$%&'()*+,-./0123456789:;<=>?"
        void $ label "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
        void $ label "`abcdefghijklmnopqrstuvwxyz{|}~"

      void $ separator

      gridWith 1 (gap 2) $ do
        void $ label "Greek Letters & Physics Variables:"
        void $ label "Δ Ω Σ α β γ δ ε θ λ μ π ρ τ ω"

      gridWith 1 (gap 2) $ do
        void $ label "Mathematical & Logic Operators:"
        void $ label "± × ÷ √ ∞ ≤ ≥ ≠ ≈ ≡ ∀ ∃ ∈ ∉ ∧ ∨ ∂ ∇"

      gridWith 1 (gap 2) $ do
        void $ label "Box Drawing & Frame Elements:"
        void $ label "┌───┬───┐  ╔═══╦═══╗  ┏━━━┳━━━┓"
        void $ label "│ A │ B │  ║ X ║ Y ║  ┃ 1 ┃ 2 ┃"
        void $ label "├───┼───┤  ╠═══╬═══╣  ┣━━━╋━━━┫"
        void $ label "│ C │ D │  ║ Z ║ W ║  ┃ 3 ┃ 4 ┃"
        void $ label "└───┴───┘  ╚═══╩═══╝  ┗━━━┻━━━┛"

      gridWith 1 (gap 2) $ do
        void $ label "Block Elements & Shading Meters:"
        void $ label "█ ▓ ▒ ░ ▀ ▄ ▌ ▐ ▖ ▗ ▘ ▙ ▚ ▛ ▜ ▝ ▞ ▟"

      gridWith 1 (gap 2) $ do
        void $ label "Keycaps & Modifiers:"
        void $ label "⏎ Enter  ⇥ Tab  ⌃ Ctrl  ⌥ Alt  ⌘ Cmd  ⌫ Bksp  ⎋ Esc"

      void $ separator

      gridWith 1 (gap 4) $ do
        void $ label "Nerd Font & UI Icon Buttons (4-Column Native Grid):"
        gridWith 4 (gap 4 . fixedH 24) $ do
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
    void $ label "LEAN BACKEND ARCHITECTURE & DESIGN PRINCIPLES"
    void $ separator

    gridWith 1 (gap 6) $ do
      void $ label "1. Single-Pass O(N) Linear Grid & Flex Layout Engine:"
      void $ label "   - Native multi-column 2D grids (gridWith N) with automatic column & row distribution."
      void $ label "   - Zero backtracking, zero flex equations, zero quadratic passes."
      void $ label "   - Direct contiguous allocation in unboxed PrimArray."
      void $ label "   - Strictly clamps child nodes to parent remaining bounds."

      void $ separator

      void $ label "2. Exact 1:1 Collision-Box Theming:"
      void $ label "   - Visual geometry matches collision/hit-test bounds exactly."
      void $ label "   - Zero rounded corners, zero soft drop-shadows, zero bloat."
      void $ label "   - Pure color themes: Tomorrow Min Light, Night, and Midnight."

      void $ separator

      void $ label "3. Embedded Cozette Bitmap Typography:"
      void $ label "   - 18,492-byte pruned OpenType bitmap font (.otb) embedded in binary."
      void $ label "   - 921 custom glyphs (ASCII, Greek, Math, Box, Powerline, Nerd icons)."
      void $ label "   - Uniform 6px cell width, 13px line height, 10px ascent."
      void $ label "   - Direct 1-bit to 32-bit software blitter with zero FreeType dependency."

      void $ separator

      void $ label "4. Integer DPI Scaling:"
      void $ label "   - Integer scaling factors (1x, 2x, 3x...)."
      void $ label "   - Logical UI coordinates mapped with exact integer floor division."
      void $ label "   - High-throughput nearest-neighbor pixel replication preserving crispness."

-- | Tab 4: Diagnostics
viewDiagnosticsTab :: Model -> NanoUI ()
viewDiagnosticsTab m = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    void $ label "SYSTEM DIAGNOSTICS & TELEMETRY"
    void $ separator

    gridWith 1 (gap 6) $ do
      void $ label "Window & Surface Telemetry (2-Column Property Grid):"
      let !sc = dpiScale m
          !physScale = physScaleFor sc
          !physW = 1680 :: Int
          !physH = 1040 :: Int
          !effScale = if physScale > 0.0 then physScale else 1.0
          !logW = round (fromIntegral physW / effScale) :: Int
          !logH = round (fromIntegral physH / effScale) :: Int

      gridWith 2 (gap 4) $ do
        void $ label "Physical Window Size:"
        void $ label (T.pack (show physW) <> " x " <> T.pack (show physH) <> " px")
        void $ label "DPI Scale Choice:"
        void $ label (formatDpiScale sc <> (if physScale <= 0.0 then " (OS Native DPI)" else " (" <> T.pack (show physScale) <> "x DPI)"))
        void $ label "Logical Viewport Size:"
        void $ label (T.pack (show logW) <> " x " <> T.pack (show logH) <> " px")
        void $ label "Framebuffer Bit Depth:"
        void $ label "32-bit BGRA (Software DIBSection)"
        void $ label "Physical RAM Surface:"
        void $ label (T.pack (show (physW * physH * 4 `div` 1024)) <> " KB")
        void $ label "Target Frame Rate:"
        void $ label "120 FPS max pacing"

      void $ separator

      gridWith 2 (gap 4) $ do
        void $ label "Active Tab:"
        void $ label (T.pack (show (activeTab m)))
        void $ label "Current Theme:"
        void $ label (T.pack (show (currentTheme m)))
        void $ label "Interaction Clicks:"
        void $ label (T.pack (show (totalClicks m)))
        void $ label "Compiler Toolchain:"
        void $ label "Zig C Compiler (zig cc)"
        void $ label "Layout Paradigm:"
        void $ label "Native Multi-Column 2D Grid"

      void $ separator

      void $ label "Floating Diagnostics Window:"
      diagBtn <- button (if debugOpen m then "[Close Debug Window]" else "[Open Floating Debug Window (FPS, Timing, Arena, RTS)]")
      when (respClicked diagBtn) (emit (ToggleDebug (not (debugOpen m))))

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
