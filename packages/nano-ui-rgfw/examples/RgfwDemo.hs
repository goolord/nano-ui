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
  , columnWith
  , contextMenu
  , emit
  , fillH
  , fillW
  , fixedH
  , fixedW
  , flex
  , gap
  , label
  , menuHeader
  , menuItem
  , menuItemDisabled
  , menuItemWithShortcut
  , menuSeparator
  , padAll
  , panelWith
  , rowWith
  , separator
  , slider
  , textArea
  , textInput
  , respClicked
  , respChanged
  )
import NanoUI.Backend.Rgfw
  ( RgfwOptions (..)
  , RgfwTheme (..)
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
  = DpiScale05
  | DpiScale1
  | DpiScale2
  | DpiScale3
  deriving (Bounded, Enum, Eq, Show)

formatDpiScale :: DpiScaleChoice -> Text
formatDpiScale DpiScale05 = "0.5x"
formatDpiScale DpiScale1  = "1x"
formatDpiScale DpiScale2  = "2x"
formatDpiScale DpiScale3  = "3x"

physScaleFor :: DpiScaleChoice -> Int
physScaleFor DpiScale05 = 1
physScaleFor DpiScale1  = 2
physScaleFor DpiScale2  = 4
physScaleFor DpiScale3  = 6

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
  }
  deriving (Eq, Show)

initialModel :: Model
initialModel =
  Model
    { activeTab    = TabControls
    , currentTheme = ThemeNight
    , dpiScale     = DpiScale1
    , counter      = 42
    , turboOn      = True
    , volumeVal    = 0.72
    , opacityVal   = 0.90
    , textVal      = "nano-ui rgfw edition"
    , notesVal     = "Lean single-pass backend\nBitmap Cozette typography\nTomorrow Min themes"
    , profileOpt   = ProfileBalanced
    , totalClicks  = 0
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
                DpiScale05 -> DpiScale1
                DpiScale1  -> DpiScale2
                DpiScale2  -> DpiScale3
                DpiScale3  -> DpiScale05
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

    -- Tab Bar
    rowWith (gap 4 . fixedH 24) $ do
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

-- | Tab 1: Controls
viewControlsTab :: Model -> NanoUI ()
viewControlsTab m = do
  rowWith (gap 12 . fillW . fillH) $ do
    -- Left Column: Interactive Form Controls
    panelWith (padAll 10 . gap 6 . fixedW 380 . fillH) $ do
      void $ label "WIDGET CONTROLS"
      void $ separator

      -- Counter
      rowWith (gap 6 . fixedH 22) $ do
        void $ label ("Counter: " <> T.pack (show (counter m)))
        incBtn <- button " +1 "
        when (respClicked incBtn) (emit Increment)
        decBtn <- button " -1 "
        when (respClicked decBtn) (emit Decrement)
        rstBtn <- button " Reset "
        when (respClicked rstBtn) (emit Reset)

      -- Checkbox
      rowWith (gap 6 . fixedH 20) $ do
        (cbResp, cbVal) <- checkbox "Enable turbo execution mode" (turboOn m)
        when (respClicked cbResp || respChanged cbResp) (emit (ToggleTurbo cbVal))

      -- Context Menu
      rowWith (gap 6 . fixedH 22) $ do
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
      columnWith (gap 2) $ do
        let volPct = round (volumeVal m * 100) :: Int
        void $ label ("Master Volume: " <> T.pack (show volPct) <> "%")
        (slResp, slVal) <- slider "Vol" 0 1 (volumeVal m)
        when (respChanged slResp) (emit (SetVolume slVal))

      columnWith (gap 2) $ do
        let opPct = round (opacityVal m * 100) :: Int
        void $ label ("Surface Opacity: " <> T.pack (show opPct) <> "%")
        (opResp, opVal) <- slider "Opacity" 0 1 (opacityVal m)
        when (respChanged opResp) (emit (SetOpacity opVal))

      -- Text Input
      columnWith (gap 2) $ do
        void $ label "Single-line Text Input:"
        (tiResp, tiVal) <- textInput "Input" (textVal m)
        when (respChanged tiResp) (emit (SetInputText tiVal))

      -- Text Area
      columnWith (gap 2) $ do
        rowWith (gap 4 . fixedH 18) $ do
          void $ label "Multi-line Notes Field:"
          void $ flex
          clrBtn <- button "Clear"
          when (respClicked clrBtn) (emit ClearNotes)
        (taResp, taVal) <- textArea "Notes" (notesVal m)
        when (respChanged taResp) (emit (SetNotesText taVal))

      -- Radio Buttons
      columnWith (gap 2) $ do
        (radResp, radVal) <- boundedRadioFieldset "Preset" (profileOpt m) $ \case
          ProfileFast     -> "Fast (Low Latency)"
          ProfileBalanced -> "Balanced (Standard)"
          ProfileQuality  -> "Quality (High Detail)"
        when (respChanged radResp) (emit (SetProfile radVal))

    -- Right Column: Live State Inspector & Visualizer
    panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
      void $ label "STATE INSPECTOR & METERS"
      void $ separator

      columnWith (gap 6) $ do
        void $ label ("Active Theme:    " <> T.pack (show (currentTheme m)))
        void $ label ("DPI Scale:       " <> formatDpiScale (dpiScale m) <> " (" <> T.pack (show (physScaleFor (dpiScale m))) <> "x physical)")
        void $ label ("Counter Value:   " <> T.pack (show (counter m)))
        void $ label ("Turbo Mode:      " <> if turboOn m then "[ENABLED]" else "[DISABLED]")
        void $ label ("Volume Slider:   " <> T.pack (show (round (volumeVal m * 100) :: Int)) <> "%")
        void $ label ("Opacity Slider:  " <> T.pack (show (round (opacityVal m * 100) :: Int)) <> "%")
        void $ label ("Profile Selected:" <> T.pack (show (profileOpt m)))
        void $ label ("Text Input:      " <> textVal m)
        void $ label ("Total Clicks:    " <> T.pack (show (totalClicks m)))

      void $ separator

      void $ label "Live Unicode Progress Bars:"
      columnWith (gap 4) $ do
        let makeBar pct =
              let filled = max 0 (min 20 (pct `div` 5))
                  empty  = 20 - filled
               in T.replicate filled "█" <> T.replicate empty "░"
            volPct = round (volumeVal m * 100) :: Int
            opPct  = round (opacityVal m * 100) :: Int
        void $ label ("Vol: [" <> makeBar volPct <> "] " <> T.pack (show volPct) <> "%")
        void $ label ("Opa: [" <> makeBar opPct  <> "] " <> T.pack (show opPct) <> "%")

-- | Tab 2: Unicode & Icon Gallery
viewGalleryTab :: NanoUI ()
viewGalleryTab = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    void $ label "COZETTE EMBEDDED BITMAP FONT // UNICODE SHOWCASE"
    void $ separator

    columnWith (gap 8) $ do
      columnWith (gap 2) $ do
        void $ label "ASCII Printable Characters:"
        void $ label "!\"#$%&'()*+,-./0123456789:;<=>?"
        void $ label "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
        void $ label "`abcdefghijklmnopqrstuvwxyz{|}~"

      void $ separator

      columnWith (gap 2) $ do
        void $ label "Greek Letters & Physics Variables:"
        void $ label "Δ Ω Σ α β γ δ ε θ λ μ π ρ τ ω"

      columnWith (gap 2) $ do
        void $ label "Mathematical & Logic Operators:"
        void $ label "± × ÷ √ ∞ ≤ ≥ ≠ ≈ ≡ ∀ ∃ ∈ ∉ ∧ ∨ ∂ ∇"

      columnWith (gap 2) $ do
        void $ label "Box Drawing & Frame Elements:"
        void $ label "┌───┬───┐  ╔═══╦═══╗  ┏━━━┳━━━┓"
        void $ label "│ A │ B │  ║ X ║ Y ║  ┃ 1 ┃ 2 ┃"
        void $ label "├───┼───┤  ╠═══╬═══╣  ┣━━━╋━━━┫"
        void $ label "│ C │ D │  ║ Z ║ W ║  ┃ 3 ┃ 4 ┃"
        void $ label "└───┴───┘  ╚═══╩═══╝  ┗━━━┻━━━┛"

      columnWith (gap 2) $ do
        void $ label "Block Elements & Shading Meters:"
        void $ label "█ ▓ ▒ ░ ▀ ▄ ▌ ▐ ▖ ▗ ▘ ▙ ▚ ▛ ▜ ▝ ▞ ▟"

      columnWith (gap 2) $ do
        void $ label "Keycaps & Modifiers:"
        void $ label "⏎ Enter  ⇥ Tab  ⌃ Ctrl  ⌥ Alt  ⌘ Cmd  ⌫ Bksp  ⎋ Esc"

      columnWith (gap 2) $ do
        void $ label "Nerd Font & UI Icons:"
        void $ label "\xF002 Search  \xF004 Health  \xF005 Star  \xF00C Check  \xF00D Close  \xF013 Settings  \xF01E Reload"
        void $ label "\xF026 Mute    \xF028 Sound   \xF04B Play   \xF04C Pause  \xF04D Stop   \xF188 Debug     \xF11B Gamepad"

-- | Tab 3: Architecture
viewArchitectureTab :: NanoUI ()
viewArchitectureTab = do
  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    void $ label "LEAN BACKEND ARCHITECTURE & DESIGN PRINCIPLES"
    void $ separator

    columnWith (gap 6) $ do
      void $ label "1. Single-Pass O(N) Linear Preorder Layout Engine:"
      void $ label "   - Zero flex equations, zero backtracking, zero quadratic passes."
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

    columnWith (gap 6) $ do
      void $ label "Window & Surface Telemetry:"
      let !sc = dpiScale m
          !physScale = physScaleFor sc
          !physW = 1680
          !physH = 1040
          !logW = physW `div` physScale
          !logH = physH `div` physScale
      void $ label ("  Physical Window Size:  " <> T.pack (show physW) <> " x " <> T.pack (show physH) <> " px")
      void $ label ("  DPI Scale Choice:      " <> formatDpiScale sc <> " (Physical " <> T.pack (show physScale) <> "x DPI)")
      void $ label ("  Logical Viewport Size: " <> T.pack (show logW) <> " x " <> T.pack (show logH) <> " px")
      void $ label ("  Framebuffer Bit Depth: 32-bit BGRA (Windows DIBSection)")
      void $ label ("  Physical RAM Surface:  " <> T.pack (show (physW * physH * 4 `div` 1024)) <> " KB")
      void $ label ("  Target Frame Rate:     120 FPS max pacing")

      void $ separator

      void $ label "Runtime State:"
      void $ label ("  Active Tab:            " <> T.pack (show (activeTab m)))
      void $ label ("  Current Theme:         " <> T.pack (show (currentTheme m)))
      void $ label ("  Interaction Clicks:    " <> T.pack (show (totalClicks m)))
      void $ label ("  Compiler Toolchain:    Zig C Compiler (zig cc)")

main :: IO ()
main = do
  let opts =
        defaultRgfwOptions
          { optTitle  = "nano-ui [RGFW Lean Backend // Tomorrow Min]"
          , optWidth  = 1680
          , optHeight = 1040
          , optTheme  = defaultDarkTheme
          , optScale  = 1
          }
  runRgfwAppReduceCustom opts (\m -> (themeForChoice (currentTheme m), physScaleFor (dpiScale m))) update initialModel appView
