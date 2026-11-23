-- | The glossary's specimen data: per-widget style cards, typography demos,
-- unexpected interactions, the state strip and the hero banner, each
-- rendered to SVG through "Glossary.Render".
module Glossary.Specimens
  ( InteractionCard (..)
  , ProbeRow (..)
  , ToggleNote (..)
  , TypoDemo (..)
  , TypoSectionData (..)
  , generateInteractions
  , generateProbeMatrix
  , generateRainbowBanner
  , generateStateStrip
  , generateTypoDemos
  , impactBadgeText
  , impactCode
  ) where

import Control.Monad (forM, void)
import Data.Aeson.Micro (ToJSON (..), Value, object, (.=))
import Data.Bits (testBit, (.&.))
import Data.Text (Text)
import Glossary.Render
  ( canvasSlack
  , exportFrame
  , framePad
  , newSvgContext
  , packSvgBank
  , probeAccent
  , probeButton
  , probeInput
  , probePanel
  , probePopup
  , renderStyleBank
  , renderSvg
  , renderSvgClick
  , renderSvgHover
  , renderSvgPress
  )
import NanoUI
import NanoUI.Testing (runFrame)
import NanoUI.Testing.Harness (withInputOff)

-- ---------------------------------------------------------------------------
-- Section II: Interactive Per-Widget Style Workbench Cards (15 Families)
-- ---------------------------------------------------------------------------

data ImpactKind
  = ImpactPrimary
  | ImpactPartial
  | ImpactSurprise
  | ImpactNoOp

impactCode :: ImpactKind -> Text
impactCode = \case
  ImpactPrimary -> "primary"
  ImpactPartial -> "partial"
  ImpactSurprise -> "surprise"
  ImpactNoOp -> "noop"

impactBadgeText :: ImpactKind -> Text
impactBadgeText = \case
  ImpactPrimary -> "Primary"
  ImpactPartial -> "Partial"
  ImpactSurprise -> "Edge case"
  ImpactNoOp -> "No-op"

data ToggleNote = ToggleNote
  { tnBit :: !Int
  , tnKey :: !Text
  , tnLabel :: !Text
  , tnShort :: !Text
  , tnImpact :: !ImpactKind
  , tnDetail :: !Text
  }

mkToggleNotes :: [(ImpactKind, Text)] -> [ToggleNote]
mkToggleNotes specs =
  [ ToggleNote bit key lbl short imp det
  | ((bit, key, lbl, short), (imp, det)) <- zip meta specs
  ]
  where
    meta :: [(Int, Text, Text, Text)]
    meta =
      [ (0, "light", "defaultLightTheme", "Light Theme")
      , (1, "btn", "buttonStyle", "buttonStyle")
      , (2, "inp", "inputStyle", "inputStyle")
      , (3, "pnl", "panelStyle", "panelStyle")
      , (4, "acc", "accentColor", "accentColor")
      , (5, "rnd", "rounded 999", "rounded 999")
      , (6, "dis", "disabledWhen True", "disabledWhen")
      ]

data ProbeRow = ProbeRow
  { prId :: !Text
  , prIsOverlay :: !Bool
  , prCategory :: !Text
  , prTitle :: !Text
  , prSignature :: !Text
  , prPrimarySurface :: !Text
  , prSummary :: !Text
  , prCodeExpr :: !Text
  , prToggles :: ![ToggleNote]
  , prBaselineSvg :: !Text
  , prBankValue :: !Value
  }

buildProbeRow ::
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Bool ->
  Float ->
  Float ->
  [(ImpactKind, Text)] ->
  NanoUI () ->
  IO ProbeRow
buildProbeRow rid cat title sig surf summary codeExpr isOverlay w h notes view = do
  (baseSvg, bankVal) <- renderStyleBank rid w (h + canvasSlack) isOverlay (framePad view)
  pure
    ProbeRow
      { prId = rid
      , prIsOverlay = isOverlay
      , prCategory = cat
      , prTitle = title
      , prSignature = sig
      , prPrimarySurface = surf
      , prSummary = summary
      , prCodeExpr = codeExpr
      , prToggles = mkToggleNotes notes
      , prBaselineSvg = baseSvg
      , prBankValue = bankVal
      }

generateProbeMatrix :: IO [ProbeRow]
generateProbeMatrix =
  sequence
    [ buildProbeRow
        "button"
        "surface-button"
        "button (normal, subtle)"
        "button :: Text -> NanoUI Bool"
        "themeButton"
        "Reads `styleBg`, `styleHoverBg`, `styleActiveBg`, `styleFg`, `styleBorder`, `styleBorderWidth`, and `styleCornerRadius` from `themeButton`. Focus adds a 2px `themeFocusRing` outline."
        "rowWith (gap 8) $ button \"Save\" >> styled subtle (button \"Subtle\")"
        False
        236
        54
        [ (ImpactPrimary, "Switches the base theme to `defaultLightTheme` before scoped modifiers run.")
        , (ImpactPrimary, "Colors `Save` (`styleBg`, `styleBorder`, `styleFg`). `Subtle` keeps only its text color because its inner `styled subtle` clears the background and border.")
        , (ImpactNoOp, "No effect: buttons do not read `themeInput`.")
        , (ImpactNoOp, "No effect: buttons do not read `themePanel`.")
        , (ImpactNoOp, "No effect: plain `button` ignores `themeAccent`. Use `styled primary` or `toggleButton` instead.")
        , (ImpactPrimary, "Sets `styleCornerRadius` to 999, turning the button into a pill (`rx` = half the height).")
        , (ImpactPrimary, "Dims the fill, border, and label, and blocks clicks and focus.")
        ]
        ( rowWith (gap 8 . alignMid) $ do
            void (button "Save")
            void (styled subtle (button "Subtle"))
        )
    , buildProbeRow
        "toggle-button"
        "surface-button"
        "toggleButton off and on"
        "toggleButton :: Tone -> Text -> Var Bool -> NanoUI ()"
        "themeButton + Tone"
        "While off, reads `themeButton` so `buttonStyle` applies. While on, replaces the fill, hover fill, and border with `toneColor`, and picks contrasting text with `readableOn`, overriding `buttonStyle` colors."
        "toggleButton Accent \"Off\" vOff >> toggleButton Accent \"On\" vOn"
        False
        236
        54
        [ (ImpactPrimary, "Switches the base theme. `readableOn` recomputes the active text color against the tone fill.")
        , (ImpactPartial, "Styles the off state only. While on, `toneColor` replaces the fill, hover fill, and border, and `readableOn` sets the text color.")
        , (ImpactNoOp, "No effect: `toggleButton` does not read `themeInput`.")
        , (ImpactNoOp, "No effect: `toggleButton` does not read `themePanel`.")
        , (ImpactPrimary, "Colors the active state when the tone is `Accent`; `readableOn` picks dark or light text.")
        , (ImpactPrimary, "Rounds both states into pills. The active state keeps `styleCornerRadius` from `themeButton`.")
        , (ImpactPrimary, "Dims both states and blocks toggling.")
        ]
        ( rowWith (gap 8 . alignMid) $ do
            vOff <- useVar False
            vOn <- useVar True
            toggleButton Accent "Off (False)" vOff
            toggleButton Accent "On (True)" vOn
        )
    , buildProbeRow
        "checkbox"
        "surface-button surface-input"
        "checkbox"
        "checkbox :: Text -> Var Bool -> NanoUI ()"
        "themeButton + themeInput + themeAccent"
        "The label and unchecked border come from `themeButton` (`styleFg`, `styleBorder`), the unchecked interior comes from `themeInput` (`styleBg`), and the checked fill uses `themeAccent` and `themeOnAccent`."
        "checkbox \"Unchecked\" vOff >> checkbox \"Checked\" vOn"
        False
        236
        68
        [ (ImpactPrimary, "Switches the base theme to light unchecked boxes and dark labels.")
        , (ImpactPartial, "Colors the label and unchecked border. The button background is cleared so no button frame draws behind the checkbox.")
        , (ImpactPartial, "Colors only the 14x14 unchecked interior.")
        , (ImpactNoOp, "No effect: `checkbox` does not read `themePanel`.")
        , (ImpactPrimary, "Colors the checked box fill and border; the checkmark uses `themeOnAccent`.")
        , (ImpactSurprise, "Ignored: checkbox corners use a fixed small radius proportional to box size.")
        , (ImpactPrimary, "Dims the boxes, checkmark, and label.")
        ]
        ( columnWith (gap 4 . tight) $ do
            vOff <- useVar False
            vOn <- useVar True
            checkbox "Unchecked" vOff
            checkbox "Checked" vOn
        )
    , buildProbeRow
        "radio"
        "surface-button surface-input"
        "radio"
        "radio :: Foldable f => f Text -> Var Int -> NanoUI ()"
        "themeButton + themeInput + themeAccent"
        "Follows the same three-surface split as `checkbox`: the unselected ring interior uses `themeInput`, the ring border and label use `themeButton`, and the selected ring and dot use `themeAccent`."
        "radio [\"Option A\", \"Option B\"] vSelected"
        False
        236
        68
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactPartial, "Colors the labels and unselected ring borders; the button background is cleared.")
        , (ImpactPartial, "Colors only the unselected ring interior.")
        , (ImpactNoOp, "No effect: `radio` does not read `themePanel`.")
        , (ImpactPrimary, "Colors the selected ring and inner dot.")
        , (ImpactSurprise, "No effect: radio indicators are always circular.")
        , (ImpactPrimary, "Dims the rings, dot, and labels.")
        ]
        ( columnWith (gap 4 . tight) $ do
            vSel <- useVar (1 :: Int)
            radio ["Option A (Unselected)" :: Text, "Option B (Selected)"] vSel
        )
    , buildProbeRow
        "text-input"
        "surface-input"
        "textInput"
        "textInput :: Var Text -> NanoUI ()"
        "themeInput"
        "`themeInput` paints the well, text and border. The placeholder blends 40% toward the well fill. On focus the border becomes `themeAccent` and the fill becomes `styleActiveBg`, with no outer ring."
        "textInputConfigured (defaultTextInputConfig {ticPlaceholder = \"Filter...\"}) vEmpty >> textInput vText"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: `textInput` reads `themeInput`.")
        , (ImpactPrimary, "Colors the well fill, border, value text and placeholder.")
        , (ImpactNoOp, "No effect: `textInput` never reads `themePanel`.")
        , (ImpactPartial, "Used for the focused border and caret.")
        , (ImpactPrimary, "Turns the well into a pill.")
        , (ImpactPrimary, "Dims the well, border and text, and blocks focus and typing.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vEmpty <- useVar ("" :: Text)
            vText <- useVar ("nano-ui" :: Text)
            textInputConfigured (defaultTextInputConfig {ticPlaceholder = "Filter..."}) vEmpty
            textInput vText
        )
    , buildProbeRow
        "search-numeric"
        "surface-input"
        "searchInput and numericInput"
        "searchInput :: Text -> Var Text -> NanoUI ()"
        "themeInput"
        "Both use `themeInput`. `searchInput` tints its magnifier and clear glyphs 45% toward the well fill. `numericInput` tints its divider 40% from the border and its stepper arrows 40% from the text color."
        "searchInput \"Search\" vQuery >> numericInput vNum"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect. The stepper buttons in `numericInput` are painted from `themeInput`, not `themeButton`.")
        , (ImpactPrimary, "Colors both fields. `searchInput` tints its icons 45% toward the well fill; `numericInput` tints its divider and arrows 40% toward the well fill.")
        , (ImpactNoOp, "No effect: neither input reads `themePanel`.")
        , (ImpactPartial, "Used for the focused border and caret.")
        , (ImpactPrimary, "Turns both wells into pills.")
        , (ImpactPrimary, "Dims the wells, icons, arrows and text.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vQ <- useVar ("plot" :: Text)
            vN <- useVar (42.5 :: Double)
            searchInput "Search" vQ
            numericInput vN
        )
    , buildProbeRow
        "select-vs-combo"
        "surface-button surface-input"
        "select and comboBox"
        "select :: f Text -> Var Int -> NanoUI ()"
        "select: themeButton | comboBox: themeInput"
        "`select` paints its closed trigger from `themeButton`, while `comboBox` is built on `textInput` and paints from `themeInput`. Both draw their open dropdown menus from `themePopup`."
        "selectWith fillW [\"Select (btn)\", ...] vIdx >> comboBox \"Pick...\" [...] vCombo"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme on both controls.")
        , (ImpactPartial, "Styles `select` only. `comboBox` reads `themeInput` and ignores `buttonStyle`.")
        , (ImpactPartial, "Styles `comboBox` only. `select` reads `themeButton` and ignores `inputStyle`.")
        , (ImpactNoOp, "No effect while closed; open menus read `themePopup`.")
        , (ImpactPartial, "Highlights the selected row in an open menu and the focused `comboBox` border.")
        , (ImpactPrimary, "Rounds `select` through `themeButton` and `comboBox` through `themeInput`.")
        , (ImpactPrimary, "Dims both controls and prevents opening their menus.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vIdx <- useVar (0 :: Int)
            vCombo <- useVar ("GHC 9.14" :: Text)
            selectWith fillW ["Select (themeButton)" :: Text, "Option 2"] vIdx
            comboBox "Pick..." ["GHC 9.14" :: Text, "GHC 9.12"] vCombo
        )
    , buildProbeRow
        "textarea"
        "surface-input"
        "textArea"
        "textArea :: Var Text -> NanoUI ()"
        "themeInput"
        "Multi-line editor on `themeInput`. Focus swaps the border to `themeAccent` and the fill to `styleActiveBg`; selections use `themeSelection`."
        "textAreaWith (fillW . fixedH 64) vDoc"
        False
        236
        84
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: `textArea` reads `themeInput`.")
        , (ImpactPrimary, "Colors the well, border and document text.")
        , (ImpactNoOp, "No effect: `textArea` never reads `themePanel`.")
        , (ImpactPartial, "Used for the active border and caret.")
        , (ImpactPrimary, "Rounds the editor corners.")
        , (ImpactPrimary, "Dims the well and text, and blocks editing.")
        ]
        ( do
            vDoc <- useVar ("line 1: module Main\nline 2: import NanoUI" :: Text)
            textAreaWith (fillW . fixedH 64) vDoc
        )
    , buildProbeRow
        "slider"
        "surface-input"
        "slider"
        "slider :: Float -> Float -> Var Float -> NanoUI ()"
        "themeInput + themeAccent"
        "The track outline and thumb border use `styleBorder` from `themeInput`. The unfilled track blends 35% from `styleBg` toward the border, the active fill uses `themeAccent`, and the thumb interior uses `themeOnAccent`. Border width is fixed at 1px and track radius at 3px."
        "sliderWith fillW 0 100 vVal"
        False
        236
        54
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: `slider` reads `themeInput` and `themeAccent`.")
        , (ImpactPartial, "Colors the track outline and thumb border, and blends the unfilled track 35% from `styleBg` toward the border.")
        , (ImpactNoOp, "No effect: `slider` never reads `themePanel`.")
        , (ImpactPrimary, "Colors the filled track; the thumb interior uses `themeOnAccent`.")
        , (ImpactSurprise, "Ignored: slider tracks always use a 3px corner radius and 1px border.")
        , (ImpactPrimary, "Dims the track, fill, and thumb.")
        ]
        ( do
            vVal <- useVar (64 :: Float)
            sliderWith fillW 0 100 vVal
        )
    , buildProbeRow
        "custom-controls"
        "surface-button surface-panel"
        "toggleSwitch, progressBar, sparkline"
        "toggleSwitch :: Var Bool -> NanoUI ()"
        "themeButton / themePanel + themeAccent"
        "Canvas widgets from `NanoUI.Widgets.Custom`. `toggleSwitch`, `progressBar`, `circularProgress` and `knob` read `themeButton` plus `themeAccent`; `sparkline` fills its well with `themePanel`'s `styleBg`."
        "toggleSwitch vSw >> progressBar 0.68 >> sparkline [2, 7, 4, 9, 5, 11, 8, 12]"
        False
        236
        94
        [ (ImpactPrimary, "Switches the base theme for all three canvas widgets.")
        , (ImpactPartial, "Colors the track border and unfilled track of `toggleSwitch` and `progressBar`; `sparkline` ignores it.")
        , (ImpactNoOp, "No effect: these widgets never read `themeInput`.")
        , (ImpactPartial, "Colors only the `sparkline` well; `toggleSwitch` and `progressBar` ignore it.")
        , (ImpactPrimary, "Colors the switch pill, progress fill, and sparkline line and dot.")
        , (ImpactSurprise, "Ignored. The switch and progress bar use `r = h/2`; the sparkline uses `r = 3`.")
        , (ImpactPrimary, "Dims `cdcTheme` in `CustomDrawContext` for all three.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            vSw <- useVar True
            toggleSwitch vSw
            progressBar 0.68
            sparkline [2, 7, 4, 9, 5, 11, 8, 12]
        )
    , buildProbeRow
        "panel-card-callout"
        "surface-panel"
        "panel, card, callout"
        "panel :: NanoUI a -> NanoUI a"
        "themePanel"
        "`panel` and `card` read `styleBg`, `styleBorder`, `styleBorderWidth`, `styleBorderSides` and `styleCornerRadius` from `themePanel`. `callout` applies a tinted `panelStyle` to its whole subtree."
        "panel (label \"panel surface\") >> callout amber (label \"Warning callout\")"
        False
        236
        94
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: containers read `themePanel`.")
        , (ImpactNoOp, "No effect: containers never read `themeInput`.")
        , (ImpactPrimary, "Colors the panel fill, border and child labels. `callout` applies its own tinted `panelStyle` on top.")
        , (ImpactNoOp, "No effect when `callout` uses an explicit color; `calloutTone Accent` binds `themeAccent`.")
        , (ImpactPrimary, "Rounds the panel and callout corners.")
        , (ImpactPrimary, "Dims the surfaces, borders and labels.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            panelWith (fillW . padAll 6) (label "panel surface")
            calloutWith (colorRGBA 235 179 56 255) (fillW . padAll 6) (label "Warning callout")
        )
    , buildProbeRow
        "scroll-bounded"
        "surface-input"
        "scrollWith (bounded)"
        "scrollWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a"
        "themeInput (cornerRadius = 0)"
        "When either axis has a `Fixed` or `Fit` size, `scrollWith` draws a bordered `themeInput` well with square corners (`cornerRadius = 0`). The scrollbar blends `themeInput` with `themeSeparator`."
        "scrollWith (fillW . fixedH 64 . padAll 6) $ label \"Row 1\" >> ..."
        False
        236
        82
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect: scroll wells do not read `themeButton`.")
        , (ImpactPrimary, "Surprise: bounded scroll wells read `themeInput`, not `themePanel`, and blend the scrollbar from it.")
        , (ImpactNoOp, "Surprise no-op: the scroll well ignores `panelStyle`; only child labels read `styleFg` from `themePanel`.")
        , (ImpactNoOp, "No effect: the scrollbar blends `themeInput` and `themeSeparator`.")
        , (ImpactSurprise, "Ignored: bounded scroll wells always draw with square corners (`cornerRadius = 0`).")
        , (ImpactPrimary, "Dims the well, scrollbar, and child labels.")
        ]
        ( scrollWith (fillW . fixedH 64 . padAll 6 . gap 4) $ do
            label "Row 1 in scroll well"
            label "Row 2 in scroll well"
            label "Row 3 in scroll well"
            label "Row 4 in scroll well"
        )
    , buildProbeRow
        "tabs-styles"
        "surface-panel surface-button"
        "tabBar styles"
        "tabBarConfigured :: TabsConfig -> Var a -> f (Tab a b) -> NanoUI ()"
        "themePanel + themeAccent (+ themeButton)"
        "Tab headers behave like buttons, yet read `themePanel` and `themeAccent`. Only `TabSegmented` reads `styleBorder` from `themeButton` for its active segment border."
        "tabBarConfigured (defaultTabsConfig {tabsStyle = ...}) vTab [tab 0 \"Code\" (), tab 1 \"AST\" ()]"
        False
        236
        106
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactSurprise, "Ignored by `TabUnderline` and `TabPill`. Tab headers read `themePanel`; only `TabSegmented` reads `styleBorder` from `themeButton`.")
        , (ImpactNoOp, "No effect: `tabBar` does not read `themeInput`.")
        , (ImpactPrimary, "Colors inactive and active tab labels, the underline rule, and the `TabSegmented` track.")
        , (ImpactPrimary, "Colors the active indicator in `TabUnderline` and the active fill in `TabPill`.")
        , (ImpactPartial, "Rounds the `TabSegmented` track and active segment. `TabUnderline` is unaffected, and `TabPill` is always a full pill.")
        , (ImpactPrimary, "Dims the tab strip and blocks switching.")
        ]
        ( columnWith (gap 4 . fillW . tight) $ do
            vTab <- useVar (0 :: Int)
            let ts = [tab (0 :: Int) "Code" (), tab 1 "AST" ()]
            tabBarConfigured (defaultTabsConfig {tabsStyle = TabUnderline}) vTab ts
            tabBarConfigured (defaultTabsConfig {tabsStyle = TabPill}) vTab ts
            tabBarConfigured (defaultTabsConfig {tabsStyle = TabSegmented}) vTab ts
        )
    , buildProbeRow
        "table-tree"
        "surface-panel surface-button"
        "simpleTable and tree"
        "simpleTable :: [Text] -> f [Text] -> NanoUI TableResponse"
        "themePanel + themeButton + themeAccent"
        "Table headers blend 55% from `themePanel` toward `themeButton`; the sorted header uses `themeButton`'s `styleFg` and unsorted headers use `themeMuted`. Tree rows alternate `themeWindow` and `themeButton` stripes and use `themeButton`'s `styleFg`."
        "simpleTable [\"Mod\", \"Surface\"] [...] >> tree \"demo-tree\" [...] vTree"
        False
        236
        138
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactPartial, "Blends 55% into the table header fill, and colors the sorted header text, tree labels and odd tree row tint.")
        , (ImpactNoOp, "No effect: neither `simpleTable` nor `tree` reads `themeInput`.")
        , (ImpactPrimary, "Colors the table cells, grid borders, header fill and tree row fill.")
        , (ImpactPrimary, "Tints the selected tree row 25% toward `themeAccent` and draws the 2px left bar.")
        , (ImpactNoOp, "No effect: cells and rows are drawn as sharp edge-to-edge rects.")
        , (ImpactPrimary, "Dims the headers, cells, grid lines and rows.")
        ]
        ( columnWith (gap 6 . fillW . tight) $ do
            void $
              simpleTable
                ["Mod", "Surface"]
                [ ["btn", "Button"]
                , ["inp", "Input"]
                ]
            vTree <- useVar (1 :: Int)
            tree
              "demo-tree"
              [ TreeItem "Theme" [TreeItem "themeButton" []]
              ]
              vTree
        )
    , buildProbeRow
        "overlays"
        "surface-popup surface-window"
        "popup, menuItem, window"
        "window :: Var Bool -> Text -> NanoUI () -> NanoUI ()"
        "themePopup / themeFloatingWindow"
        "`modal`, `popup`, `contextMenu` and open select/combo menus use `overlayMenuStyle` (from `themePopup`, with a `themeButton` hover fallback when the popup hover fill equals its fill). `window` reads `themeFloatingWindow` and `themeSeparator`."
        "window vWin \"Inspector\" (label \"Floating window\")"
        True
        236
        106
        [ (ImpactPrimary, "Switches the base theme.")
        , (ImpactNoOp, "No effect until hover. `menuItem` falls back to `styleHoverBg` from `themeButton` only when the popup hover fill equals its fill.")
        , (ImpactNoOp, "No effect: overlays never read `themeInput`.")
        , (ImpactPrimary, "Colors the floating window chrome via `windowStyle` and popup menus via `popupStyle`.")
        , (ImpactNoOp, "No effect on standard window chrome.")
        , (ImpactPrimary, "Rounds the floating window corners.")
        , (ImpactPrimary, "Dims the window chrome, title bar and body.")
        ]
        ( do
            vWin <- useVar True
            window vWin "Inspector" $
              label "Floating window"
        )
    ]

-- ---------------------------------------------------------------------------
-- Section III: Interactive Typography Workbench & Specimens
-- ---------------------------------------------------------------------------

data TypoDemo = TypoDemo
  { tdTitle :: !Text
  , tdCode :: !Text
  , tdNote :: !Text
  , tdSvg :: !Text
  }

data TypoSectionData = TypoSectionData
  { tsdBaseSvg :: !Text
  , tsdBankValue :: !Value
  , tsdDemos :: ![TypoDemo]
  }

applyTypoMask :: Int -> Layout -> Layout
applyTypoMask mask =
  (if testBit mask 5 then fontSize 16 else id)
    . (if testBit mask 4 then fontTone Danger else id)
    . (if testBit mask 3 then fontStrike else id)
    . (if testBit mask 2 then fontUnderline else id)
    . (if testBit mask 1 then fontItalic else id)
    . (if testBit mask 0 then fontBold else id)

generateTypoDemos :: IO TypoSectionData
generateTypoDemos = do
  -- 64-combination Interactive Typography & *With Workbench (bits 0..5)
  ctx <- newSvgContext
  let w = 480
      h = 152
      inp = withInputOff w h
      viewFor mask =
        let m = applyTypoMask mask
         in panelWith (fillW . fillH . padAll 10 . gap 8) $ do
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "labelWith"
                labelWith m "Hamburgefonstiv 0123"
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "textInput"
                vTxt <- useVar ("Hamburgefonstiv 0123" :: Text)
                textInputConfigured (defaultTextInputConfig {ticLayout = m (fillW defaultLayout)}) vTxt
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "buttonWith"
                void (buttonWith (m . fillW) "Hamburgefonstiv 0123")
              rowWith (gap 8 . alignMid . fillW) $ do
                labelWith (fixedW 142 . fontMono) "buttonContent"
                void $
                  buttonContentWith fillW $
                    labelWith m "Hamburgefonstiv 0123"
  _ <- runFrame ctx inp (viewFor 0)
  typoSvgs <- forM [0 .. 63] $ \mask -> do
    fst <$> exportFrame "wb-typo" ctx inp (viewFor mask)
  let typoBaseSvg = case typoSvgs of
        (s0 : _) -> s0
        [] -> ""
      typoBankVal = packSvgBank typoSvgs

  svgLabels <-
    renderSvg "typo-labels" 480 124 $
      panelWith (fillW . fillH . padAll 10 . gap 6) $ do
        rowWith (gap 12 . alignMid) $ do
          label "label (14px)"
          heading "heading (WeightMedium)"
          labelWith (fontTone Accent) "fontTone Accent"
        rowWith (gap 12 . alignMid) $ do
          muted "muted"
          mono "mono"
          danger "danger"
          bold "bold"
          italic "italic"
          underline "underline"
        rowWith (gap 12 . alignMid) $ do
          labelWith fontStrike "fontStrike"
          labelWith (fontColor (colorRGBA 56 189 248 255) . fontBold) "fontColor + fontBold"
          kv "Key" "Value"

  svgToneMatrix <-
    renderSvg "typo-tone-matrix" 480 116 $
      panelWith (fillW . fillH . padAll 10 . gap 8) $ do
        rowWith (gap 6 . alignMid) $ do
          void (styled primary (button "primary"))
          void (styled destructive (button "destructive"))
          void (styled success (button "success"))
          void (styled warning (button "warning"))
          void (styled subtle (button "subtle"))
        rowWith (gap 6 . alignMid) $ do
          void (styled (tinted themeAccent) (button "tinted Accent"))
          void (styled (tinted themeDanger) (button "tinted Danger"))
          void (styled (tinted themeSuccess) (button "tinted Success"))
          void (styled (tinted themeWarning) (button "tinted Warning"))

  pure
    TypoSectionData
      { tsdBaseSvg = typoBaseSvg
      , tsdBankValue = typoBankVal
      , tsdDemos =
          [ TypoDemo
              "Text helpers and font modifiers"
              "label, heading, labelWith (fontTone Accent), muted, mono, danger, bold, italic, underline, fontStrike, fontColor"
              "NodeText and NodeTextInput pack font variant, tone, weight, style and decoration into styleIdx; buttons and selects pack the same above their own 32 bits."
              svgLabels
          , TypoDemo
              "Tones and tinted surfaces"
              "styled primary / destructive / success / warning / subtle / tinted themeAccent"
              "tone fills the button with the semantic color and picks readableOn for the text. tinted blends 18% of the color into the background, draws a 55% border and keeps the color for the text."
              svgToneMatrix
          ]
      }

-- ---------------------------------------------------------------------------
-- Section IV: 8 Unexpected Interactions & Edge Cases
-- ---------------------------------------------------------------------------

data InteractionCard = InteractionCard
  { icNumber :: !Text
  , icCategory :: !Text
  , icTitle :: !Text
  , icWhere :: !Text
  , icWhy :: !Text
  , icFix :: !Text
  , icCodeBad :: !Text
  , icCodeGood :: !Text
  , icLabelA :: !Text
  , icSvgA :: !Text
  , icLabelB :: !Text
  , icSvgB :: !Text
  }

generateInteractions :: IO [InteractionCard]
generateInteractions = do
  -- 1. checkbox & radio 3-surface split + modal popup shift
  i3Bad <-
    renderSvg "i3-bad" 260 120 $
      styled
        ( buttonStyle
            ( background (colorRGBA 220 38 38 255)
                . borderColor (colorRGBA 56 189 248 255)
                . foreground (colorRGBA 56 189 248 255)
            )
            . inputStyle (background (colorRGBA 4 120 87 255))
        )
        $ panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          v0 <- useVar False
          v1 <- useVar True
          checkbox "Unchecked (input bg + btn border)" v0
          checkbox "Checked (accent bg)" v1
          rSel <- useVar (0 :: Int)
          radio ["Radio ring (input bg + btn border)" :: Text] rSel
  i3Good <-
    renderSvg "i3-good" 260 120 $
      styled
        ( buttonStyle (foreground (colorRGBA 56 189 248 255) . borderColor (colorRGBA 56 189 248 255))
            . popupStyle (foreground (colorRGBA 251 191 36 255) . borderColor (colorRGBA 251 191 36 255))
        )
        $ do
          vOpen <- useVar True
          modal vOpen "Inside modal (reads themePopup)" $ do
            vC <- useVar False
            checkbox "Switched to popupStyle" vC

  -- 2. select (themeButton) vs comboBox (themeInput)
  i4Bad <-
    renderSvg "i4-bad" 260 92 $
      styled probeInput $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vCombo <- useVar ("comboBox (styled)" :: Text)
          vSel <- useVar (0 :: Int)
          comboBox "Pick..." ["comboBox (styled)" :: Text] vCombo
          selectWith fillW ["select (ignores inputStyle)" :: Text] vSel
  i4Good <-
    renderSvg "i4-good" 260 92 $
      styled (probeInput . buttonStyle (fillColor (colorRGBA 4 120 87 255) . foreground (colorRGBA 209 250 229 255) . borderColor (colorRGBA 52 211 153 255) . borderWidth 2)) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vCombo <- useVar ("comboBox (inputStyle)" :: Text)
          vSel <- useVar (0 :: Int)
          comboBox "Pick..." ["comboBox (inputStyle)" :: Text] vCombo
          selectWith fillW ["select (buttonStyle matched)" :: Text] vSel

  -- 3. scrollWith bounded (themeInput well, cornerRadius=0) vs grow (page background, no border)
  i5Bad <-
    renderSvg "i5-bad" 260 96 $
      styled (probeInput . rounded 12) $
        columnWith (fillW . fillH . padAll 8) $
          scrollWith (fillW . fixedH 80 . padAll 6 . gap 4) $ do
            label "fixedH 80: uses themeInput well"
            label "cornerRadius forced to 0"
            label "Line 3 (scrollable)"
            label "Line 4 (scrollable)"
  i5Good <-
    renderSvg "i5-good" 260 96 $
      styled (probeInput . rounded 12) $
        columnWith (fillW . fillH . padAll 8) $
          scrollWith (grow . padAll 6 . gap 4) $ do
            label "scrollWith grow (fillW . fillH):"
            label "Drops the themeInput well and border"
            label "Paints the page background instead"

  -- 4. callout leaks panelStyle into child panels
  let warnCol = colorRGBA 235 179 56 255
  i6Bad <-
    renderSvg "i6-bad" 260 104 $
      framePad $
        callout warnCol $ do
          label "Outer Warning callout"
          card (label "Nested card inherits the yellow tint")
  i6Good <-
    renderSvg "i6-good" 260 104 $
      framePad $ do
        basePanel <- themePanel <$> uiTheme
        callout warnCol $ do
          label "Outer Warning callout"
          styled (\t -> t {themePanel = basePanel}) $
            card (label "Nested card with restored themePanel")

  -- 5. toggleButton active state overwrites buttonStyle with Tone
  i7Bad <-
    renderSvg "i7-bad" 260 80 $
      styled probeButton $
        panelWith (fillW . fillH . padAll 10 . gap 8) $
          rowWith (gap 8 . alignMid) $ do
            vOff <- useVar False
            vOn <- useVar True
            toggleButton Warning "OFF (uses buttonStyle)" vOff
            toggleButton Warning "ON (Warning wins)" vOn
  i7Good <-
    renderSvg "i7-good" 260 80 $
      styled (probeButton . probeAccent) $
        panelWith (fillW . fillH . padAll 10 . gap 8) $
          rowWith (gap 8 . alignMid) $ do
            vOff <- useVar False
            vOn <- useVar True
            toggleButton Accent "OFF (buttonStyle)" vOff
            toggleButton Accent "ON (accentColor)" vOn

  -- 6. borderWidth & borderSides do not add layout padding
  i9Bad <-
    renderSvg "i9-bad" 260 80 $
      framePad $
        styled (panelStyle (borderColor (colorRGBA 244 63 94 255) . borderWidth 8)) $
          panelWith (fillW . tight) $
            label "tight + borderWidth 8 overlaps text"
  i9Good <-
    renderSvg "i9-good" 260 80 $
      framePad $
        styled (panelStyle (borderLeft 6 (colorRGBA 244 63 94 255))) $
          panelWith (fillW . padLRTB 12 8 8 8) $
            label "borderLeft 6 + padLeft 12 clears stroke"

  -- 7. Composition order (.) vs nested styled scopes
  i10Bad <-
    renderSvg "i10-bad" 260 84 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        rowWith (gap 6) $ do
          void (styled (primary . destructive) (button "primary . destructive"))
        rowWith (gap 6) $ do
          void (buttonWith (fixedW 110 . fixedW 220) "fixedW 110 wins")
  i10Good <-
    renderSvg "i10-good" 260 84 $
      panelWith (fillW . fillH . padAll 8 . gap 6) $ do
        rowWith (gap 6) $ do
          void (styled primary (styled destructive (button "nested: inner destructive wins")))

  -- 8. Tabs (tabHeaderVisualStyle) read themePanel, not themeButton
  i11Bad <-
    renderSvg "i11-bad" 260 96 $
      styled probeButton $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vT <- useVar (0 :: Int)
          let ts = [tab (0 :: Int) "Tab A" (), tab 1 "Tab B" ()]
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabUnderline}) vT ts
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabContained}) vT ts
  i11Good <-
    renderSvg "i11-good" 260 96 $
      styled (probePanel . probeAccent) $
        panelWith (fillW . fillH . padAll 8 . gap 6) $ do
          vT <- useVar (0 :: Int)
          let ts = [tab (0 :: Int) "Tab A" (), tab 1 "Tab B" ()]
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabUnderline}) vT ts
          tabBarConfigured (defaultTabsConfig {tabsStyle = TabContained}) vT ts

  pure
    [ InteractionCard
        "01"
        "surface-button surface-input surface-popup"
        "checkbox and radio span three surfaces"
        "NanoUI.Internal.Frame.Chrome (widgetVisualStyle), NanoUI.Internal.Frame.Paint.Widgets (paintCheckboxNode)"
        "Checkboxes and radios strip the button background and read three different theme slots: the unchecked interior comes from `themeInput` (`styleBg`), the outline and label come from `themeButton` (`styleBorder` and `styleFg`), and the checked fill and mark come from `themeAccent` and `themeOnAccent`. Inside a `modal`, the outline and label switch from `themeButton` to `themePopup`."
        "Style `inputStyle` for the unchecked interior, `buttonStyle` for the border and label (plus `popupStyle` inside modals), and `accentColor` for the checked state."
        "-- buttonStyle background is stripped for checkboxes\nstyled (buttonStyle (background red)) (checkbox \"Opt\" v)"
        "styled (inputStyle (background wellBg)\n      . buttonStyle (borderColor ringCol . foreground txtCol)\n      . accentColor checkCol) (checkbox \"Opt\" v)"
        "inputStyle interior, buttonStyle border and text, accentColor check"
        i3Bad
        "inside a modal, border and label switch to popupStyle"
        i3Good
    , InteractionCard
        "02"
        "surface-button surface-input"
        "select and comboBox use different surfaces"
        "NanoUI.Internal.Frame.Chrome (widgetVisualStyle)"
        "Even though they look alike when closed, `select` paints from `themeButton` while `comboBox` is built on `textInput` and paints from `themeInput`. Applying `inputStyle` to a form styles `textInput`, `numericInput`, and `comboBox`, leaving `select` unstyled."
        "Apply your field style to both `inputStyle` and `buttonStyle`, or wrap `select` in `styled (buttonStyle ...)`."
        "-- select stays on themeButton under inputStyle\nstyled (inputStyle myFieldStyle) $ do\n  comboBox opts vCombo\n  select opts vIdx"
        "styled (inputStyle myFieldStyle . buttonStyle myFieldStyle) $ do\n  comboBox opts vCombo\n  select opts vIdx"
        "inputStyle only: comboBox changes, select stays default"
        i4Bad
        "inputStyle and buttonStyle matched"
        i4Good
    , InteractionCard
        "03"
        "surface-input surface-panel"
        "scrollWith changes surface depending on its size"
        "NanoUI.Internal.Frame.Paint (paintScrollSurface)"
        "When either axis has a fixed or fit size, `scrollWith` draws a square-cornered, bordered `themeInput` well. When both axes fill (`scrollWith grow`), it acts as a full-page scroller, dropping the border and filling with `pageBackground` (`themeWindow` by default)."
        "Give embedded scroll boxes a bounded height (`fixedH`, `maxH`), or wrap a `grow` scroller inside a styled `panelWith`."
        "-- both axes fill: no well, no border\nscrollWith (grow . padAll 6) body"
        "-- bounded height keeps the themeInput well\nscrollWith (fillW . fixedH 80 . padAll 6) body"
        "grow: no well or border, fills with page background"
        i5Good
        "fixedH 80: bordered themeInput well with square corners"
        i5Bad
    , InteractionCard
        "04"
        "surface-panel"
        "callout tints nested panels"
        "NanoUI.Internal.Widgets.Layout (calloutWith)"
        "`calloutWith` wraps its children in `styled (panelStyle ...)`, so any `panel` or `card` inside the callout inherits its tinted background and border. `modal` and `popup` read `themePopup`, so overlays keep the normal `themePanel`."
        "Save `basePanel <- themePanel <$> uiTheme` before the callout and restore it around inner panels with `styled (\\t -> t {themePanel = basePanel})`."
        "callout Warning $ do\n  label \"Warning\"\n  card (label \"Inner card turns yellow\")"
        "basePanel <- themePanel <$> uiTheme\ncallout Warning $ do\n  label \"Warning\"\n  styled (\\t -> t {themePanel = basePanel}) (card (label \"Clean card\"))"
        "nested card inherits the callout tint"
        i6Bad
        "themePanel restored inside the callout"
        i6Good
    , InteractionCard
        "05"
        "surface-button"
        "toggleButton ignores buttonStyle when active"
        "NanoUI.Internal.Frame.Chrome (widgetVisualStyle)"
        "While off, `toggleButton` reads `themeButton`, so `buttonStyle` applies normally. While on, it replaces `styleBg`, `styleHoverBg`, and `styleBorder` with its tone color (`toneColor`) and computes contrasting text via `readableOn`, overriding `buttonStyle`."
        "Change the tone color (`accentColor` for `Accent`, or the matching tone) to customize the active state."
        "-- buttonStyle only styles the off state\nstyled (buttonStyle (fillColor blue)) (toggleButton Warning \"On\" vTrue)"
        "-- pair buttonStyle (off) with accentColor (on)\nstyled (buttonStyle (fillColor blue) . accentColor rose)\n  (toggleButton Accent \"On\" vTrue)"
        "active state ignores buttonStyle"
        i7Bad
        "buttonStyle for off, accentColor for on"
        i7Good
    , InteractionCard
        "06"
        "surface-panel"
        "borderWidth does not reserve layout padding"
        "NanoUI.Internal.Frame.Chrome (strokeStyledSides), NanoUI.Internal.Layout.Solve"
        "Theme scopes never alter layout geometry. `borderWidth 8` draws an 8px inset stroke without adding padding, so inside a `tight` container it paints right over child text. When `borderSides` draws fewer than all four sides, corners are drawn flat and ignore `cornerRadius`."
        "Add matching padding (`padAll`, `padLeft`, or `padLRTB`) whenever you draw thick borders."
        "styled (panelStyle (borderWidth 8)) $\n  panelWith tight (label \"Overlapped\")"
        "styled (panelStyle (borderLeft 6 rose)) $\n  panelWith (padLRTB 12 8 8 8) (label \"Padded\")"
        "borderWidth 8 on tight overlaps text"
        i9Bad
        "borderLeft 6 with padLRTB 12 8 8 8 clears text"
        i9Good
    , InteractionCard
        "07"
        "surface-button"
        "modifier order in (.) vs. nested styled"
        "NanoUI.Internal.Style, function composition"
        "Modifiers are plain Haskell functions. In `(f . g) x`, `g` runs first and `f` runs second, so the leftmost modifier wins: `primary . destructive` resolves to `primary`, and `fixedW 110 . fixedW 220` resolves to 110. Nested scopes run outside in, so `styled outer (styled inner view)` lets the inner scope win."
        "Put the winning override on the left in a `(.)` chain, and innermost when nesting `styled` calls."
        "styled (primary . destructive) (button \"Which wins?\") -- primary\nbuttonWith (fixedW 110 . fixedW 220) \"Width?\"        -- 110px"
        "styled primary $ styled destructive $ button \"Inner destructive wins\""
        "(.) chain: leftmost modifier wins"
        i10Bad
        "nested styled: innermost scope wins"
        i10Good
    , InteractionCard
        "08"
        "surface-panel surface-button"
        "tabs read themePanel, not themeButton"
        "NanoUI.Internal.Frame.Chrome (tabHeaderVisualStyle)"
        "Even though tab headers behave like buttons, they take their colors from `themePanel` and `themeAccent`. `buttonStyle` has no effect on `TabUnderline`, `TabPill`, or `TabContained`; only `TabSegmented` reads `styleBorder` from `themeButton` for its active border."
        "Style tabs through `panelStyle` and `accentColor`."
        "-- buttonStyle does not affect TabUnderline or TabContained\nstyled (buttonStyle myStyle) (tabBar active tabsList)"
        "-- panelStyle and accentColor style the tab strip\nstyled (panelStyle myPanel . accentColor myAccent) (tabBar active tabsList)"
        "buttonStyle leaves TabUnderline and TabContained unchanged"
        i11Bad
        "panelStyle and accentColor style the headers"
        i11Good
    ]

-- ---------------------------------------------------------------------------
-- Interactive States & Styles Playground (6 States x 32 Style Masks = 192 Frames)
-- ---------------------------------------------------------------------------

applyStateStyleMask :: Int -> NanoUI () -> NanoUI ()
applyStateStyleMask smask view0 =
  let hasThemeMod = (smask .&. 0x0F) /= 0
      themeMod =
        (if testBit smask 3 then probeAccent else id)
          . (if testBit smask 2 then probePopup else id)
          . (if testBit smask 1 then probeInput else id)
          . (if testBit smask 0 then probeButton else id)
      applyStyled = if hasThemeMod then styled themeMod else id
      applyDis = if testBit smask 4 then disabledWhen True else id
   in applyStyled (applyDis view0)

generateStateStrip :: IO (Text, Value)
generateStateStrip = do
  let w = 440
      h = 122
      buildView :: Bool -> NanoUI ()
      buildView focusBtn =
        panelWith (fillW . fillH . padAll 10 . gap 8) $ do
          rowWith (gap 10 . alignMid . fillW) $ do
            rBtn <- buttonWith' (fixedW 195) "Action Button"
            if focusBtn then requestFocus (respId rBtn) else pure ()
            vInp <- useVar ("Editable Input" :: Text)
            textInputConfigured (defaultTextInputConfig {ticLayout = fillW defaultLayout}) vInp
          rowWith (gap 10 . alignMid . fillW) $ do
            vSel <- useVar (0 :: Int)
            selectWith (fixedW 195) ["Select (themeButton)" :: Text, "Popup Row 2", "Popup Row 3"] vSel
            label "Click a state or style pill"

      renderOneState :: Int -> Int -> IO Text
      renderOneState stIdx smask = do
        let ui = applyStateStyleMask smask (buildView (stIdx == 3))
            tag = "wb-state"
        case stIdx of
          0 -> renderSvg tag w h ui
          1 -> renderSvgHover tag w h (V2 95 22) ui
          2 -> renderSvgPress tag w h (V2 95 22) ui
          3 -> renderSvg tag w h ui
          4 -> renderSvgClick tag w h (V2 310 22) ui
          _ -> renderSvgClick tag w h (V2 95 54) ui

  svgs <-
    sequence
      [ renderOneState stIdx smask
      | stIdx <- [0 .. 5]
      , smask <- [0 .. 31]
      ]
  let baseSvg = case svgs of
        (s0 : _) -> s0
        [] -> ""
  pure (baseSvg, packSvgBank svgs)

-- ---------------------------------------------------------------------------
-- Hero Interactive All-Surface Workbench (128 Combinations)
-- ---------------------------------------------------------------------------

generateRainbowBanner :: IO (Text, Value)
generateRainbowBanner = do
  let compositeView :: NanoUI ()
      compositeView =
        panelWith (fillW . fillH . padAll 10 . gap 8) $ do
          rowWith (gap 8 . alignMid . fillW) $ do
            heading "Surface Anatomy"
            void (button "Button")
            vOn <- useVar True
            toggleButton Accent "Active" vOn
            vChk <- useVar False
            checkbox "Check" vChk
            vChk2 <- useVar True
            checkbox "On" vChk2
          rowWith (gap 8 . alignMid . fillW) $ do
            vTxt <- useVar ("textInput (themeInput)" :: Text)
            textInputConfigured (defaultTextInputConfig {ticLayout = fixedW 170 defaultLayout}) vTxt
            vSel <- useVar (0 :: Int)
            selectWith (fixedW 130) ["select (btn)" :: Text, "Item 2"] vSel
            vSl <- useVar (60 :: Float)
            sliderWith fillW 0 100 vSl
  renderStyleBank "hero" 520 96 False compositeView

instance ToJSON ToggleNote where
  toJSON tn =
    object
      [ "bit" .= tnBit tn
      , "key" .= tnKey tn
      , "label" .= tnLabel tn
      , "short" .= tnShort tn
      , "impact" .= impactCode (tnImpact tn)
      , "badge" .= impactBadgeText (tnImpact tn)
      , "detail" .= tnDetail tn
      ]

instance ToJSON InteractionCard where
  toJSON c =
    object
      [ "id" .= icNumber c
      , "category" .= icCategory c
      , "title" .= icTitle c
      , "where" .= icWhere c
      , "why" .= icWhy c
      , "fix" .= icFix c
      , "codeBad" .= icCodeBad c
      , "codeGood" .= icCodeGood c
      , "labelA" .= icLabelA c
      , "svgA" .= icSvgA c
      , "labelB" .= icLabelB c
      , "svgB" .= icSvgB c
      ]
