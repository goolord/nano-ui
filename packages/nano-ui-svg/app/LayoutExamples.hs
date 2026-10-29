-- | Layout examples for the book's Layout chapter. Each anchored block is
-- included in @book/src/layout/index.md@, and the glossary generator renders
-- the same definitions to @book/src/images/layout/@, framed by its own
-- padding, so the code on the page is the code that drew the picture.
module LayoutExamples (layoutExamples) where

import Control.Monad (void, when)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI

-- Every example is a whole view; the signatures are kept here, off the page.
rowExample, columnExample, gridExample, panelExample, cardExample,
  calloutExample, layersExample, centerExample, responsiveExample,
  mouseAreaExample, separatorExample, spacerExample, fillWExample,
  fillHExample, growExample, fixedWExample, fixedHExample, fixedWHExample,
  minWExample, maxWExample, minHExample, maxHExample, percentExample,
  aspectExample, padAllExample, padXYExample, padLeftExample,
  padRightExample, padTopExample, padBottomExample, padLRTBExample,
  gapExample, tightExample, alignStartExample, alignEndExample,
  alignCenterExample, alignTopExample, alignMidExample, alignBottomExample,
  alignBaselineExample, wrapExample, lineGapExample, lineAlignExample,
  pinAtExample, pointerBlockExample, pointerPassExample :: NanoUI ()

-- ANCHOR: row
rowExample =
  rowWith (gap 8) $
    mapM_ (void . button) ["One", "Two", "Three"]
-- ANCHOR_END: row

-- ANCHOR: column
columnExample =
  columnWith (gap 8) $
    mapM_ (void . buttonWith fillW) ["One", "Two", "Three"]
-- ANCHOR_END: column

-- ANCHOR: grid
gridExample =
  gridWith 3 (gap 8 . fillW) $
    mapM_
      (\i -> void (buttonWith fillW ("Cell " <> T.pack (show (i :: Int)))))
      [1 .. 6]
-- ANCHOR_END: grid

-- ANCHOR: panel
panelExample =
  panelWith (fixedW 300 . padAll 12 . gap 6) $ do
    heading "Deployments"
    muted "3 services running"
-- ANCHOR_END: panel

-- ANCHOR: card
cardExample =
  card $ do
    heading "Build #482"
    muted "Passed in 1m 42s"
-- ANCHOR_END: card

-- ANCHOR: callout
calloutExample = do
  warnCol <- themeWarning <$> uiTheme
  callout warnCol $ do
    heading "Certificate expiring"
    muted "TLS certificate expires in 3 days."
-- ANCHOR_END: callout

-- ANCHOR: layers
layersExample =
  layers $ do
    let badgeRed = colorRGB 204 64 64
    void (buttonWith (fixedWH 170 40) "Inbox")
    styled (panelStyle (background badgeRed . borderColor badgeRed)) $
      panelWith (alignEnd . alignTop . padXY 6 1) $
        labelWith (fontColor colorWhite) "3"
-- ANCHOR_END: layers

-- ANCHOR: center
centerExample =
  panelWith (fillW . fillH) $
    center (label "Centred on both axes")
-- ANCHOR_END: center

-- ANCHOR: responsive
responsiveExample =
  responsiveRowCol 640 (tight . gap 8 . fillW) $ do
    panelWith (fillW . padAll 12) (label "Sidebar")
    panelWith (fillW . padAll 12) (label "Content")
-- ANCHOR_END: responsive

-- ANCHOR: mousearea
mouseAreaExample = do
  (_, area) <-
    panelWith fillW $
      mouseArea (fillW . padAll 10 . gap 4) $ do
        label "build.log"
        muted "Click anywhere in this card"
  when (respClicked area) (muted "opening build.log")
-- ANCHOR_END: mousearea

-- ANCHOR: separator
separatorExample =
  columnWith (fillW . gap 8) $ do
    label "Above the rule"
    separator
    label "Below the rule"
-- ANCHOR_END: separator

-- ANCHOR: spacer
spacerExample =
  rowWith (fillW . alignMid) $ do
    label "Left"
    spacer (Grow 1) Fit
    label "Right"
-- ANCHOR_END: spacer

-- ANCHOR: fillw
fillWExample =
  rowWith (fillW . gap 8) $ do
    void (buttonWith (fixedW 110) "fixedW 110")
    void (buttonWith fillW "fillW")
-- ANCHOR_END: fillw

-- ANCHOR: fillh
fillHExample =
  rowWith (fillW . gap 8 . fixedH 96) $ do
    panelWith (fixedW 120 . fillH) (label "fillH")
    panelWith (fixedW 120) (label "natural")
-- ANCHOR_END: fillh

-- ANCHOR: grow
growExample =
  rowWith (grow . gap 8) $ do
    panelWith (fixedW 120) (label "fixedW 120")
    panelWith grow (label "grow")
-- ANCHOR_END: grow

-- ANCHOR: fixedw
fixedWExample =
  rowWith (gap 8) $ do
    void (buttonWith (fixedW 60) "60")
    void (buttonWith (fixedW 110) "110")
    void (buttonWith (fixedW 170) "170")
-- ANCHOR_END: fixedw

-- ANCHOR: fixedh
fixedHExample =
  rowWith (gap 8 . fillW) $ do
    void (buttonWith (fixedH 30) "fixedH 30")
    void (buttonWith (fixedH 56) "fixedH 56")
    void (buttonWith (fixedH 82) "fixedH 82")
-- ANCHOR_END: fixedh

-- ANCHOR: fixedwh
fixedWHExample =
  rowWith (gap 8) $ do
    void (buttonWith (fixedWH 120 36) "120 x 36")
    void (buttonWith (fixedWH 200 72) "200 x 72")
-- ANCHOR_END: fixedwh

-- ANCHOR: minw
minWExample =
  rowWith (fillW . gap 8) $ do
    void (buttonWith (minW 200) "minW 200")
    void (button "natural")
-- ANCHOR_END: minw

-- ANCHOR: maxw
maxWExample =
  rowWith (fillW . gap 8) $ do
    void (buttonWith (fillW . maxW 160) "maxW 160")
    void (buttonWith fillW "fillW")
-- ANCHOR_END: maxw

-- ANCHOR: minh
minHExample =
  rowWith (fillW . gap 8) $ do
    panelWith (minH 80 . fillW) (label "minH 80")
    panelWith fillW (label "natural")
-- ANCHOR_END: minh

-- ANCHOR: maxh
maxHExample =
  rowWith (fillW . gap 8 . fixedH 100) $ do
    panelWith (maxH 50 . fillW) (label "maxH 50")
    panelWith (fillW . fillH) (label "fillH")
-- ANCHOR_END: maxh

-- ANCHOR: percent
percentExample =
  rowWith (fillW . gap 8) $ do
    panelWith (percent 30) (label "percent 30")
    panelWith (percent 60) (label "percent 60")
-- ANCHOR_END: percent

-- ANCHOR: aspect
aspectExample =
  panelWith (fillW . aspect (16 / 9)) $
    center (label "16 : 9")
-- ANCHOR_END: aspect

-- ANCHOR: padall
padAllExample =
  panelWith (fixedWH 260 120 . padAll 20) $
    panelWith (fillW . fillH) (label "padAll 20")
-- ANCHOR_END: padall

-- ANCHOR: padxy
padXYExample =
  panelWith (fixedWH 280 100 . padXY 32 10) $
    panelWith (fillW . fillH) (label "padXY 32 10")
-- ANCHOR_END: padxy

-- ANCHOR: padleft
padLeftExample =
  panelWith (fixedWH 260 100 . padLeft 48) $
    panelWith (fillW . fillH) (label "padLeft 48")
-- ANCHOR_END: padleft

-- ANCHOR: padright
padRightExample =
  panelWith (fixedWH 260 100 . padRight 48) $
    panelWith (fillW . fillH) (label "padRight 48")
-- ANCHOR_END: padright

-- ANCHOR: padtop
padTopExample =
  panelWith (fixedWH 260 120 . padTop 40) $
    panelWith (fillW . fillH) (label "padTop 40")
-- ANCHOR_END: padtop

-- ANCHOR: padbottom
padBottomExample =
  panelWith (fixedWH 260 120 . padBottom 40) $
    panelWith (fillW . fillH) (label "padBottom 40")
-- ANCHOR_END: padbottom

-- ANCHOR: padlrtb
padLRTBExample =
  panelWith (fixedWH 280 120 . padLRTB 40 8 30 12) $
    panelWith (fillW . fillH) (label "padLRTB 40 8 30 12")
-- ANCHOR_END: padlrtb

-- ANCHOR: gap
gapExample =
  columnWith (fillW . gap 12) $ do
    rowWith (gap 4) $ mapM_ (void . button) ["gap 4", "B", "C"]
    rowWith (gap 24) $ mapM_ (void . button) ["gap 24", "B", "C"]
-- ANCHOR_END: gap

-- ANCHOR: tight
tightExample =
  rowWith (gap 16 . fillW) $ do
    panelWith (padAll 14 . gap 6 . fillW) $ do
      muted "padAll 14"
      label "default padding"
    panelWith (tight . padAll 14 . gap 6 . fillW) $ do
      muted "tight"
      label "no padding"
-- ANCHOR_END: tight

-- ANCHOR: alignstart
alignStartExample =
  panelWith (fixedWH 300 110 . padAll 10) $
    void (buttonWith (alignStart . fixedW 140) "alignStart")
-- ANCHOR_END: alignstart

-- ANCHOR: alignend
alignEndExample =
  panelWith (fixedWH 300 110 . padAll 10) $
    void (buttonWith (alignEnd . fixedW 140) "alignEnd")
-- ANCHOR_END: alignend

-- ANCHOR: aligncenter
alignCenterExample =
  panelWith (fixedWH 300 110 . padAll 10) $
    void (buttonWith (alignCenter . fixedW 140) "alignCenter")
-- ANCHOR_END: aligncenter

-- ANCHOR: aligntop
alignTopExample =
  panelWith (fixedWH 300 110 . padAll 10) $
    rowWith (fillW . fillH . gap 8) $
      void (buttonWith (alignTop . fixedW 140) "alignTop")
-- ANCHOR_END: aligntop

-- ANCHOR: alignmid
alignMidExample =
  panelWith (fixedWH 300 110 . padAll 10) $
    rowWith (fillW . fillH . gap 8) $
      void (buttonWith (alignMid . fixedW 140) "alignMid")
-- ANCHOR_END: alignmid

-- ANCHOR: alignbottom
alignBottomExample =
  panelWith (fixedWH 300 110 . padAll 10) $
    rowWith (fillW . fillH . gap 8) $
      void (buttonWith (alignBottom . fixedW 140) "alignBottom")
-- ANCHOR_END: alignbottom

-- ANCHOR: alignbaseline
alignBaselineExample =
  rowWith (gap 12 . fixedW 340) $ do
    labelWith (fontSize 24 . alignBaseline) "Title"
    labelWith alignBaseline "baseline text"
-- ANCHOR_END: alignbaseline

-- ANCHOR: wrap
wrapExample =
  rowWith (wrap . gap 6 . fixedW 280) $
    mapM_ (void . button) ["haskell", "immediate", "mode", "layout", "sdl", "widgets"]
-- ANCHOR_END: wrap

-- ANCHOR: linegap
lineGapExample =
  columnWith (fillW . gap 16) $ do
    let labels = ["haskell", "immediate", "mode", "layout", "sdl", "widgets"]
    muted "lineGap 2"
    rowWith (wrap . lineGap 2 . gap 6 . fixedW 280) $ mapM_ (void . button) labels
    muted "lineGap 18"
    rowWith (wrap . lineGap 18 . gap 6 . fixedW 280) $ mapM_ (void . button) labels
-- ANCHOR_END: linegap

-- ANCHOR: linealign
lineAlignExample =
  rowWith (wrap . lineAlign LinesCenter . gap 6 . fixedW 210) $
    mapM_ (void . button) ["alpha", "beta", "gamma", "delta", "epsilon"]
-- ANCHOR_END: linealign

-- ANCHOR: pinat
pinAtExample =
  panelWith (fixedWH 280 110 . padAll 12) $ do
    heading "Inbox"
    muted "The badge is pinned, not stacked."
    let badgeCol = colorRGB 204 64 64
    styled (panelStyle (background badgeCol . borderColor badgeCol)) $
      panelWith (pinAt 6 (-6) . alignEnd . alignTop . padXY 6 1) $
        labelWith (fontColor colorWhite) "12"
-- ANCHOR_END: pinat

-- ANCHOR: pointerblock
pointerBlockExample =
  layers $ do
    void (buttonWith (fixedWH 200 44) "Open logs")
    styled (panelStyle (background (colorRGBA 0 0 0 150) . borderWidth 0)) $
      panelWith (pinAt 0 0 . grow . pointer PointerBlock . padAll 6) $
        muted "scrim"
-- ANCHOR_END: pointerblock

-- ANCHOR: pointerpass
pointerPassExample =
  layers $ do
    void (buttonWith (fixedWH 200 44) "Open logs")
    labelWith (pinAt 8 (-6) . pointer PointerPass) "decorative label"
-- ANCHOR_END: pointerpass

-- | Every layout example, in the order the book presents them, with the
-- canvas size to render it at. The hover points park the pointer on the
-- control a pointer-mode example overlays.
layoutExamples :: [(Text, Float, Float, Maybe V2, NanoUI ())]
layoutExamples =
  [ ("row", 420, 74, Nothing, rowExample)
  , ("column", 420, 140, Nothing, columnExample)
  , ("grid", 420, 120, Nothing, gridExample)
  , ("panel", 420, 110, Nothing, panelExample)
  , ("card", 420, 120, Nothing, cardExample)
  , ("callout", 420, 130, Nothing, calloutExample)
  , ("layers", 420, 90, Nothing, layersExample)
  , ("center", 420, 150, Nothing, centerExample)
  , ("responsive-narrow", 420, 140, Nothing, responsiveExample)
  , ("responsive-wide", 760, 110, Nothing, responsiveExample)
  , ("mousearea", 420, 100, Nothing, mouseAreaExample)
  , ("separator", 420, 110, Nothing, separatorExample)
  , ("spacer", 420, 74, Nothing, spacerExample)
  , ("fillw", 420, 74, Nothing, fillWExample)
  , ("fillh", 420, 140, Nothing, fillHExample)
  , ("grow", 420, 150, Nothing, growExample)
  , ("fixedw", 420, 74, Nothing, fixedWExample)
  , ("fixedh", 420, 130, Nothing, fixedHExample)
  , ("fixedwh", 420, 120, Nothing, fixedWHExample)
  , ("minw", 420, 74, Nothing, minWExample)
  , ("maxw", 420, 74, Nothing, maxWExample)
  , ("minh", 420, 130, Nothing, minHExample)
  , ("maxh", 420, 130, Nothing, maxHExample)
  , ("percent", 420, 100, Nothing, percentExample)
  , ("aspect", 460, 280, Nothing, aspectExample)
  , ("padall", 420, 150, Nothing, padAllExample)
  , ("padxy", 420, 130, Nothing, padXYExample)
  , ("padleft", 420, 130, Nothing, padLeftExample)
  , ("padright", 420, 130, Nothing, padRightExample)
  , ("padtop", 420, 150, Nothing, padTopExample)
  , ("padbottom", 420, 150, Nothing, padBottomExample)
  , ("padlrtb", 420, 150, Nothing, padLRTBExample)
  , ("gap", 420, 110, Nothing, gapExample)
  , ("tight", 420, 140, Nothing, tightExample)
  , ("alignstart", 420, 150, Nothing, alignStartExample)
  , ("aligncenter", 420, 150, Nothing, alignCenterExample)
  , ("alignend", 420, 150, Nothing, alignEndExample)
  , ("aligntop", 420, 150, Nothing, alignTopExample)
  , ("alignmid", 420, 150, Nothing, alignMidExample)
  , ("alignbottom", 420, 150, Nothing, alignBottomExample)
  , ("alignbaseline", 420, 90, Nothing, alignBaselineExample)
  , ("wrap", 420, 170, Nothing, wrapExample)
  , ("linegap", 420, 230, Nothing, lineGapExample)
  , ("linealign", 420, 170, Nothing, lineAlignExample)
  , ("pinat", 420, 160, Nothing, pinAtExample)
  , ("pointer-block", 420, 100, Just (V2 114 36), pointerBlockExample)
  , ("pointer-pass", 420, 100, Just (V2 40 20), pointerPassExample)
  ]
