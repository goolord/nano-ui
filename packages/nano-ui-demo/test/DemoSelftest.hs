-- | Headless UI test for the SDL demo. It draws 'demoUi' on a hidden
-- window, drives real mouse and keyboard gestures, and checks the text spans
-- each frame produced. With @continuous@ set it exercises direct-to-window
-- presentation.
module DemoSelftest
    ( selftest
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Primitive.PrimArray (sizeofPrimArray)
import NanoUI
import NanoUI.Backend
import NanoUI.Backend.Sdl
import NanoUI.Internal.Context (ctxResolveFont, ctxResolveMeasure)
import NanoUI.Internal.Debug (CoreDebugSnapshot (dbgPresents))
import NanoUI.Testing
  ( collectOverlayTextSpans
  , collectTextSpans
  )
import NanoUI.Testing.Harness
  ( findExact
  , findHeader
  , findRightmost
  , expectText
  , hasText
  , requireSpan
  , spanLabel
  )
import NanoUI.Testing.Harness qualified as Harness
import Text.Printf (printf)
import qualified Data.Text as T
import DemoApp (withHiddenWindow)
import SdlDemo (demoUi)

-- | Draw 'demoUi' on a hidden SDL window and drive it through the main
-- widget interactions, failing loudly on any regression.
selftest :: Bool -> IO ()
selftest continuous = do
  withHiddenWindow 1280 800 (V2 (-10) (-10)) (\o -> o {sdlAppContinuous = continuous}) $ \ctx env _ -> do
    -- Shaped-run font measurement must match SDL3_ttf string measurement.
    (fmNorm16, _) <- ctxResolveFont ctx 16.0 WeightNormal FontStyleNormal FontRegular
    (fmItal16, _) <- ctxResolveFont ctx 16.0 WeightNormal FontStyleItalic FontRegular
    (wNorm, _) <- ctxResolveMeasure ctx 16.0 WeightNormal FontStyleNormal FontRegular "Slanted synthetic italic font style."
    (wItal, _) <- ctxResolveMeasure ctx 16.0 WeightNormal FontStyleItalic FontRegular "Slanted synthetic italic font style."
    runNorm <- lineWidthIO fmNorm16 "Slanted synthetic italic font style."
    runItal <- lineWidthIO fmItal16 "Slanted synthetic italic font style."
    when (abs (runNorm - wNorm) > 0.01) $
      fail $ printf "selftest: shaped width mismatch for normal sentence: measure=%.2f, shaped=%.2f" wNorm runNorm
    when (abs (runItal - wItal) > 0.01) $
      fail $ printf "selftest: shaped width mismatch for italic sentence: measure=%.2f, shaped=%.2f" wItal runItal
    putStrLn $ printf "MEASURE string: norm=%.1f, ital=%.1f" wNorm wItal
    -- The shaped path (fmShape / pushText) must match SDL3_ttf measurement;
    -- include GPOS kerning and ligatures for pairs like To, AV, and fi.
    (fmNorm20, _) <- ctxResolveFont ctx 20.0 WeightNormal FontStyleNormal FontRegular
    (fmItal20, _) <- ctxResolveFont ctx 20.0 WeightNormal FontStyleItalic FontRegular
    let checkRun :: String -> FontMetrics -> FontStyle -> String -> IO ()
        checkRun tag fm st pair = do
          (wab, _) <- ctxResolveMeasure ctx 20.0 WeightNormal st FontRegular (T.pack pair)
          runW <- lineWidthIO fm (T.pack pair)
          when (abs (runW - wab) > 0.01) $
            fail $ printf "selftest: %s shaped width mismatch for '%s': measure=%.2f, shaped=%.2f" tag pair wab runW
    checkRun "norm" fmNorm20 FontStyleNormal "To"
    checkRun "ital" fmItal20 FontStyleItalic "To"
    checkRun "norm" fmNorm20 FontStyleNormal "AV"
    checkRun "ital" fmItal20 FontStyleItalic "AV"
    checkRun "norm" fmNorm20 FontStyleNormal "fi"
    checkRun "ital" fmItal20 FontStyleItalic "fi"
    -- Shaped text draws glyph by glyph, so a line wider than the 2048px glyph
    -- atlas still gets quads and a width, and a short line gets one quad a
    -- glyph.
    (fmBig, _) <- ctxResolveFont ctx 64.0 WeightNormal FontStyleNormal FontRegular
    let bigTxt = T.replicate 400 "f"
    bigGlyphs <- drawShaped fmBig bigTxt
    bigWidth <- lineWidthIO fmBig bigTxt
    shortGlyphs <- drawShaped fmBig "fits"
    when (maybe True (\(ShapedGlyphs q) -> sizeofPrimArray q < 400 * 8) bigGlyphs) $
      fail "selftest: a line wider than the atlas lost its glyphs"
    when (bigWidth <= 0) $
      fail "selftest: a line wider than the atlas lost its width"
    when (maybe True (\(ShapedGlyphs q) -> sizeofPrimArray q /= 4 * 8) shortGlyphs) $
      fail "selftest: a short shaped line did not draw one quad a glyph"
    let idle =
          emptyInput
            { inputWindowSize = Size 1280 800
            , inputMousePos = V2 640 400
            }
    (ctx', base) <- syncDisplay ctx env idle
    let drawWith frameUi inp = void (sdlDrawFrame ctx' frameUi env inp False)
        drawOnce = drawWith demoUi
        clickPos = Harness.clickPos drawOnce base
        dragPos = Harness.dragPos drawOnce base
        clickTab = Harness.clickTab collectTextSpans drawOnce ctx' base
    void (sdlDrawFrame ctx' demoUi env base True)
    spans0 <- collectTextSpans ctx'
    expectText "selftest: Controls body missing" "Feature" spans0
    -- A field value wider than the glyph atlas draws glyph by glyph, and the
    -- text drawn before and after it in the frame survives.
    nameLbl <- requireSpan "selftest: Name label" (findRightmost "Name" spans0)
    clickPos (V2 (v2X nameLbl + 80) (v2Y nameLbl))
    drawOnce (base {inputChars = T.replicate 400 "f"})
    drawOnce base
    spansLong <- collectTextSpans ctx'
    expectText "selftest: long field text lost the tab content" "Feature" spansLong
    unless (length spansLong >= length spans0 - 1) $
      fail "selftest: long field text collapsed the span set"
    clickTab "Table"
    spansTable <- collectTextSpans ctx'
    expectText "selftest: table body missing after Table tab" "David" spansTable
    hdr <- requireSpan "selftest: Name header" (findHeader "Name" spansTable)
    clickPos hdr
    spansSorted <- collectTextSpans ctx'
    expectText "selftest: header click did not toggle sort" "descending" spansSorted
    dept <- requireSpan "selftest: Dept header" (findHeader "Dept" spansSorted)
    dragPos dept (V2 (v2X dept + 180) (v2Y dept))
    spansDrag <- collectTextSpans ctx'
    expectText "selftest: table missing after header drag" "Sonia" spansDrag
    clickTab "List"
    spansTree <- collectTextSpans ctx'
    expectText "selftest: tree missing after List tab" "src" spansTree
    readme <- requireSpan "selftest: README.md" (findExact "README.md" spansTree)
    clickPos readme
    spansSel <- collectTextSpans ctx'
    expectText "selftest: tree click did not select README.md" "7" spansSel
    clickTab "Typography"
    spansType <- collectTextSpans ctx'
    expectText "selftest: typography missing after Typography tab" "Live Playground" spansType
    drawOnce (base {inputScroll = V2 0 (-350)})
    drawOnce base
    sizeSpan <- requireSpan "selftest: Size slider" (findRightmost "Size" spansType)
    for_ [20, 60, 100, 140, 180, 50, 120, -60, -100, 0 :: Float] $ \dx ->
      dragPos sizeSpan (V2 (v2X sizeSpan + dx) (v2Y sizeSpan))
    spansTypeAfter <- collectTextSpans ctx'
    expectText "selftest: typography missing after size changes" "Live Playground" spansTypeAfter
    clickTab "Panes"
    spansPane0 <- collectTextSpans ctx'
    expectText "selftest: pane grid missing after Panes tab" "Pane 1" spansPane0
    -- Pane titles, left to right.
    let titles ss =
          sortOn
            (rectX . fst)
            [ (r, l)
            | (r, txt, _, _, _) <- ss
            , let l = spanLabel txt
            , "Pane " `T.isPrefixOf` l
            , let rest = T.drop 5 l
            , not (T.null rest)
            , T.all isDigit (T.takeWhile (/= ' ') rest)
            ]
        paneCount = length . titles
        titleCenter = Harness.spanCenter . fst
    unless (paneCount spansPane0 == 1) $ fail "selftest: expected exactly one pane initially"
    plus <- requireSpan "selftest: split button" (findExact "+" spansPane0)
    clickPos plus
    spansPane1 <- collectTextSpans ctx'
    unless (paneCount spansPane1 == 2) $ fail "selftest: split did not create a second pane"
    maxBtn <- requireSpan "selftest: maximize button" (findExact "M" spansPane1)
    clickPos maxBtn
    spansPaneMax <- collectTextSpans ctx'
    expectText "selftest: maximize did not fill the grid" "maximized" spansPaneMax
    restoreBtn <- requireSpan "selftest: restore button" (findExact "R" spansPaneMax)
    clickPos restoreBtn
    spansPane2 <- collectTextSpans ctx'
    unless (paneCount spansPane2 == 2) $ fail "selftest: restore lost a pane"
    closeBtn <- requireSpan "selftest: close button" (findExact "x" spansPane2)
    clickPos closeBtn
    spansPane3 <- collectTextSpans ctx'
    unless (paneCount spansPane3 == 1) $ fail "selftest: close did not remove a pane"
    plus2 <- requireSpan "selftest: split button after close" (findExact "+" spansPane3)
    clickPos plus2
    spansPane4 <- collectTextSpans ctx'
    [leftTitle4, rightTitle4] <- pure (titles spansPane4)
    -- Edge drop: grab the right pane and drop it on the left pane's left edge.
    -- The dragged pane must become the new leftmost pane.
    dragPos (titleCenter rightTitle4) (V2 (rectX (fst leftTitle4) - 10) (rectY (fst leftTitle4) + 150))
    spansEdge <- collectTextSpans ctx'
    [leftTitle5, rightTitle5] <- pure (titles spansEdge)
    unless (snd leftTitle5 /= snd leftTitle4) $ fail "selftest: edge drop did not land on the left side"
    -- Center drop: grab the left pane and drop it on the center of the right
    -- pane. The two panes swap, so the leftmost title must change again.
    let halfGap = (rectX (fst rightTitle5) - rectX (fst leftTitle5)) / 2
    dragPos (titleCenter leftTitle5) (V2 (rectX (fst rightTitle5) + halfGap) (rectY (fst rightTitle5) + 160))
    spansPane5 <- collectTextSpans ctx'
    [afterSwap, _] <- pure (titles spansPane5)
    unless (snd afterSwap /= snd leftTitle5) $ fail "selftest: pane drag did not swap the panes"
    -- Top-level drop: build three side-by-side panes, then drag the middle one
    -- to the grid's outer left edge. The grid must restructure at the top level
    -- into one pane on the left and the other two side-by-side on the right
    -- half, rather than a flat third column.
    plus3 <- requireSpan "selftest: split button for three panes" (findExact "+" spansPane5)
    clickPos plus3
    spansPane3c <- collectTextSpans ctx'
    [leftT3, midT3, _] <- pure (titles spansPane3c)
    dragPos
      (V2 (v2X (titleCenter midT3) + 70) (v2Y (titleCenter midT3) + 100))
      (V2 (rectX (fst leftT3) - 10) (rectY (fst leftT3) + 150))
    spansTop <- collectTextSpans ctx'
    [leftAfterTop, rightLeftTop, rightRightTop] <- pure (titles spansTop)
    -- A top-level wrap gives the left pane the whole left half, so the gap to
    -- the first right pane (~half the width) exceeds the gap between the two
    -- right panes (~a quarter); a flat third column keeps them equal.
    let gapLeft = rectX (fst rightLeftTop) - rectX (fst leftAfterTop)
        gapRight = rectX (fst rightRightTop) - rectX (fst rightLeftTop)
    unless (snd leftAfterTop == snd midT3) $ fail "selftest: top-level drop did not move the middle pane left"
    unless (gapLeft > gapRight + 10) $ fail "selftest: top-level drop did not collapse the remaining panes onto one side"
    -- Pane headers are optional: turn them off and the titles vanish but the
    -- panes (and their content) remain.
    hdrBtn <- requireSpan "selftest: headers checkbox" (findExact "Pane headers" spansTop)
    clickPos hdrBtn
    spansPane6 <- collectTextSpans ctx'
    unless (paneCount spansPane6 == 0) $ fail "selftest: disabling pane headers did not hide them"
    expectText "selftest: headerless panes lost their content" "Contents of" spansPane6
    clickTab "Controls"
    spansCtl <- collectTextSpans ctx'
    expectText "selftest: Controls missing after tab back" "Feature" spansCtl
    feat0 <- requireSpan "selftest: Feature checkbox" (findRightmost "Feature" spansCtl)
    clickPos feat0
    spansOn <- collectTextSpans ctx'
    expectText "selftest: checkbox did not turn Feature on" "on" spansOn
    clickPos feat0
    spansOff <- collectTextSpans ctx'
    expectText "selftest: checkbox did not turn Feature off" "off" spansOff
    clickPos feat0
    spansOn2 <- collectTextSpans ctx'
    expectText "selftest: checkbox did not turn Feature on again" "on" spansOn2
    -- Theme is a radio fieldset: all options stay visible in the plain spans;
    -- click "Tomorrow Light" directly (the state card shows the old value).
    lightOpt <- requireSpan "selftest: Tomorrow Light option" (findExact "Tomorrow Light" spansOn2)
    clickPos lightOpt
    spansTheme <- collectTextSpans ctx'
    expectText "selftest: radio did not pick Tomorrow Light" "Tomorrow Light" spansTheme
    th <- getTheme ctx'
    unless (th == tomorrowMinLightTheme) $ fail "selftest: context theme was not updated to Tomorrow Light"
    vol <- requireSpan "selftest: Volume slider" (findRightmost "Volume" spansTheme)
    clickPos (V2 (v2X vol + 80) (v2Y vol))
    about <- requireSpan "selftest: About button" (findExact "About" spansTheme)
    clickPos about
    spansModal <- collectOverlayTextSpans ctx' base
    expectText "selftest: About modal missing" "Immediate-mode" spansModal
    -- Font metrics can make the body overflow the modal's initial viewport.
    -- The footer must be reachable by scrolling, not necessarily visible yet.
    unless (hasText "Close" spansModal) $ do
      bodyPos <- requireSpan "selftest: About body" (findExact "nano-ui" spansModal)
      drawOnce (base {inputMousePos = bodyPos, inputScroll = V2 0 10})
      drawOnce base
    spansModalFooter <- collectOverlayTextSpans ctx' base
    expectText "selftest: About Close button unreachable" "Close" spansModalFooter
    drawOnce (Harness.keyInp KeyEscape base)
    drawOnce base
    spansClosed <- collectOverlayTextSpans ctx' base
    when (hasText "Immediate-mode" spansClosed) $ fail "selftest: Escape did not dismiss About"
    spansLatest <- collectTextSpans ctx'
    debugBtn <- requireSpan "selftest: Debug button" (findExact "Debug" spansLatest)
    clickPos debugBtn
    spansDebug <- collectOverlayTextSpans ctx' base
    expectText "selftest: Debug window missing" "Frame" spansDebug
    expectText "selftest: Debug Runtime section missing" "Runtime" spansDebug
    -- Formatted rows may be reused between samples, but must track the next
    -- backend refresh rather than freezing the first snapshot in the cache.
    sampledDraws <- newIORef 0
    let observeDebug = do
          snapshot <- askSdlDebug
          liftIO $ writeIORef sampledDraws (dbgPresents (dbgCore snapshot))
          demoUi
    drawWith observeDebug base
    previous <- readIORef sampledDraws
    threadDelay 300000
    drawWith observeDebug base
    current <- readIORef sampledDraws
    refreshed <- collectOverlayTextSpans ctx' base
    unless (current > previous && isJust (findExact (T.pack (show current)) refreshed)) $
      fail "selftest: Debug draws counter did not refresh"
  putStrLn ("demo selftest" <> (if continuous then " (continuous)" else "") <> ": ok")
