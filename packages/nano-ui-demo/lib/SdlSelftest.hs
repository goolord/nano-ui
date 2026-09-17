-- | Headless UI self-test for the SDL demo. Not a unit-test framework: it
-- draws 'demoUi' on a hidden window and drives real mouse/keyboard gestures,
-- asserting on the text spans each frame produced.
--
-- It is deliberately decoupled from "SdlDemo": the app passes in its own
-- images and UI so this module never needs to know what the demo looks like.
--
-- Run via @cabal run -fsdl nano-ui-sdl-demo -- --selftest@; add
-- @--continuous@ to exercise direct-to-window presentation.
module SdlSelftest
    ( selftest
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (comparing)
import Data.Primitive.PrimArray (sizeofPrimArray)
import Data.Primitive.SmallArray (SmallArray)
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Context (ctxResolveFont, ctxResolveMeasure)
import NanoUI.Debug (CoreDebugSnapshot (dbgPresents))
import NanoUI.Testing
  ( Context
  , collectOverlayTextSpans
  , collectTextSpans
  )
import NanoUI.Testing.Harness
  ( findExact
  , findHeader
  , findRightmost
  , hasText
  , requireSpan
  , spanLabel
  )
import NanoUI.Testing.Harness qualified as Harness
import DemoData (registerDemoImages)
import Text.Printf (printf)
import qualified Data.Text as T

-- | Draw the given UI on a hidden SDL window and drive it through the main
-- widget interactions, failing loudly on any regression.
selftest :: Bool -> SmallArray RgbaImage -> NanoUI () -> IO ()
selftest continuous imgs ui = do
  ctx0 <- newSdlContext
  ok <- registerDemoImages ctx0 imgs
  unless ok $ fail "selftest: registerImage failed"
  let opts =
        defaultSdlOptions
          { sdlWindowHidden = True
          , sdlWindowSize = Size 1280 800
          , sdlWindowResizable = False
          , sdlAppContinuous = continuous
          }
  withSdl opts ctx0 $ \ctx env -> do
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
    -- catches regressions where per-glyph fallback ignored GPOS kerning for
    -- pairs like To, AV, and fi.
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
    void (sdlDrawFrame ctx' ui env base True)
    spans0 <- collectTextSpans ctx'
    unless (hasText "Feature" spans0) $ fail "selftest: Controls body missing"
    -- Regression (long field text): typing a value wider than the glyph atlas
    -- must render per-glyph. The old whole-run path reset the atlas
    -- mid-frame, so every quad already recorded in the frame sampled the
    -- wiped texture (text below vanished) and the run itself drew from the
    -- atlas-origin UVs (the white patch). Exercise the full paint pipeline
    -- with an oversized field value; span collection must survive it.
    nameLbl <- requireSpan "selftest: Name label" (findRightmost "Name" spans0)
    clickPos ui ctx' env base (V2 (v2X nameLbl + 80) (v2Y nameLbl))
    drawOnce ui ctx' env (base {inputChars = T.replicate 400 "f"})
    drawOnce ui ctx' env base
    spansLong <- collectTextSpans ctx'
    unless (hasText "Feature" spansLong) $ fail "selftest: long field text lost the tab content"
    unless (length spansLong >= length spans0 - 1) $
      fail "selftest: long field text collapsed the span set"
    clickTab ui ctx' env base "Table"
    spansTable <- collectTextSpans ctx'
    unless (hasText "David" spansTable) $ fail "selftest: table body missing after Table tab"
    hdr <- requireSpan "selftest: Name header" (findHeader "Name" spansTable)
    clickPos ui ctx' env base hdr
    spansSorted <- collectTextSpans ctx'
    unless (hasText "descending" spansSorted) $ fail "selftest: header click did not toggle sort"
    dept <- requireSpan "selftest: Dept header" (findHeader "Dept" spansSorted)
    dragPos ui ctx' env base dept (V2 (v2X dept + 180) (v2Y dept))
    spansDrag <- collectTextSpans ctx'
    unless (hasText "Sonia" spansDrag) $ fail "selftest: table missing after header drag"
    clickTab ui ctx' env base "List"
    spansTree <- collectTextSpans ctx'
    unless (hasText "src" spansTree) $ fail "selftest: tree missing after List tab"
    readme <- requireSpan "selftest: README.md" (findExact "README.md" spansTree)
    clickPos ui ctx' env base readme
    spansSel <- collectTextSpans ctx'
    unless (hasText "7" spansSel) $ fail "selftest: tree click did not select README.md"
    clickTab ui ctx' env base "Typography"
    spansType <- collectTextSpans ctx'
    unless (hasText "Live Playground" spansType) $ fail "selftest: typography missing after Typography tab"
    drawOnce ui ctx' env (base {inputScroll = V2 0 (-350)})
    drawOnce ui ctx' env base
    sizeSpan <- requireSpan "selftest: Size slider" (findRightmost "Size" spansType)
    for_ [20, 60, 100, 140, 180, 50, 120, -60, -100, 0 :: Float] $ \dx ->
      dragPos ui ctx' env base sizeSpan (V2 (v2X sizeSpan + dx) (v2Y sizeSpan))
    spansTypeAfter <- collectTextSpans ctx'
    unless (hasText "Live Playground" spansTypeAfter) $ fail "selftest: typography missing after size changes"
    clickTab ui ctx' env base "Panes"
    spansPane0 <- collectTextSpans ctx'
    unless (hasText "Pane 1" spansPane0) $ fail "selftest: pane grid missing after Panes tab"
    let paneCount ss =
          length
            [ ()
            | (_, txt, _, _, _) <- ss
            , let l = spanLabel txt
            , "Pane " `T.isPrefixOf` l
            , let rest = T.drop 5 l
            , not (T.null rest)
            , T.all isDigit (T.takeWhile (/= ' ') rest)
            ]
    unless (paneCount spansPane0 == 1) $ fail "selftest: expected exactly one pane initially"
    plus <- requireSpan "selftest: split button" (findExact "+" spansPane0)
    clickPos ui ctx' env base plus
    spansPane1 <- collectTextSpans ctx'
    unless (paneCount spansPane1 == 2) $ fail "selftest: split did not create a second pane"
    maxBtn <- requireSpan "selftest: maximize button" (findExact "M" spansPane1)
    clickPos ui ctx' env base maxBtn
    spansPaneMax <- collectTextSpans ctx'
    unless (hasText "maximized" spansPaneMax) $ fail "selftest: maximize did not fill the grid"
    restoreBtn <- requireSpan "selftest: restore button" (findExact "R" spansPaneMax)
    clickPos ui ctx' env base restoreBtn
    spansPane2 <- collectTextSpans ctx'
    unless (paneCount spansPane2 == 2) $ fail "selftest: restore lost a pane"
    closeBtn <- requireSpan "selftest: close button" (findExact "x" spansPane2)
    clickPos ui ctx' env base closeBtn
    spansPane3 <- collectTextSpans ctx'
    unless (paneCount spansPane3 == 1) $ fail "selftest: close did not remove a pane"
    -- Whole-pane drag-and-drop: re-split into two side-by-side panes, then grab
    -- the left pane anywhere and drop it on the center of the right pane. The
    -- two panes swap, so the pane whose title was leftmost must change.
    plus2 <- requireSpan "selftest: split button after close" (findExact "+" spansPane3)
    clickPos ui ctx' env base plus2
    spansPane4 <- collectTextSpans ctx'
    unless (paneCount spansPane4 == 2) $ fail "selftest: re-split did not yield two panes"
    let titles ss =
          [ (r, l)
          | (r, txt, _, _, _) <- ss
          , let l = spanLabel txt
          , "Pane " `T.isPrefixOf` l
          , let rest = T.drop 5 l
          , not (T.null rest)
          , T.all isDigit (T.takeWhile (/= ' ') rest)
          ]
        titleCenter = Harness.spanCenter . fst
        leftTitle4 = minimumBy (comparing (rectX . fst)) (titles spansPane4)
        rightTitle4 = maximumBy (comparing (rectX . fst)) (titles spansPane4)
    -- Edge drop: grab the right pane and drop it on the left pane's LEFT edge.
    -- The dragged pane must land on the left side of the new split, becoming
    -- the new leftmost pane.
    let edgeFrom = titleCenter rightTitle4
        edgeTo = V2 (rectX (fst leftTitle4) - 10) (rectY (fst leftTitle4) + 150)
    dragPos ui ctx' env base edgeFrom edgeTo
    spansEdge <- collectTextSpans ctx'
    unless (paneCount spansEdge == 2) $ fail "selftest: edge drop lost a pane"
    let leftAfterEdge = snd (minimumBy (comparing (rectX . fst)) (titles spansEdge))
    unless (leftAfterEdge /= snd leftTitle4) $ fail "selftest: edge drop did not land on the left side"
    -- Center drop: grab the left pane and drop it on the center of the right
    -- pane. The two panes swap, so the leftmost title must change again.
    let leftTitle5 = minimumBy (comparing (rectX . fst)) (titles spansEdge)
        rightTitle5 = maximumBy (comparing (rectX . fst)) (titles spansEdge)
        halfGap = (rectX (fst rightTitle5) - rectX (fst leftTitle5)) / 2
        fromSwap = titleCenter leftTitle5
        toSwap = V2 (rectX (fst rightTitle5) + halfGap) (rectY (fst rightTitle5) + 160)
    dragPos ui ctx' env base fromSwap toSwap
    spansPane5 <- collectTextSpans ctx'
    unless (paneCount spansPane5 == 2) $ fail "selftest: pane drag lost a pane"
    let afterSwap = snd (minimumBy (comparing (rectX . fst)) (titles spansPane5))
    unless (afterSwap /= snd leftTitle5) $ fail "selftest: pane drag did not swap the panes"
    -- Top-level drop: build three side-by-side panes, then drag the middle one
    -- to the grid's outer left edge. The grid must restructure at the top level
    -- into one pane on the left and the other two side-by-side on the right
    -- half, rather than a flat third column.
    plus3 <- requireSpan "selftest: split button for three panes" (findExact "+" spansPane5)
    clickPos ui ctx' env base plus3
    spansPane3c <- collectTextSpans ctx'
    unless (paneCount spansPane3c == 3) $ fail "selftest: third split did not yield three panes"
    let ts3 = titles spansPane3c
        leftT3 = minimumBy (comparing (rectX . fst)) ts3
        rightT3 = maximumBy (comparing (rectX . fst)) ts3
        -- Exactly three panes: the middle is the one that is neither extreme.
        midT3 = fromMaybe leftT3 (listToMaybe (filter (\t -> t /= leftT3 && t /= rightT3) ts3))
        fromTop = V2 (v2X (titleCenter midT3) + 70) (v2Y (titleCenter midT3) + 100)
        toTop = V2 (rectX (fst leftT3) - 10) (rectY (fst leftT3) + 150)
    dragPos ui ctx' env base fromTop toTop
    spansTop <- collectTextSpans ctx'
    unless (paneCount spansTop == 3) $ fail "selftest: top-level drop lost a pane"
    let tsTop = titles spansTop
        leftAfterTop = minimumBy (comparing (rectX . fst)) tsTop
        rightPanesTop = filter ((/= snd leftAfterTop) . snd) tsTop
        rightLeftTop = minimumBy (comparing (rectX . fst)) rightPanesTop
        rightRightTop = maximumBy (comparing (rectX . fst)) rightPanesTop
        -- A top-level wrap gives the left pane the whole left half, so the gap
        -- to the first right pane (~half the width) exceeds the gap between the
        -- two right panes (~a quarter); a flat third column keeps them equal.
        gapLeft = rectX (fst rightLeftTop) - rectX (fst leftAfterTop)
        gapRight = rectX (fst rightRightTop) - rectX (fst rightLeftTop)
    unless (snd leftAfterTop == snd midT3) $ fail "selftest: top-level drop did not move the middle pane left"
    unless (gapLeft > gapRight + 10) $ fail "selftest: top-level drop did not collapse the remaining panes onto one side"
    -- Pane headers are optional: turn them off and the titles vanish but the
    -- panes (and their content) remain.
    hdrBtn <- requireSpan "selftest: headers checkbox" (findExact "Pane headers" spansTop)
    clickPos ui ctx' env base hdrBtn
    spansPane6 <- collectTextSpans ctx'
    unless (paneCount spansPane6 == 0) $ fail "selftest: disabling pane headers did not hide them"
    unless (hasText "Contents of" spansPane6) $ fail "selftest: headerless panes lost their content"
    clickTab ui ctx' env base "Controls"
    spansCtl <- collectTextSpans ctx'
    unless (hasText "Feature" spansCtl) $ fail "selftest: Controls missing after tab back"
    feat0 <- requireSpan "selftest: Feature checkbox" (findRightmost "Feature" spansCtl)
    clickPos ui ctx' env base feat0
    spansOn <- collectTextSpans ctx'
    unless (hasText "on" spansOn) $ fail "selftest: checkbox did not turn Feature on"
    clickPos ui ctx' env base feat0
    spansOff <- collectTextSpans ctx'
    unless (hasText "off" spansOff) $ fail "selftest: checkbox did not turn Feature off"
    clickPos ui ctx' env base feat0
    spansOn2 <- collectTextSpans ctx'
    unless (hasText "on" spansOn2) $ fail "selftest: checkbox did not turn Feature on again"
    -- Theme is a radio fieldset: all options stay visible in the plain spans;
    -- click "Tomorrow Light" directly (the state card shows the old value).
    lightOpt <- requireSpan "selftest: Tomorrow Light option" (findExact "Tomorrow Light" spansOn2)
    clickPos ui ctx' env base lightOpt
    spansTheme <- collectTextSpans ctx'
    unless (hasText "Tomorrow Light" spansTheme) $ fail "selftest: radio did not pick Tomorrow Light"
    th <- getTheme ctx'
    unless (th == tomorrowMinLightTheme) $ fail "selftest: context theme was not updated to Tomorrow Light"
    vol <- requireSpan "selftest: Volume slider" (findRightmost "Volume" spansTheme)
    clickPos ui ctx' env base (V2 (v2X vol + 80) (v2Y vol))
    about <- requireSpan "selftest: About button" (findExact "About" spansTheme)
    clickPos ui ctx' env base about
    spansModal <- collectOverlayTextSpans ctx' base
    unless (hasText "Immediate-mode" spansModal) $ fail "selftest: About modal missing"
    -- Font metrics can make the body overflow the modal's initial viewport.
    -- The footer must be reachable by scrolling, not necessarily visible yet.
    unless (hasText "Close" spansModal) $ do
      bodyPos <- requireSpan "selftest: About body" (findExact "nano-ui" spansModal)
      drawOnce ui ctx' env (base {inputMousePos = bodyPos, inputScroll = V2 0 10})
      drawOnce ui ctx' env base
    spansModalFooter <- collectOverlayTextSpans ctx' base
    unless (hasText "Close" spansModalFooter) $ fail "selftest: About Close button unreachable"
    drawOnce ui ctx' env (base {inputKeys = inputKeysFromList [KeyEscape]})
    drawOnce ui ctx' env base
    spansClosed <- collectOverlayTextSpans ctx' base
    when (hasText "Immediate-mode" spansClosed) $ fail "selftest: Escape did not dismiss About"
    spansLatest <- collectTextSpans ctx'
    debugBtn <- requireSpan "selftest: Debug button" (findExact "Debug" spansLatest)
    clickPos ui ctx' env base debugBtn
    spansDebug <- collectOverlayTextSpans ctx' base
    unless (hasText "Frame" spansDebug) $ fail "selftest: Debug window missing"
    unless (hasText "Runtime" spansDebug) $ fail "selftest: Debug Runtime section missing"
    -- Formatted rows may be reused between samples, but must track the next
    -- backend refresh rather than freezing the first snapshot in the cache.
    sampledDraws <- newIORef 0
    let observeDebug = do
          snapshot <- askSdlDebug
          uiIO $ writeIORef sampledDraws (dbgPresents (dbgCore snapshot))
          ui
    drawOnce observeDebug ctx' env base
    previous <- readIORef sampledDraws
    threadDelay 300000
    drawOnce observeDebug ctx' env base
    current <- readIORef sampledDraws
    refreshed <- collectOverlayTextSpans ctx' base
    unless (current > previous && isJust (findExact (T.pack (show current)) refreshed)) $
      fail "selftest: Debug draws counter did not refresh"
  putStrLn "selftest: ok"

-- | Draw one frame of the UI under test (no presenting).
drawOnce :: NanoUI () -> Context -> SdlEnv -> Input -> IO ()
drawOnce ui ctx env inp = void (sdlDrawFrame ctx ui env inp False)

clickPos :: NanoUI () -> Context -> SdlEnv -> Input -> V2 -> IO ()
clickPos ui ctx env = Harness.clickPos (drawOnce ui ctx env)

clickTab :: NanoUI () -> Context -> SdlEnv -> Input -> T.Text -> IO ()
clickTab ui ctx env = Harness.clickTab collectTextSpans (drawOnce ui ctx env) ctx

dragPos :: NanoUI () -> Context -> SdlEnv -> Input -> V2 -> V2 -> IO ()
dragPos ui ctx env = Harness.dragPos (drawOnce ui ctx env)
