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
  withHiddenWindow 1280 800 (V2 640 400) (\o -> o {sdlAppContinuous = continuous}) $ \ctx env idle -> do
    -- The shaped path (fmShape / pushText) must measure runs as SDL3_ttf
    -- does, GPOS kerning and ligatures included, for pairs like To, AV and fi.
    let checkRun :: Float -> FontStyle -> String -> IO ()
        checkRun size st run = do
          (fm, _) <- ctxResolveFont ctx size WeightNormal st FontRegular
          (measured, _) <- ctxResolveMeasure ctx size WeightNormal st FontRegular (T.pack run)
          shaped <- lineWidthIO fm (T.pack run)
          when (abs (shaped - measured) > 0.01) $
            fail $ printf "selftest: %s shaped width mismatch for '%s': measure=%.2f, shaped=%.2f" (show st) run measured shaped
    for_ [FontStyleNormal, FontStyleItalic] $ \st -> do
      checkRun 16 st "Slanted synthetic italic font style."
      mapM_ (checkRun 20 st) ["To", "AV", "fi"]
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
    (ctx', base) <- syncDisplay ctx env idle
    let drawWith frameUi inp = void (sdlDrawFrame ctx' frameUi env inp False)
        drawOnce = drawWith demoUi
        clickPos = Harness.clickPos drawOnce base
        dragPos = Harness.dragPos drawOnce base
        clickTab = Harness.clickTab collectTextSpans drawOnce ctx' base
        pressKey k = mapM_ drawOnce [Harness.keyInp k base, base]
        -- The frame's text spans, failing with @msg@ unless one holds @needle@.
        spansWith msg needle = do
          spans <- collectTextSpans ctx'
          spans <$ expectText msg needle spans
        expect msg needle = void (spansWith msg needle)
        expectOverlay msg needle = expectText msg needle =<< collectOverlayTextSpans ctx' base
    void (sdlDrawFrame ctx' demoUi env base True)
    spans0 <- spansWith "selftest: Controls body missing" "Feature"
    -- A field value wider than the glyph atlas draws glyph by glyph, and the
    -- text drawn before and after it in the frame survives.
    nameLbl <- requireSpan "selftest: Name label" (findRightmost "Name" spans0)
    clickPos (V2 (v2X nameLbl + 80) (v2Y nameLbl))
    drawOnce (base {inputChars = T.replicate 400 "f"})
    drawOnce base
    spansLong <- spansWith "selftest: long field text lost the tab content" "Feature"
    unless (length spansLong >= length spans0 - 1) $
      fail "selftest: long field text collapsed the span set"
    clickTab "Table"
    spansTable <- spansWith "selftest: table body missing after Table tab" "David"
    clickPos =<< requireSpan "selftest: Name header" (findHeader "Name" spansTable)
    spansSorted <- spansWith "selftest: header click did not toggle sort" "descending"
    dept <- requireSpan "selftest: Dept header" (findHeader "Dept" spansSorted)
    dragPos dept (V2 (v2X dept + 180) (v2Y dept))
    expect "selftest: table missing after header drag" "Sonia"
    clickTab "List"
    spansTree <- spansWith "selftest: tree missing after List tab" "src"
    clickPos =<< requireSpan "selftest: README.md" (findExact "README.md" spansTree)
    expect "selftest: tree click did not select README.md" "7"
    clickTab "Typography"
    spansType <- spansWith "selftest: typography missing after Typography tab" "Live Playground"
    drawOnce (base {inputScroll = V2 0 (-350)})
    drawOnce base
    sizeSpan <- requireSpan "selftest: Size slider" (findRightmost "Size" spansType)
    for_ [20, 60, 100, 140, 180, 50, 120, -60, -100, 0 :: Float] $ \dx ->
      dragPos sizeSpan (V2 (v2X sizeSpan + dx) (v2Y sizeSpan))
    expect "selftest: typography missing after size changes" "Live Playground"
    clickTab "Panes"
    spansPane0 <- spansWith "selftest: pane grid missing after Panes tab" "Pane 1"
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
        -- Click the button labelled @name@ in @spans@, and return the spans
        -- after it.
        press name spans = do
          clickPos =<< requireSpan ("selftest: button " <> T.unpack name) (findExact name spans)
          collectTextSpans ctx'
    unless (paneCount spansPane0 == 1) $ fail "selftest: expected exactly one pane initially"
    spansPane1 <- press "+" spansPane0
    unless (paneCount spansPane1 == 2) $ fail "selftest: split did not create a second pane"
    spansPaneMax <- press "M" spansPane1
    expectText "selftest: maximize did not fill the grid" "maximized" spansPaneMax
    spansPane2 <- press "R" spansPaneMax
    unless (paneCount spansPane2 == 2) $ fail "selftest: restore lost a pane"
    spansPane3 <- press "x" spansPane2
    unless (paneCount spansPane3 == 1) $ fail "selftest: close did not remove a pane"
    spansPane4 <- press "+" spansPane3
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
    spansPane3c <- press "+" spansPane5
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
    spansPane6 <- press "Pane headers" spansTop
    unless (paneCount spansPane6 == 0) $ fail "selftest: disabling pane headers did not hide them"
    expectText "selftest: headerless panes lost their content" "Contents of" spansPane6
    clickTab "Controls"
    spansCtl <- spansWith "selftest: Controls missing after tab back" "Feature"
    feat0 <- requireSpan "selftest: Feature checkbox" (findRightmost "Feature" spansCtl)
    clickPos feat0
    expect "selftest: checkbox did not turn Feature on" "on"
    clickPos feat0
    expect "selftest: checkbox did not turn Feature off" "off"
    clickPos feat0
    spansOn2 <- spansWith "selftest: checkbox did not turn Feature on again" "on"
    -- Theme is a radio fieldset: all options stay visible in the plain spans;
    -- click "Tomorrow Light" directly (the state card shows the old value).
    lightOpt <- requireSpan "selftest: Tomorrow Light option" (findExact "Tomorrow Light" spansOn2)
    clickPos lightOpt
    spansTheme <- spansWith "selftest: radio did not pick Tomorrow Light" "Tomorrow Light"
    th <- getTheme ctx'
    unless (th == tomorrowMinLightTheme) $ fail "selftest: context theme was not updated to Tomorrow Light"
    -- Following the system gives the default theme in light or dark, as SDL
    -- reports the desktop.
    systemOpt <- requireSpan "selftest: Follow system option" (findExact "Follow system" spansTheme)
    clickPos systemOpt
    appearance <- getSystemAppearance ctx'
    thSystem <- getTheme ctx'
    unless (thSystem == (if appearance == Just AppearanceDark then defaultTheme else defaultLightTheme)) $
      fail "selftest: following the system did not pick the default theme's light or dark version"
    clickPos lightOpt
    vol <- requireSpan "selftest: Volume slider" (findRightmost "Volume" spansTheme)
    clickPos (V2 (v2X vol + 80) (v2Y vol))
    clickPos =<< requireSpan "selftest: About button" (findExact "About" spansTheme)
    spansModal <- collectOverlayTextSpans ctx' base
    expectText "selftest: About modal missing" "Immediate-mode" spansModal
    -- Font metrics can make the body overflow the modal's initial viewport.
    -- The footer must be reachable by scrolling, not necessarily visible yet.
    unless (hasText "Close" spansModal) $ do
      bodyPos <- requireSpan "selftest: About body" (findExact "nano-ui" spansModal)
      drawOnce (base {inputMousePos = bodyPos, inputScroll = V2 0 10})
      drawOnce base
    expectOverlay "selftest: About Close button unreachable" "Close"
    pressKey KeyEscape
    spansClosed <- collectOverlayTextSpans ctx' base
    when (hasText "Immediate-mode" spansClosed) $ fail "selftest: Escape did not dismiss About"
    -- F1 is the About button's shortcut.
    pressKey (KeyF 1)
    expectOverlay "selftest: F1 did not open About" "Immediate-mode"
    pressKey KeyEscape
    void . press "Debug" =<< collectTextSpans ctx'
    expectOverlay "selftest: Debug window missing" "Frame"
    expectOverlay "selftest: Debug Runtime section missing" "Runtime"
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
