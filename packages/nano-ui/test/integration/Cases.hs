module Cases
  ( module Cases.Animation
  , module Cases.Combo
  , module Cases.ContextMenu
  , module Cases.CustomWidget
  , module Cases.Damage
  , module Cases.Demo
  , module Cases.Modal
  , module Cases.NoThunks
  , module Cases.Scroll
  , module Cases.Select
  , module Cases.Tabs
  , module Cases.TextInput
  , module Cases.Tooltip
  , module Cases.Window
  , module Cases.Font
  , runAsciiTest
  , runAspectLayoutTest
  , runBase16ThemeTest
  , runCheckboxTest
  , runCheckboxInitialTest
  , runColumnCardWrapTest
  , runCompactHostTest
  , runDemoWrapWideOrderTest
  , runDrawTest
  , runDrawingTest
  , runEmbedStateTest
  , runFitSizingTest
  , runFitMutedWidthTest
  , runFlexShrinkTest
  , runGridTest
  , runGrowFitsWindowTest
  , runGrowEqualSplitTest
  , runGrowContentFloorTest
  , runGrowLockCascadeTest
  , runGrowEqualSplitHeightTest
  , runGrowWrapPushesSiblingTest
  , runHostProfileGapTest
  , runHostProfileMeasureTest
  , runHostSlotTest
  , runHoverDamageTest
  , runHoverSkipTest
  , runHoverTest
  , runIconSetTest
  , runIdKeyedListTest
  , runIdleTest
  , runIdStabilityTest
  , runIdUniquenessTest
  , runIdZeroAllocTest
  , runImageTest
  , runInteractionTest
  , runLabelAlignEndTest
  , runLayoutTest
  , runOverlayTest
  , runPanelPaintsTest
  , runPaneGridMixedDragTest
  , runPercentLayoutTest
  , runPercentGapShrinkTest
  , runPointerCursorCheckboxTest
  , runPointerCursorTest
  , runReduceClickTest
  , runReduceIdentityTest
  , runReduceMessagesTest
  , runReduceUpdatesTest
  , runRowPanelLayoutTest
  , runSliderFillWidthTest
  , runSliderTest
  , runTabFocusTest
  , runTextMultilineTest
  , runTextWrapAssignedTest
  , runTextWrapTest
  , runTwoCardWrapTest
  , runUseFlagClickTest
  , runWidgetNoStringEmitTest
  , runWithKeyTest
  , runSearchFieldClearTest
  , runSearchFieldDebounceTest
  ) where

import Cases.Animation
import Cases.Combo
import Cases.ContextMenu
import Cases.CustomWidget
import Cases.Damage
import Cases.Demo
import Cases.Modal
import Cases.NoThunks
import Cases.Scroll
import Cases.Select
import Cases.Tabs
import Cases.TextInput
import Cases.Tooltip
import Cases.Window
import Cases.Font
import Control.Monad (forM, replicateM, void, when)
import Control.Concurrent (threadDelay)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (nub, sort)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import Effectful (liftIO)
import Effectful.State.Static.Local (State, evalState, get, modify)
import NanoUI
import NanoUI.Context (Context (..))
import NanoUI.Layout.Arena (NodeType (..), arenaCount, getNodeType, getNodeValue)
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, measureRespW, runClickReduce, withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , checkLabelAlignEnd
  , checkLabelAlignEndInk
  , clickPair
  , spanXOf
  , spanYOf
  , vertUv
  , warmup2
  , warmupDraw
  , withInputOff
  )
import NanoUI.Widgets.SplitPane
  ( DropTarget (..)
  , GridNode (..)
  , dropPreview
  , dropTargetForPane
  , layoutNode
  , topLevelDropTarget
  , treeMovePane
  )

runHostProfileGapTest :: Context -> IORef Int -> IO ()
runHostProfileGapTest _ failed = do
  let defaultGap = layoutGap defaultLayout
      cellGap = resolveLayoutGap CellHost (monospaceMetrics 1) defaultGap
      pixelGap = resolveLayoutGap PixelHost (monospaceMetrics 16) defaultGap
  assertEq failed cellGap 1
  assertEq failed pixelGap defaultGap

runHostProfileMeasureTest :: Context -> IORef Int -> IO ()
runHostProfileMeasureTest _ failed = do
  let txt = "abcde"
      fmCell = monospaceMetrics 1
      fmPixel = monospaceMetrics 16
      cellW = textDisplayWidth CellHost fmCell txt
      pixelW = textDisplayWidth PixelHost fmPixel txt
  assertEq failed cellW (fromIntegral (terminalPaintColumns txt))
  assertEq failed pixelW (fromIntegral (T.length txt) * fmAdvance fmPixel ' ')

runIdStabilityTest :: Context -> IORef Int -> IO ()
runIdStabilityTest ctx failed = do
  let inp = withInput 100 100
      sample = runFrame ctx inp (column (replicateM 3 nextId))
  (ids1, _, _, _) <- sample
  (ids2, _, _, _) <- sample
  assertEq failed ids1 ids2
  case ids1 of
    [a, b, c] -> assert failed (a /= b && b /= c && a /= c)
    _ -> assert failed False

runIdUniquenessTest :: Context -> IORef Int -> IO ()
runIdUniquenessTest ctx failed = do
  (ids, _, _, _) <- runFrame ctx (withInput 100 100) (column (replicateM 3 nextId))
  case ids of
    [a, b, c] -> assert failed (a /= b && b /= c && a /= c)
    _ -> assert failed False

runIdZeroAllocTest :: Context -> IORef Int -> IO ()
runIdZeroAllocTest ctx failed = do
  let inp = withInput 100 100
  _ <- runFrame ctx inp $ column $ burstNextIds 4096
  assert failed True

runIdKeyedListTest :: Context -> IORef Int -> IO ()
runIdKeyedListTest ctx failed = do
  let inp = withInput 200 200
      keyedIds :: [String] -> IO ([WidgetId], [FrameMsg], DrawData, Bool)
      keyedIds keys = runFrame ctx inp (column (mapM (\k -> keyed k nextId) keys))
      idFor :: String -> [String] -> [WidgetId] -> Maybe WidgetId
      idFor key keys ids = lookup key (zip keys ids)
  (idsA, _, _, _) <- keyedIds ["a", "b", "c"]
  (idsPrep, _, _, _) <- keyedIds ["x", "a", "b", "c"]
  (idsApp, _, _, _) <- keyedIds ["a", "b", "c", "y"]
  (idsRev, _, _, _) <- keyedIds ["c", "b", "a"]
  assertEq failed (idFor "a" ["a", "b", "c"] idsA) (idFor "a" ["x", "a", "b", "c"] idsPrep)
  assertEq failed (idFor "b" ["a", "b", "c"] idsA) (idFor "b" ["a", "b", "c", "y"] idsApp)
  assertEq failed (idFor "c" ["a", "b", "c"] idsA) (idFor "c" ["c", "b", "a"] idsRev)

runFitSizingTest :: Context -> IORef Int -> IO ()
runFitSizingTest ctx failed = do
  let inp = withInput 400 100
  w1 <- measureRespW ctx inp (column (label "hi"))
  w2 <- measureRespW ctx inp (column (label "a much longer label"))
  assert failed (w1 > 0 && w1 < 400)
  assertGt failed w2 w1

runFitMutedWidthTest :: Context -> IORef Int -> IO ()
runFitMutedWidthTest ctx failed = do
  let inp = withInput 400 100
      ui = columnWith tight (muted "HelloFitMuted")
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  case [w | (Rect _ _ w _, t, _, _, _) <- spans, "HelloFitMuted" `T.isInfixOf` t] of
    (w : _) -> assertGt failed w 8
    _ -> assert failed False

runWithKeyTest :: Context -> IORef Int -> IO ()
runWithKeyTest ctx failed = do
  let inp = withInput 200 200
      action = column (do a <- keyed (0 :: Int) nextId; b <- keyed (1 :: Int) nextId; pure [a, b])
  (idsA, _, _, _) <- runFrame ctx inp action
  (idsB, _, _, _) <- runFrame ctx inp action
  case (idsA, idsB) of
    ([a0, a1], [b0, b1]) -> assert failed (a0 == b0 && a1 == b1 && a0 /= a1)
    _ -> assert failed False

runLayoutTest :: Context -> IORef Int -> IO ()
runLayoutTest ctx failed = do
  let ui = column' (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutGap = 8}) $ do
        _ <- row' (defaultLayout {layoutWidth = Grow 1}) (spacer (Grow 1) Fit >> label "grow test")
        label "nested"
  (_, _, draw, _) <- runFrame ctx (withInput 400 300) ui
  assertGt failed (drawVertexCount draw) 0

runRowPanelLayoutTest :: Context -> IORef Int -> IO ()
runRowPanelLayoutTest ctx failed = do
  let inp = withInput 800 600
      ui = rowWith (tight . fillW) $ do
        panelWith (minW 200 . fillH) (void (label "Side"))
        panelWith (grow . fillW . fillH) (rowWith (tight . gap 8) (button "Left" >> button "Right"))
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  assert failed (rectX (respRect resp) >= 190)

runColumnCardWrapTest :: Context -> IORef Int -> IO ()
runColumnCardWrapTest ctx failed = do
  let inp = withInput 520 800
      ui = responsiveRowCol 720 (tight . gap 8 . fillW $ defaultLayout) $ do
        columnWith (tight . gap 8 . fillW) (card (void (label "LeftTop")) >> card (void (label "LeftBot")))
        card (void (label "Right"))
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  case (spanYOf "Right" spans, spanYOf "LeftTop" spans) of
    ([ry], [ly]) -> assertGt failed ry (ly + 1)
    _ -> assert failed False

runTwoCardWrapTest :: Context -> IORef Int -> IO ()
runTwoCardWrapTest ctx failed = do
  let inp = withInput 520 800
      ui = responsiveRowCol 720 (tight . gap 8 . fillW $ defaultLayout) (card (void (label "CardA")) >> card (void (label "CardB")))
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  case (spanYOf "CardA" spans, spanYOf "CardB" spans) of
    ([ay], [by]) -> assertGt failed by (ay + 1)
    _ -> assert failed False

runDemoWrapWideOrderTest :: Context -> IORef Int -> IO ()
runDemoWrapWideOrderTest ctx failed = do
  let inp = withInput 1200 800
      ui = responsiveRowCol 720 (tight . gap 8 . fillW $ defaultLayout) $ do
        columnWith (tight . gap 8 . fillW) (card (void (label "State")) >> card (void (label "Gallery")))
        card (void (label "Controls"))
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  case (spanXOf "State" spans, spanXOf "Controls" spans) of
    ([sx], [cx]) -> assertGt failed cx (sx + 1)
    _ -> assert failed False

runDrawTest :: Context -> IORef Int -> IO ()
runDrawTest ctx failed = do
  (_, _, draw, _) <- runFrame ctx (withInput 100 100) (column (label "draw"))
  assert failed (drawIndexCount draw >= 6 && not (drawCmdNull draw))

runDrawingTest :: Context -> IORef Int -> IO ()
runDrawingTest ctx failed = do
  let ui =
        drawing (fixedWH 80 40 defaultLayout) $ \r ->
          V.singleton
            ( Stroke
                (rectX r)
                (rectY r + rectH r * 0.5)
                (rectX r + rectW r)
                (rectY r + rectH r * 0.5)
                2
                (colorRGBA 255 0 0 255)
            )
      inp = withInput 200 80
  (_, _, draw, _) <- runFrame ctx inp ui
  assert failed (drawIndexCount draw >= 6 && not (drawCmdNull draw))
  (_, _, draw2, _) <- runFrame ctx inp ui
  assert failed (drawIndexCount draw2 >= 6 && not (drawCmdNull draw2))

runOverlayTest :: Context -> IORef Int -> IO ()
runOverlayTest ctx failed = do
  let inp0 = withInput 200 80
      ui = column (button "Hover" >>= \btn -> tooltip btn "tip")
  _ <- runFrame ctx inp0 ui
  (_, _, draw, _) <- runFrame ctx (inp0 {inputMousePos = V2 10 10}) ui
  assert failed (any ((== LayerOverlay) . cmdLayer) (drawCmdElems draw))

runInteractionTest :: Context -> IORef Int -> IO ()
runInteractionTest ctx failed = do
  let inp0 = withInput 200 100
      ui = column (button "Click")
      (press, release) = clickPair inp0 (V2 10 10)
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx press ui
  (resp, msgs, _, _) <- runFrame ctx release ui
  assert failed (respClicked resp && null msgs)

runHoverTest :: Context -> IORef Int -> IO ()
runHoverTest ctx failed = do
  let inp0 = withInput 200 100
      ui = column (button "Hover")
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputMousePos = V2 10 10}) ui
  hot <- getHotId ctx
  assert failed (hashWidgetId hot /= 0)

runPointerCursorTest :: Context -> IORef Int -> IO ()
runPointerCursorTest ctx failed = do
  let inp0 = withInput 200 100
      ui = column (button "Click")
  _ <- runFrame ctx inp0 ui
  let inp1 = inp0 {inputMousePos = V2 10 10}
  _ <- runFrame ctx inp1 ui
  want <- pointerCursorWanted ctx inp1
  assert failed want
  let inp2 = inp0 {inputMousePos = V2 (-1) (-1)}
  _ <- runFrame ctx inp2 ui
  want2 <- pointerCursorWanted ctx inp2
  assert failed (not want2)

runPointerCursorCheckboxTest :: Context -> IORef Int -> IO ()
runPointerCursorCheckboxTest ctx failed = do
  let inp0 = withInput 200 100
      ui = column (checkbox "Feature" False)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      hover = inp0 {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)}
  _ <- runFrame ctx hover ui
  want <- pointerCursorWanted ctx hover
  assert failed want
  let click = hover {inputMouseDown = True, inputMousePressed = True, inputMouseReleased = False}
  _ <- runFrame ctx click ui
  wantClick <- pointerCursorWanted ctx click
  assert failed wantClick

runImageTest :: Context -> IORef Int -> IO ()
runImageTest ctx failed = do
  let px a b c = BS.pack (concat (replicate 16 [a, b, c, 255]))
  ok1 <- registerImage ctx (ImageId 1) 4 4 (px 255 0 0)
  ok7 <- registerImage ctx (ImageId 7) 4 4 (px 0 0 255)
  assert failed (ok1 && ok7)
  let inp0 = withInput 320 200
      imgLayout = defaultLayout {layoutWidth = Fixed 40, layoutHeight = Fixed 24}
      ui = row (image imgLayout (ImageId 1) >> image imgLayout (ImageId 7))
  (resp, drawData) <- warmupDraw ctx inp0 ui
  let Rect _ _ w h = respRect resp
  assert failed (abs (w - 40) <= 0.5 && abs (h - 24) <= 0.5)
  let texCmds = filter (\c -> cmdTextureId c == atlasTextureId) (drawCmdElems drawData)
  assertEq failed (length texCmds) 1
  assert failed (any (\c -> cmdIndexCount c == 12) texCmds)
  (u0, _) <- vertUv drawData 0
  (u4, _) <- vertUv drawData 4
  assert failed (abs (u0 - u4) >= 1e-6)
  let missing = image imgLayout (ImageId 0)
  _ <- runFrame ctx inp0 missing
  (_, _, missingData, _) <- runFrame ctx inp0 missing
  assert failed (not (any (\c -> cmdTextureId c == atlasTextureId) (drawCmdElems missingData)))

runIdleTest :: Context -> IORef Int -> IO ()
runIdleTest _ failed = do
  ctx <- newContext
  let inp = withInputOff 100 100
  _ <- runFrame ctx inp (label "idle")
  need <- needsRedraw ctx inp inp
  assert failed (not need)

runHoverSkipTest :: Context -> IORef Int -> IO ()
runHoverSkipTest _ failed = do
  ctx <- newContext
  let ui = column (button "OK")
      inp0 = withInputOff 240 80
  (resp, _, _, _) <- runFrame ctx inp0 ui >>= \_ -> runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      inside = V2 (rx + rw / 2) (ry + rh / 2)
      inside2 = V2 (rx + rw / 2 + 1) (ry + rh / 2)
      inp1 = inp0 {inputMousePos = inside}
      inp2 = inp0 {inputMousePos = inside2}
  needEnter <- needsRedraw ctx inp0 inp1
  assert failed needEnter
  _ <- runFrame ctx inp1 ui
  let drain = inp1 {inputDeltaTime = 1}
  _ <- runFrame ctx drain ui
  needStay <- needsRedraw ctx drain inp2
  assert failed (not needStay)
  let inpClick = inp1 {inputMouseDown = True, inputMousePressed = True}
  needClick <- needsRedraw ctx drain inpClick
  assert failed needClick

runHoverDamageTest :: Context -> IORef Int -> IO ()
runHoverDamageTest _ failed = do
  ctx <- newContext
  let ui = column (button "OK")
      inp0 = withInputOff 240 80
  _ <- runFrame ctx inp0 ui
  d0 <- takeDamage ctx
  assertEq failed d0 DamageFull
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      inside = V2 (rx + rw / 2) (ry + rh / 2)
      inp1 = inp0 {inputMousePos = inside}
  _ <- runFrame ctx inp1 ui
  d1 <- takeDamage ctx
  case d1 of
    DamageFull -> assert failed False
    DamageClip (Rect _ _ w h) -> assert failed (w * h < 240 * 80 * 0.5)
  let inpClick = inp1 {inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx inpClick ui
  d2 <- takeDamage ctx
  case d2 of
    DamageFull -> assert failed False
    DamageClip (Rect _ _ w h) -> assert failed (w * h < 240 * 80 * 0.5)


runAsciiTest :: Context -> IORef Int -> IO ()
runAsciiTest ctx failed = do
  (_, _, draw, _) <- runFrame ctx (withInput 40 10) (column' (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "snap"))
  let ascii = renderASCII 40 10 draw
  assertEq failed (length ascii) 10
  assert failed (not (all (all (== ' ')) ascii))

runIconSetTest :: Context -> IORef Int -> IO ()
runIconSetTest _ failed = do
  assertEq failed (parseIconSet "nerd") (Just IconsNerd)
  assertEq failed (parseIconSet "FontAwesome") (Just IconsFontAwesome)
  assertEq failed (parseIconSet " ascii ") (Just IconsAscii)
  assertEq failed (parseIconSet "auto") Nothing
  assertEq failed (iconsFor IconsAscii) asciiIcons
  assertEq failed (iconsFor IconsNerd) glyphIcons
  assertEq failed (iconsFor IconsFontAwesome) glyphIcons
  assertEq failed (checkboxMark glyphIcons True) (iconChecked glyphIcons)
  assertEq failed (terminalTextColumns (iconChecked glyphIcons)) (terminalTextColumns (iconUnchecked glyphIcons))

runCheckboxTest :: Context -> IORef Int -> IO ()
runCheckboxTest ctx failed = do
  let inp0 = withInput 200 100
      ui = column (checkbox "Opt" False)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      (press, release) = clickPair inp0 (V2 (rx + 1) (ry + 0.5))
  _ <- runFrame ctx press ui
  ((_, checked), _, _, _) <- runFrame ctx release ui
  assert failed checked
  ((_, checked2), _, _, _) <- runFrame ctx inp0 ui
  assert failed checked2
  _ <- runFrame ctx press ui
  ((_, checked3), _, _, _) <- runFrame ctx release ui
  assert failed (not checked3)
  ((_, checked4), _, _, _) <- runFrame ctx inp0 ui
  assert failed (not checked4)

-- | An unclicked checkbox must keep rendering its initial value; the frame's
-- post-UI value sync must not reset it to unchecked when no state is stored.
runCheckboxInitialTest :: Context -> IORef Int -> IO ()
runCheckboxInitialTest ctx failed = do
  let inp0 = withInput 200 100
      ui = column (checkbox "Opt" True)
  (resp, _) <- warmup2 ctx inp0 ui
  assertCheckboxNodeValue failed ctx 1
  let Rect rx ry _ _ = respRect resp
      (press, release) = clickPair inp0 (V2 (rx + 1) (ry + 0.5))
  _ <- runFrame ctx press ui
  ((_, checked), _, _, _) <- runFrame ctx release ui
  assert failed (not checked)
  assertCheckboxNodeValue failed ctx 0
  _ <- runFrame ctx press ui
  ((_, checked2), _, _, _) <- runFrame ctx release ui
  assert failed checked2
  assertCheckboxNodeValue failed ctx 1

assertCheckboxNodeValue :: IORef Int -> Context -> Float -> IO ()
assertCheckboxNodeValue failed ctx expected = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  vals <- forM [0 .. n - 1] $ \i -> do
    nt <- getNodeType na i
    case nt of
      NodeCheckbox -> do
        v <- getNodeValue na i
        pure [v]
      _ -> pure []
  case concat vals of
    [v] -> assertEq failed v expected
    vs -> assert failed (vs == [expected])

runSliderTest :: Context -> IORef Int -> IO ()
runSliderTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column (slider "Vol" 0 100 10)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Vol" rx ry rw rh
      drag = V2 (rectX track + rectW track * 0.75) (rectY track + rectH track / 2)
  ((_, val), _, _, _) <- runFrame ctx (inp0 {inputMousePos = drag, inputMouseDown = True, inputMousePressed = True}) ui
  assertGt failed val 10

runSliderFillWidthTest :: Context -> IORef Int -> IO ()
runSliderFillWidthTest ctx failed = do
  let inp0 = withInput 400 120
      ui = columnWith fillW (slider "Vol" 0 100 0)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
  assertGt failed rw 300
  let track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Vol" rx ry rw rh
      endDrag = V2 (rectX track + rectW track - 2) (rectY track + rectH track / 2)
  ((_, val), _, _, _) <- runFrame ctx (inp0 {inputMousePos = endDrag, inputMouseDown = True, inputMousePressed = True}) ui
  assertGt failed val 90

runTabFocusTest :: Context -> IORef Int -> IO ()
runTabFocusTest ctx failed = do
  let inp0 = withInput 200 120
      ui = column (button "One" >> button "Two" >> pure ())
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus1 <- getFocusId ctx
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus2 <- getFocusId ctx
  assert failed (focus1 /= WidgetId 0 && focus2 /= WidgetId 0 && focus1 /= focus2)

runTextWrapTest :: Context -> IORef Int -> IO ()
runTextWrapTest _ failed = do
  ctx <- newCellContext
  let inp = withInput 40 10
      long = T.replicate 24 (T.pack "x")
  _ <- runFrame ctx inp (labelEx (defaultLayout {layoutMaxW = 8}) long)
  spans <- collectTextSpans ctx
  assert failed (length spans >= 3)

runTextWrapAssignedTest :: Context -> IORef Int -> IO ()
runTextWrapAssignedTest _ failed = do
  ctx <- newCellContext
  let inp = withInput 20 12
      long = T.replicate 24 (T.pack "x")
      ui = column' (defaultLayout {layoutWidth = Fixed 8, layoutPadding = Padding 0 0 0 0, layoutGap = 0})
             (labelEx (defaultLayout {layoutWidth = Grow 1}) long)
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  assert failed (length spans >= 3)

runTextMultilineTest :: Context -> IORef Int -> IO ()
runTextMultilineTest _ failed = do
  ctx <- newCellContext
  _ <- runFrame ctx (withInput 40 10) (labelEx (tight . fontMono $ defaultLayout) "aa\nbb\ncc")
  spans <- collectTextSpans ctx
  let rows = sort [(round y :: Int, txt) | (Rect _ y _ _, txt, _, _, _) <- spans]
  assertEq failed (map snd rows) ["aa", "bb", "cc"]
  case map fst rows of
    [a, b, c] -> assert failed (b == a + 1 && c == b + 1)
    _ -> assert failed False

runGridTest :: Context -> IORef Int -> IO ()
runGridTest _ failed = do
  ctx <- newCellContext
  let ui = grid' 2 (defaultLayout {layoutWidth = Fixed 4, layoutGap = 0, layoutPadding = Padding 0 0 0 0})
             (label "AA" >> label "BB" >> label "CC" >> label "DD" >> pure ())
  _ <- runFrame ctx (withInput 30 10) ui
  spans <- collectTextSpans ctx
  let ys = nub [round y :: Int | (Rect _ y _ _, _, _, _, _) <- spans]
  assert failed (length ys >= 2)

runFlexShrinkTest :: Context -> IORef Int -> IO ()
runFlexShrinkTest _ failed = do
  ctx <- newCellContext
  let ui = row' (defaultLayout {layoutWidth = Fixed 5, layoutGap = 0, layoutPadding = Padding 0 0 0 0})
             (labelEx (defaultLayout {layoutWidth = Shrink 1}) "AA" >> labelEx (defaultLayout {layoutWidth = Shrink 1}) "BB" >> labelEx (defaultLayout {layoutWidth = Shrink 1}) "CC" >> pure ())
  _ <- runFrame ctx (withInput 20 10) ui
  spans <- collectTextSpans ctx
  assertEq failed (length spans) 3
  let lastX = maximum [x | (Rect x _ _ _, _, _, _, _) <- spans]
  assert failed (lastX <= 3.5)

runGrowFitsWindowTest :: Context -> IORef Int -> IO ()
runGrowFitsWindowTest ctx failed = do
  let inp = withInput 10 10
      ui = row' (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutPadding = Padding 0 0 0 0, layoutGap = 0})
             (do a <- spacer (Grow 1) (Fixed 8); b <- spacer (Grow 1) (Fixed 8); pure (a, b))
  (ra, rb) <- warmup2 ctx inp ui
  let Rect x1 _ w1 _ = respRect ra
      Rect x2 _ w2 _ = respRect rb
  assert failed (w1 > 0 && w2 > 0 && x1 >= -0.01 && x2 + w2 <= 10.01 && abs (w1 - w2) <= 0.5)

runPercentLayoutTest :: Context -> IORef Int -> IO ()
runPercentLayoutTest ctx failed = do
  let inp = withInput 200 80
      ui = rowWith (fixedW 200 . tight . gap 0) $ do
        a <- labelEx (percent 25 . tight $ defaultLayout) "A"
        b <- labelEx (percent 75 . tight $ defaultLayout) "B"
        pure (a, b)
  (a, b) <- warmup2 ctx inp ui
  let Rect _ _ wa _ = respRect a
      Rect _ _ wb _ = respRect b
  assert failed (abs (wa - 50) <= 1 && abs (wb - 150) <= 1)

-- | Percent children flex like CSS: two 50% columns plus a gap must give back
-- the overflow so the pair lands exactly on the row width (equal halves, no
-- spill past the row's right edge).
runPercentGapShrinkTest :: Context -> IORef Int -> IO ()
runPercentGapShrinkTest ctx failed = do
  let inp = withInput 300 80
      ui = rowWith (fixedW 206 . tight . gap 6) $ do
        a <- labelEx (percent 50 . tight $ defaultLayout) "A"
        b <- labelEx (percent 50 . tight $ defaultLayout) "B"
        pure (a, b)
  (a, b) <- warmup2 ctx inp ui
  let Rect xa _ wa _ = respRect a
      Rect xb _ wb _ = respRect b
  assert failed (abs (wa - 100) <= 0.5 && abs (wb - 100) <= 0.5)
  assert failed (abs (xb - (xa + wa + 6)) <= 0.5)

-- | Grow children split the free space by factor with a min-content floor:
-- two fillW labels with unequal text come out equal when both fit their share
-- (instead of the old content + share-of-slack split). The row is fixed-width,
-- so grow labels measure their full text (12px per char in this context).
runGrowEqualSplitTest :: Context -> IORef Int -> IO ()
runGrowEqualSplitTest ctx failed = do
  let inp = withInput 210 40
      ui = rowWith (fixedW 210 . tight . gap 0) $ do
        a <- labelEx (fillW . tight $ defaultLayout) "A"
        b <- labelEx (fillW . tight $ defaultLayout) "AAAAA"
        pure (a, b)
  (a, b) <- warmup2 ctx inp ui
  let Rect _ _ wa _ = respRect a
      Rect _ _ wb _ = respRect b
  assert failed (abs (wa - 105) <= 0.5 && abs (wb - 105) <= 0.5)

-- | The floor in action: a grow child whose content needs more than its share
-- takes exactly its content width, and the sibling re-shares what is left.
runGrowContentFloorTest :: Context -> IORef Int -> IO ()
runGrowContentFloorTest ctx failed = do
  let inp = withInput 200 40
      ui = rowWith (fixedW 200 . tight . gap 0) $ do
        a <- labelEx (fillW . tight $ defaultLayout) "A"
        b <- labelEx (fillW . tight $ defaultLayout) (T.replicate 15 "A")
        pure (a, b)
  (a, b) <- warmup2 ctx inp ui
  let Rect _ _ wa _ = respRect a
      Rect _ _ wb _ = respRect b
  assert failed (abs (wb - 180) <= 0.5)
  assert failed (abs (wa - 20) <= 0.5)
  assert failed (abs (wa + wb - 200) <= 0.5)

-- | Multi-sweep cascade: locking the largest child shrinks the share pool,
-- which must lock the middle child on a later sweep and re-share to the
-- smallest. If the solver stopped after one sweep, the middle child would get
-- 55 (half the remainder) instead of its 60 content size.
runGrowLockCascadeTest :: Context -> IORef Int -> IO ()
runGrowLockCascadeTest ctx failed = do
  let inp = withInput 240 40
      ui = rowWith (fixedW 240 . tight . gap 0) $ do
        a <- labelEx (fillW . minW 12 . tight $ defaultLayout) "A"
        b <- labelEx (fillW . minW 60 . tight $ defaultLayout) "A"
        c <- labelEx (fillW . minW 130 . tight $ defaultLayout) "A"
        pure (a, b, c)
  (a, b, c) <- warmup2 ctx inp ui
  let Rect _ _ wa _ = respRect a
      Rect _ _ wb _ = respRect b
      Rect _ _ wc _ = respRect c
  assert failed (abs (wa - 50) <= 0.5)
  assert failed (abs (wb - 60) <= 0.5)
  assert failed (abs (wc - 130) <= 0.5)
  assert failed (abs (wa + wb + wc - 240) <= 0.5)

-- | The same min-content-floored equal split runs for the vertical axis:
-- two fillH children of a fixed-height column come out equal. The spacers
-- keep a non-zero width because prev-rect tracking skips zero-area rects.
runGrowEqualSplitHeightTest :: Context -> IORef Int -> IO ()
runGrowEqualSplitHeightTest ctx failed = do
  let inp = withInput 60 200
      ui = columnWith (fixedH 200 . tight . gap 0) $ do
        a <- spacer (Fixed 10) (Grow 1)
        b <- spacer (Fixed 10) (Grow 1)
        pure (a, b)
  (a, b) <- warmup2 ctx inp ui
  let Rect _ _ _ ha = respRect a
      Rect _ _ _ hb = respRect b
  assert failed (abs (ha - 100) <= 0.5 && abs (hb - 100) <= 0.5)

runLabelAlignEndTest :: Context -> IORef Int -> IO ()
runLabelAlignEndTest _ failed = do
  checkLabelAlignEnd failed =<< newCellContext
  checkLabelAlignEnd failed =<< newPixelContext
  checkLabelAlignEndInk failed

runAspectLayoutTest :: Context -> IORef Int -> IO ()
runAspectLayoutTest ctx failed = do
  let inp = withInput 320 240
      ui = columnWith (fixedW 160 . tight) (labelEx (fixedAspectW 160 2 . tight $ defaultLayout) "X")
  resp <- warmup2 ctx inp ui
  let Rect _ _ w h = respRect resp
  assert failed (abs (w - 160) <= 1 && abs (h - 80) <= 1)

runGrowWrapPushesSiblingTest :: Context -> IORef Int -> IO ()
runGrowWrapPushesSiblingTest _ failed = do
  ctx <- newCellContext
  let inp = withInput 6 20
      ui = column' (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutPadding = Padding 0 0 0 0, layoutGap = 0}) $ do
        grid' 1 (defaultLayout {layoutWidth = Grow 1, layoutPadding = Padding 0 0 0 0, layoutGap = 0})
          (label "AAAA" >> label "BBBB" >> pure ())
        label "BELOW"
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let ysFor t = [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == t]
  case (ysFor "BBBB", ysFor "BELOW") of
    ([by], [sy]) -> assert failed (sy >= by + 0.5)
    _ -> assert failed False

runHostSlotTest :: Context -> IORef Int -> IO ()
runHostSlotTest ctx failed = do
  let inp = withInput 80 80
      hostUiString = do
        _ <- column (pure ())
        askHost @String
      hostUiInt = do
        _ <- column (pure ())
        askHost @Int
  (miss, _, _, _) <- runFrame ctx inp hostUiString
  setHost ctx ("ok" :: String)
  setHost ctx (1 :: Int)
  (hitS, _, _, _) <- runFrame ctx inp hostUiString
  (hitI, _, _, _) <- runFrame ctx inp hostUiInt
  assert failed (miss == Nothing && hitS == Just "ok" && hitI == Just 1)

runCompactHostTest :: Context -> IORef Int -> IO ()
runCompactHostTest ctx failed = do
  _ <- compactHost ctx ([0 .. 9999] :: [Int])
  let ui = do
        _ <- column (pure ())
        askCompact @[Int]
  (got, _, _, _) <- runFrame ctx (withInput 80 80) ui
  case got of
    Just xs | length xs == 10000 && last xs == 9999 -> pure ()
    _ -> assert failed False

runEmbedStateTest :: Context -> IORef Int -> IO ()
runEmbedStateTest ctx failed = do
  let ui :: Eff '[Ui, State Int, IOE] Int
      ui = do
        _ <- column (pure ())
        modify (+ (1 :: Int))
        modify (+ (1 :: Int))
        get
  (n, _, _, _) <- runFrameEff (runEff . evalState (0 :: Int)) ctx (withInput 80 80) ui
  assertEq failed n 2

data CounterMsg = Inc | Dec
  deriving (Eq, Show)

data Counter = Counter {counterN :: Int}
  deriving (Eq, Show)

updateCounter :: CounterMsg -> Counter -> Counter
updateCounter Inc m = m {counterN = counterN m + 1}
updateCounter Dec m = m {counterN = counterN m - 1}

runReduceMessagesTest :: Context -> IORef Int -> IO ()
runReduceMessagesTest ctx failed = do
  let inp = withInput 80 80
      model0 = Counter 0
      view _ =
        column $
          emit Inc >> emit Dec >> emit Inc >> emit ("noise" :: String)
  ((), model1, msgs, _, dirty) <- runFrameReduce updateCounter ctx inp model0 view
  assert failed (msgs == [Inc, Dec, Inc] && model1 == Counter 1 && dirty)

runReduceUpdatesTest :: Context -> IORef Int -> IO ()
runReduceUpdatesTest ctx failed = do
  let ui =
        column $
          emit (updateCounter Inc) >> emit (updateCounter Dec) >> emit (updateCounter Inc)
  (_, msgs, _, _) <- runFrame ctx (withInput 80 80) ui
  let model1 = reduceUpdates (Counter 0) msgs
  assertEq failed model1 (Counter 1)

runReduceClickTest :: Context -> IORef Int -> IO ()
runReduceClickTest ctx failed = do
  let inp0 = withInput 240 120
      view m = do
        resp <- button "Go"
        onClick resp (emit Inc)
        label_ (T.pack (show (counterN m)))
        pure resp
  _ <- runFrameReduce updateCounter ctx inp0 (Counter 0) view
  (resp, model0, _, _, _) <- runFrameReduce updateCounter ctx inp0 (Counter 0) view
  assertEq failed model0 (Counter 0)
  (modelR, msgs, dirty) <- runClickReduce updateCounter ctx inp0 (Counter 0) view (centerOf resp)
  assert failed (msgs == [Inc] && modelR == Counter 1 && dirty)
  (_, model1, _, _, _) <- runFrameReduce updateCounter ctx inp0 modelR view
  assertEq failed model1 (Counter 1)

runReduceIdentityTest :: Context -> IORef Int -> IO ()
runReduceIdentityTest ctx failed = do
  let inp = withInput 80 80
      view _ = column (emit Inc >> emit Dec)
  ((), model1, msgs, _, dirty) <- runFrameReduce updateCounter ctx inp (Counter 0) view
  assert failed (msgs == [Inc, Dec] && model1 == Counter 0 && not dirty)

runWidgetNoStringEmitTest :: Context -> IORef Int -> IO ()
runWidgetNoStringEmitTest ctx failed = do
  let inp0 = withInput 240 120
  (resp, _, _, _) <- runFrame ctx inp0 (button "Go")
  let (press, release) = clickPair inp0 (centerOf resp)
  _ <- runFrame ctx press (button "Go")
  (_, msgs, _, _) <- runFrame ctx release (button "Go")
  assert failed (null (decodeMessages msgs :: [String]))

runUseFlagClickTest :: Context -> IORef Int -> IO ()
runUseFlagClickTest ctx failed = do
  let inp0 = withInput 240 120
      ui = do
        (open, setOpen) <- useFlag False
        (note, setNote) <- useText ""
        resp <- button "Go"
        onClick resp (setOpen True >> setNote "hi")
        pure (open, note, resp)
  (open0, note0, resp) <- warmup2 ctx inp0 ui
  assert failed (not open0 && note0 == "")
  let (press, release) = clickPair inp0 (centerOf resp)
  _ <- runFrame ctx press ui
  ((open1, note1, _), _, _, _) <- runFrame ctx release ui
  assert failed (open1 && note1 == "hi")

runPanelPaintsTest :: Context -> IORef Int -> IO ()
runPanelPaintsTest ctx failed = do
  let inp = withInput 200 200
      fat = padAll 16 (fillW defaultLayout)
  (_, _, colDraw, _) <- runFrame ctx inp (column' fat (label "x"))
  (_, _, panDraw, _) <- runFrame ctx inp (panel' fat (label "x"))
  assertGt failed (drawVertexCount panDraw) (drawVertexCount colDraw)

-- | Drag-drop previews must come from simulating the post-drop layout, not
-- from halving the target's pre-drop rect: in a grid mixing 'AxisV' and
-- 'AxisH' splits, dropping first removes the dragged pane, which collapses
-- its parent split and re-flows the sibling subtrees, so the naive highlight
-- lands at the wrong position and size.
runPaneGridMixedDragTest :: Context -> IORef Int -> IO ()
runPaneGridMixedDragTest ctx failed = do
  -- Model level: vertical root split with a horizontal split inside the right
  -- branch — pane 1 left, panes 2 (top right) and 3 (bottom right).
  let minSize = 40
      gutter = 4
      base = Rect 0 0 600 400
      tree0 = Split 100 AxisV 0.5 (Pane 1) (Split 101 AxisH 0.5 (Pane 2) (Pane 3))
      regions0 = fst (layoutNode minSize gutter tree0 base)
      r2 = regions0 M.! 2
      r3 = regions0 M.! 3
      regionAfter dt = do
        t' <- treeMovePane 1 999 dt tree0
        pure (M.lookup 1 (fst (layoutNode minSize gutter t' base)))
      preview dt = dropPreview minSize gutter tree0 1 base dt
      matchesSimulation dt =
        case (preview dt, regionAfter dt) of
          (Just (rPrev, _), Just (Just rPost)) -> rPrev == rPost
          _ -> False
  assertEq failed regions0 $
    M.fromList
      [ (1, Rect 0 0 300 400)
      , (2, Rect 304 0 296 200)
      , (3, Rect 304 204 296 196)
      ]
  -- Cross-axis edge drop on the bottom-right pane: removing pane 1 collapses
  -- the root split, so the right branch re-flows to the whole grid and pane 1
  -- lands in its bottom-right corner — not in a half of the target's old rect
  -- (which would be Rect 304 302 296 98).
  let dtA = dropTargetForPane r3 (V2 (rectX r3 + rectW r3 / 2) (rectY r3 + rectH r3 * 0.9)) 3
  assertEq failed dtA (DropSplit 3 AxisH False)
  assertEq failed (preview dtA) (Just (Rect 0 306 600 94, DropSplit 3 AxisH False))
  assert failed (matchesSimulation dtA)
  -- Edge drop on the top-right pane.
  let dtB = dropTargetForPane r2 (V2 (rectX r2 + rectW r2 * 0.9) (rectY r2 + rectH r2 / 2)) 2
  assertEq failed dtB (DropSplit 2 AxisV False)
  assertEq failed (preview dtB) (Just (Rect 304 0 296 200, DropSplit 2 AxisV False))
  assert failed (matchesSimulation dtB)
  -- Center drop swaps; the preview is the target's exact region.
  let dtC = dropTargetForPane r2 (V2 (rectX r2 + rectW r2 / 2) (rectY r2 + rectH r2 / 2)) 2
  assertEq failed dtC (DropSwap 2)
  assertEq failed (preview dtC) (Just (Rect 304 0 296 200, DropSwap 2))
  -- Top-level edge drops restructure the whole grid.
  assertEq failed (topLevelDropTarget 20 base (V2 5 200)) (Just (DropTop AxisV True))
  assertEq failed (topLevelDropTarget 20 base (V2 300 200)) Nothing
  let dtD = DropTop AxisV True
  assertEq failed (preview dtD) (Just (Rect 0 0 300 400, DropTop AxisV True))
  assert failed (matchesSimulation dtD)

  -- Widget level: build the same mixed grid through a live paneGrid, drag the
  -- left pane onto the bottom-right pane's lower edge, and check the drop
  -- lands it below that pane (tree order of the restructured grid).
  rects <- newIORef IM.empty
  step <- newIORef (0 :: Int)
  nbRef <- newIORef (0 :: Word64)
  let inp0 = withInput 600 400
      cfg =
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgMinSize = 40
          , pgSpacing = 4
          , pgViewPane = \pid pctx -> do
              liftIO (modifyIORef' rects (IM.insert (fromIntegral pid) (pgcRect pctx)))
              s <- liftIO (readIORef step)
              case s of
                0 -> do
                  nb <- pgcSplit pctx AxisV
                  liftIO $ do
                    writeIORef nbRef nb
                    writeIORef step 1
                1 -> do
                  nb <- liftIO (readIORef nbRef)
                  when (pid == nb) $ do
                    _ <- pgcSplit pctx AxisH
                    liftIO (writeIORef step 2)
                _ -> pure ()
              pure (PaneView "P" True Nothing)
          }
      ui = paneGrid cfg
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  (pgr0, _, _, _) <- runFrame ctx inp0 ui
  case pgrPanes pgr0 of
    [pa, pb, pc] -> do
      rs <- readIORef rects
      case (IM.lookup (fromIntegral pa) rs, IM.lookup (fromIntegral pc) rs) of
        (Just ra, Just rc) -> do
          let grab = V2 (rectX ra + rectW ra / 2) (rectY ra + rectH ra / 2)
              -- 30px above the grid's bottom edge: inside the pane's bottom
              -- drop zone but clear of the 20px top-level band.
              dest = V2 (rectX rc + rectW rc / 2) (rectY rc + rectH rc - 30)
              press =
                inp0
                  { inputMousePos = grab
                  , inputMouseDown = True
                  , inputMousePressed = True
                  , inputMouseReleased = False
                  }
              hold = press {inputMousePos = dest, inputMousePressed = False}
              release = hold {inputMouseDown = False, inputMouseReleased = True}
          _ <- runFrame ctx press ui
          _ <- runFrame ctx hold ui
          (pgr1, _, _, _) <- runFrame ctx release ui
          assertEq failed (pgrPanes pgr1) [pb, pc, pa]
        _ -> assert failed False
    _ -> assert failed False


runBase16ThemeTest :: Context -> IORef Int -> IO ()
runBase16ThemeTest ctx failed = do
  let dark = themeFromBase16 base16TomorrowNight
      light = themeFromBase16 base16TomorrowLight
  -- Window background matches base00
  assertEq failed (themeWindow dark) (base00 base16TomorrowNight)
  assertEq failed (themeWindow light) (base00 base16TomorrowLight)
  -- Accent matches base0D
  assertEq failed (themeAccent dark) (base0D base16TomorrowNight)
  assertEq failed (themeAccent light) (base0D base16TomorrowLight)
  -- Series colors match base08-base0E
  assertEq failed (themeRed dark) (base08 base16TomorrowNight)
  assertEq failed (themeOrange dark) (base09 base16TomorrowNight)
  assertEq failed (themeYellow dark) (base0A base16TomorrowNight)
  assertEq failed (themeGreen dark) (base0B base16TomorrowNight)
  assertEq failed (themePurple dark) (base0E base16TomorrowNight)
  -- Lowercase aliases match uppercase record fields
  assertEq failed (base0a base16TomorrowNight) (base0A base16TomorrowNight)
  assertEq failed (base0b base16TomorrowNight) (base0B base16TomorrowNight)
  assertEq failed (base0c base16TomorrowNight) (base0C base16TomorrowNight)
  assertEq failed (base0d base16TomorrowNight) (base0D base16TomorrowNight)
  assertEq failed (base0e base16TomorrowNight) (base0E base16TomorrowNight)
  assertEq failed (base0f base16TomorrowNight) (base0F base16TomorrowNight)
  -- Function aliases produce identical themes
  assertEq failed dark (base16Theme base16TomorrowNight)
  assertEq failed dark (base16ToTheme base16TomorrowNight)
  -- Dark theme styling checks
  assertEq failed (styleBg (themeInput dark)) (base00 base16TomorrowNight)
  assertEq failed (styleBg (themeButton dark)) (base02 base16TomorrowNight)
  assertEq failed (styleFg (themeButton dark)) (base07 base16TomorrowNight)
  assertEq failed (themeOverlayDim dark) (colorRGBA 0 0 0 160)
  -- Light theme styling checks
  assertEq failed (styleBg (themeInput light)) (base00 base16TomorrowLight)
  assertEq failed (styleBg (themeButton light)) (base01 base16TomorrowLight)
  assertEq failed (styleFg (themeButton light)) (base05 base16TomorrowLight)
  assertEq failed (themeOverlayDim light) (colorRGBA 0 0 0 100)
  -- Integration: Context theme update and UI frame renders with base16 theme
  ctx' <- withTheme ctx dark
  th <- getTheme ctx'
  assertEq failed th dark
  let inp = withInput 200 100
  (_, _, draw, _) <- runFrame ctx' inp (button "Base16 Button")
  assertGt failed (drawVertexCount draw) 0

-- Clicking the embedded clear (×) must empty the field, keep focus, and fire an
-- immediate (non-debounced) change pulse.
runSearchFieldClearTest :: Context -> IORef Int -> IO ()
runSearchFieldClearTest ctx failed = do
  let inp0 = withInput 320 100
      ui = column (searchField "Search…" "hello world")
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect bx by bw bh = respRect resp
      cy = by + bh / 2
      scanClear x
        | x < bx = pure Nothing
        | otherwise = do
            let probe = inp0 {inputMousePos = V2 x cy}
            _ <- runFrame ctx probe ui
            kind <- uiCursorKind ctx probe
            if kind == UiCursorPointer then pure (Just x) else scanClear (x - 2)
  mcx <- scanClear (bx + bw - 6)
  case mcx of
    Nothing -> assert failed False
    Just cx -> do
      let press = inp0 {inputMousePos = V2 cx cy, inputMouseDown = True, inputMousePressed = True, inputMouseReleased = False}
      _ <- runFrame ctx press ui
      ((r1, t1), _, _, _) <- runFrame ctx inp0 ui
      assertEq failed t1 ""
      assert failed (respChanged r1)
      ((r2, _), _, _, _) <- runFrame ctx inp0 ui
      assert failed (not (respChanged r2))

-- Typing is echoed immediately but the change pulse only fires after the text
-- has been idle for the configured debounce window.
runSearchFieldDebounceTest :: Context -> IORef Int -> IO ()
runSearchFieldDebounceTest ctx failed = do
  let inp0 = withInput 320 100
      ui = column (searchFieldConfigured (defaultSearchFieldConfig {sfcDebounceMs = 40}) "")
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  ((rA, tA), _, _, _) <- runFrame ctx (inp0 {inputChars = "a"}) ui
  assertEq failed tA "a"
  assert failed (not (respChanged rA))
  ((rB, tB), _, _, _) <- runFrame ctx (inp0 {inputChars = "b"}) ui
  assertEq failed tB "ab"
  assert failed (not (respChanged rB))
  threadDelay 80000
  ((rC, tC), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed tC "ab"
  assert failed (respChanged rC)
  threadDelay 50000
  ((rD, _), _, _, _) <- runFrame ctx inp0 ui
  assert failed (not (respChanged rD))



