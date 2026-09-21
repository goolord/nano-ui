module Cases
  ( runAspectLayoutTest
  , runCheckboxInitialTest
  , runDrawingTest
  , runEmbedStateTest
  , runEmptyFrameTest
  , runFitMutedWidthTest
  , runGrowSplitTest
  , runHostSlotTest
  , runHoverDamageTest
  , runIdKeyedListTest
  , runKvMultilineHeightTest
  , runImageTest
  , runImageSwapDamageTest
  , runLabelAlignEndTest
  , runLayoutReuseTest
  , runDeepNestingTest
  , runPanelPaintsTest
  , runPaneGridMixedDragTest
  , runPaneGridClippedControlTest
  , runPaneGridDropPreviewTest
  , runPaneGridPinnedPaneTest
  , runPercentGapShrinkTest
  , runPointerCursorTest
  , runReduceClickTest
  , runReduceMessagesTest
  , runResponsiveWrapTest
  , runSliderFillWidthTest
  , runWidgetNoStringEmitTest
  , runSearchInputClearTest
  , runSearchInputDebounceTest
  , runSearchInputSetTextDebounceTest
  ) where

import Control.Monad (forM_, void, when)
import Control.Concurrent (threadDelay)
import GHC.Clock (getMonotonicTime)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Word (Word64)
import Effectful (liftIO)
import Effectful.State.Static.Local (State, evalState, get, modify)
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Context (Context (..))
import NanoUI.Emit qualified as Emit
import NanoUI.Internal.Layout.Arena
  ( NodeType (..)
  , arenaArrays
  , foldNodesM
  , getNodeType
  , getNodeValue
  , tagNodeType
  , treeFirstChild
  , treeNextSibling
  , writeTagEnum
  , writeTree
  )
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, assertJust, assertJustM, runClickReduce, withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , checkLabelAlignEndInk
  , clickPair
  , held
  , holdAt
  , pressAt
  , releaseAt
  , spanCenter
  , spanRect
  , spanXOf
  , spanYOf
  , vertUv
  , warmup2
  , warmupDraw
  , warmupFocused
  , withInputOff
  )
import NanoUI.Internal.Widgets.SplitPane
  ( PaneDrop (..)
  , dropPreview
  , DropPreview (..)
  , dropPreviewTreeSized
  , dropTargetForPane
  , layoutNode
  , reflowFixed
  , topLevelDropTarget
  )

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
  case idsA of
    [a, b, c] -> assert failed (a /= b && b /= c && a /= c)
    _ -> assert failed False
  assertEq failed (idFor "a" ["a", "b", "c"] idsA) (idFor "a" ["x", "a", "b", "c"] idsPrep)
  assertEq failed (idFor "b" ["a", "b", "c"] idsA) (idFor "b" ["a", "b", "c", "y"] idsApp)
  assertEq failed (idFor "c" ["a", "b", "c"] idsA) (idFor "c" ["c", "b", "a"] idsRev)

runFitMutedWidthTest :: Context -> IORef Int -> IO ()
runFitMutedWidthTest ctx failed = do
  let inp = withInput 400 100
      ui = columnWith tight (muted "HelloFitMuted")
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  assertJust failed (rectW <$> spanRect "HelloFitMuted" spans) $ \w -> assertGt failed w 8

-- | Text and resize changes must invalidate cached layout.
runLayoutReuseTest :: Context -> IORef Int -> IO ()
runLayoutReuseTest ctx failed = do
  let inp = withInput 400 300
      ui1 =
        columnWith (tight . gap 4 . fillW) $ do
          void (label "alpha")
          void (button "beta")
          void (label "gamma delta epsilon")
  _ <- runFrame ctx inp ui1
  -- Frame 2 takes the cached-layout path (same descriptor).
  _ <- runFrame ctx inp ui1
  s1 <- collectTextSpans ctx
  -- Text change invalidates the cached descriptor.
  let ui2 =
        columnWith (tight . gap 4 . fillW) $ do
          void (label "alpha changed")
          void (button "beta")
          void (label "gamma delta epsilon")
  _ <- runFrame ctx inp ui2
  s2 <- collectTextSpans ctx
  assert failed (s2 /= s1)
  -- Width change must not serve the cached placement for width-dependent
  -- (wrapping) layout.
  let wrapUi =
        columnWith (tight . fillW) $
          label "the quick brown fox jumps over the lazy dog repeatedly"
  _ <- runFrame ctx (withInput 600 200) wrapUi
  sw0 <- collectTextSpans ctx
  _ <- runFrame ctx (withInput 180 200) wrapUi
  sw1 <- collectTextSpans ctx
  assert failed (sw1 /= sw0)

-- | Nesting deeper than the initial snapshot-level capacity must still lay
-- out correctly (the level array grows on demand).
runDeepNestingTest :: Context -> IORef Int -> IO ()
runDeepNestingTest ctx failed = do
  let nest :: Int -> NanoUI ()
      nest 0 = void (label "deep")
      nest k = column (nest (k - 1))
  _ <- runFrame ctx (withInput 300 300) (nest 320)
  spans <- collectTextSpans ctx
  assert failed (any (\(_, t, _, _, _) -> t == "deep") spans)

-- | A responsive row stacks its children below the breakpoint and keeps them
-- side by side above it.
runResponsiveWrapTest :: Context -> IORef Int -> IO ()
runResponsiveWrapTest ctx failed = do
  let ui = responsiveRowCol 720 (tight . gap 8 . fillW) $ do
        columnWith (tight . gap 8 . fillW) (card (void (label "LeftTop")) >> card (void (label "LeftBot")))
        card (void (label "Right"))
  _ <- warmup2 ctx (withInput 520 800) ui
  narrow <- collectTextSpans ctx
  case (spanYOf "Right" narrow, spanYOf "LeftBot" narrow) of
    ([ry], [ly]) -> assertGt failed ry (ly + 1)
    _ -> assert failed False
  _ <- warmup2 ctx (withInput 1200 800) ui
  wide <- collectTextSpans ctx
  case (spanXOf "Right" wide, spanXOf "LeftTop" wide) of
    ([rx], [lx]) -> assertGt failed rx (lx + 1)
    _ -> assert failed False

runDrawingTest :: Context -> IORef Int -> IO ()
runDrawingTest ctx failed = do
  let ui =
        drawing (fixedWH 80 40) $ \r ->
          pure
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

runPointerCursorTest :: Context -> IORef Int -> IO ()
runPointerCursorTest ctx failed = do
  let inp0 = withInput 200 100
      ui = column $ do
        btn <- button' "Click"
        (cb, _) <- checkbox' "Feature" False
        pure (btn, cb)
      wantAt inp = runFrame ctx inp ui >> pointerCursorWanted ctx inp
  (btn, cb) <- warmup2 ctx inp0 ui
  onButton <- wantAt (inp0 {inputMousePos = centerOf btn})
  assert failed onButton
  offWidgets <- wantAt (inp0 {inputMousePos = V2 (-1) (-1)})
  assert failed (not offWidgets)
  let hoverBox = inp0 {inputMousePos = centerOf cb}
  onBox <- wantAt hoverBox
  assert failed onBox
  pressBox <- wantAt (hoverBox {inputMouseDown = True, inputMousePressed = True, inputMouseReleased = False})
  assert failed pressBox

-- A frame whose UI adds no widgets is an empty frame, not a read of a node
-- that was never added: presses, wheel input, redraw and cursor queries all
-- run on the empty arena. Node 0 starts out as a container that is its own
-- child, so any read of it recurses without end. Widgets added on the next
-- frame still lay out.
runEmptyFrameTest :: Context -> IORef Int -> IO ()
runEmptyFrameTest ctx failed = do
  arrays <- arenaArrays (ctxNodeArena ctx)
  writeTagEnum arrays 0 tagNodeType NodeContainer
  writeTree arrays 0 treeFirstChild 0
  writeTree arrays 0 treeNextSibling (-1)
  let inp0 = (withInput 320 200) {inputMousePos = V2 40 40}
      press = inp0 {inputMouseDown = True, inputMousePressed = True, inputScroll = V2 0 1}
      ui = row $ do
        wid <- currentId
        image (fixedWH 40 24) (ImageId 0)
        pure wid
  _ <- runFrame ctx inp0 (pure ())
  _ <- runFrame ctx press (pure ())
  _ <- needsRedraw ctx inp0 (inp0 {inputMousePos = V2 60 60})
  _ <- uiCursorKind ctx inp0
  wid <- warmup2 ctx inp0 ui
  mRect <- getPrevRect ctx wid
  assert failed (maybe False (\(Rect _ _ w h) -> abs (w - 40) <= 0.5 && abs (h - 24) <= 0.5) mRect)

-- Switching an image to another id repaints the image, and only the image:
-- frames swapped inside a page must neither leave a stale frame on screen nor
-- repaint the whole page.
runImageSwapDamageTest :: Context -> IORef Int -> IO ()
runImageSwapDamageTest ctx failed = do
  let px a = BS.pack (concat (replicate 16 [a, 0, 0, 255]))
  ok1 <- registerImage ctx (ImageId 1) 4 4 (px 60)
  ok2 <- registerImage ctx (ImageId 2) 4 4 (px 120)
  assert failed (ok1 && ok2)
  frameRef <- newIORef (ImageId 1)
  let inp0 = withInputOff 320 200
      ui = fmap snd $
        scrollArea (padAll 10 . grow) $
          column $ do
            label "frames"
            wid <- currentId
            image (fixedWH 40 24) =<< uiIO (readIORef frameRef)
            pure wid
  wid <- warmup2 ctx inp0 ui
  _ <- takeDamage ctx
  writeIORef frameRef (ImageId 2)
  _ <- runFrame ctx inp0 ui
  dmg <- takeDamage ctx
  mRect <- getPrevRect ctx wid
  case (dmg, mRect) of
    (DamageClip (Rect dx dy dw dh), Just (Rect ix iy iw ih)) -> do
      assert failed (dx <= ix && dy <= iy && dx + dw >= ix + iw && dy + dh >= iy + ih)
      assert failed (dw * dh < 320 * 200 / 4)
    _ -> assert failed False

runImageTest :: Context -> IORef Int -> IO ()
runImageTest ctx failed = do
  let px a b c = BS.pack (concat (replicate 16 [a, b, c, 255]))
  ok1 <- registerImage ctx (ImageId 1) 4 4 (px 255 0 0)
  ok7 <- registerImage ctx (ImageId 7) 4 4 (px 0 0 255)
  assert failed (ok1 && ok7)
  let inp0 = withInput 320 200
      imgLayout = fixedWH 40 24
      ui = row $ do
        image imgLayout (ImageId 1)
        wid <- currentId
        image imgLayout (ImageId 7)
        pure wid
  (wid, drawData) <- warmupDraw ctx inp0 ui
  assertJustM failed (getPrevRect ctx wid) $ \(Rect _ _ w h) -> assert failed (abs (w - 40) <= 0.5 && abs (h - 24) <= 0.5)
  let texCmds = filter (\c -> cmdTextureId c == atlasTextureId) (drawCmdElems drawData)
  assertEq failed (length texCmds) 1
  assert failed (any (\c -> cmdIndexCount c == 12) texCmds)
  (u0, _) <- vertUv drawData 0
  (u4, _) <- vertUv drawData 4
  assert failed (abs (u0 - u4) >= 1e-6)
  -- Fresh ids start above the registered ones and never repeat.
  ((fresh1, fresh2), _, _, _) <- runFrame ctx inp0 ((,) <$> freshImageId <*> freshImageId)
  assertEq failed (map unImageId [fresh1, fresh2]) [8, 9]
  let missing = image imgLayout (ImageId 0)
  _ <- runFrame ctx inp0 missing
  (_, _, missingData, _) <- runFrame ctx inp0 missing
  assert failed (not (any (\c -> cmdTextureId c == atlasTextureId) (drawCmdElems missingData)))

-- | Hover enter and press repaint only the button, and pointer motion inside
-- an already-hovered button does not request a redraw.
runHoverDamageTest :: Context -> IORef Int -> IO ()
runHoverDamageTest ctx failed = do
  let ui = column (button' "OK")
      inp0 = withInputOff 240 80
      assertSmall dmg = case dmg of
        DamageFull -> assert failed False
        DamageClip (Rect _ _ w h) -> assert failed (w * h < 240 * 80 * 0.5)
  _ <- runFrame ctx inp0 ui
  d0 <- takeDamage ctx
  assertEq failed d0 DamageFull
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let V2 cx cy = centerOf resp
      inp1 = inp0 {inputMousePos = V2 cx cy}
      inp2 = inp0 {inputMousePos = V2 (cx + 1) cy}
  needEnter <- needsRedraw ctx inp0 inp1
  assert failed needEnter
  _ <- runFrame ctx inp1 ui
  assertSmall =<< takeDamage ctx
  let drain = inp1 {inputDeltaTime = 1}
  _ <- runFrame ctx drain ui
  needStay <- needsRedraw ctx drain inp2
  assert failed (not needStay)
  let inpClick = inp1 {inputMouseDown = True, inputMousePressed = True}
  needClick <- needsRedraw ctx drain inpClick
  assert failed needClick
  _ <- runFrame ctx inpClick ui
  assertSmall =<< takeDamage ctx


-- | An unclicked checkbox must keep rendering its initial value; the frame's
-- post-UI value sync must not reset it to unchecked when no state is stored.
runCheckboxInitialTest :: Context -> IORef Int -> IO ()
runCheckboxInitialTest ctx failed = do
  checkedRef <- newIORef True
  let inp0 = withInput 200 100
      ui = column (held checkedRef (checkbox' "Opt"))
  (resp, _) <- warmup2 ctx inp0 ui
  assertCheckboxNodeValue failed ctx 1
  let Rect rx ry _ _ = respRect resp
      (press, release) = clickPair inp0 (V2 (rx + 1) (ry + 0.5))
  _ <- runFrame ctx press ui
  ((_, checked), _, _, _) <- runFrame ctx release ui
  assert failed (not checked)
  assertCheckboxNodeValue failed ctx 0
  -- The toggled value persists on an idle frame.
  ((_, idle), _, _, _) <- runFrame ctx inp0 ui
  assert failed (not idle)
  _ <- runFrame ctx press ui
  ((_, checked2), _, _, _) <- runFrame ctx release ui
  assert failed checked2
  assertCheckboxNodeValue failed ctx 1

assertCheckboxNodeValue :: IORef Int -> Context -> Float -> IO ()
assertCheckboxNodeValue failed ctx expected = do
  let na = ctxNodeArena ctx
  vals <- foldNodesM na (\acc i -> do
    nt <- getNodeType na i
    if nt == NodeCheckbox then (: acc) <$> getNodeValue na i else pure acc) []
  case vals of
    [v] -> assertEq failed v expected
    vs -> assert failed (vs == [expected])

runSliderFillWidthTest :: Context -> IORef Int -> IO ()
runSliderFillWidthTest ctx failed = do
  let inp0 = withInput 400 120
      ui = columnWith fillW (slider' 0 100 0)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
  assertGt failed rw 300
  let track = sliderTrackBounds rx ry rw rh
      endDrag = V2 (rectX track + rectW track - 2) (rectY track + rectH track / 2)
  ((_, val), _, _, _) <- runFrame ctx (pressAt inp0 endDrag) ui
  assertGt failed val 90

-- | Percent children size against the row width, and flex like CSS: two 50%
-- columns plus a gap must give back the overflow so the pair lands exactly on
-- the row width (equal halves, no spill past the row's right edge).
runPercentGapShrinkTest :: Context -> IORef Int -> IO ()
runPercentGapShrinkTest ctx failed = do
  let quarters = rowWith (fixedW 200 . tight . gap 0) $ do
        a <- labelWith' (percent 25 . tight) "A"
        b <- labelWith' (percent 75 . tight) "B"
        pure (a, b)
  (qa, qb) <- warmup2 ctx (withInput 200 80) quarters
  assert failed (abs (rectW (respRect qa) - 50) <= 1 && abs (rectW (respRect qb) - 150) <= 1)
  let inp = withInput 300 80
      ui = rowWith (fixedW 206 . tight . gap 6) $ do
        a <- labelWith' (percent 50 . tight) "A"
        b <- labelWith' (percent 50 . tight) "B"
        pure (a, b)
  (a, b) <- warmup2 ctx inp ui
  let Rect xa _ wa _ = respRect a
      Rect xb _ wb _ = respRect b
  assert failed (abs (wa - 100) <= 0.5 && abs (wb - 100) <= 0.5)
  assert failed (abs (xb - (xa + wa + 6)) <= 0.5)

-- | Grow children split the free space by factor with a min-content floor
-- (fixed-width rows, 12px per char in this context):
--
-- * equal split: two fillW labels with unequal text come out equal when both
--   fit their share;
-- * content floor: a child whose content needs more than its share takes
--   exactly its content width and the sibling re-shares what is left;
-- * lock cascade: locking the largest child shrinks the share pool, which
--   must lock the middle child on a later sweep (one sweep would give it 55);
-- * the vertical axis splits the same way. Its spacers keep a non-zero width
--   because prev-rect tracking skips zero-area rects.
runGrowSplitTest :: Context -> IORef Int -> IO ()
runGrowSplitTest ctx failed = do
  let growLabel l txt = respId <$> labelWith' (fillW . l . tight) txt
      growSpacer = currentId <* spacer (Fixed 10) (Grow 1)
      cases :: [(Input, Rect -> Float, NanoUI [WidgetId], [Float])]
      cases =
        [ ( withInput 210 40
          , rectW
          , rowWith (fixedW 210 . tight . gap 0) $
              sequence [growLabel id "A", growLabel id "AAAAA"]
          , [105, 105]
          )
        , ( withInput 200 40
          , rectW
          , rowWith (fixedW 200 . tight . gap 0) $
              sequence [growLabel id "A", growLabel id (T.replicate 15 "A")]
          , [20, 180]
          )
        , ( withInput 240 40
          , rectW
          , rowWith (fixedW 240 . tight . gap 0) $
              sequence [growLabel (minW 12) "A", growLabel (minW 60) "A", growLabel (minW 130) "A"]
          , [50, 60, 130]
          )
        , ( withInput 60 200
          , rectH
          , columnWith (fixedH 200 . tight . gap 0) $
              sequence [growSpacer, growSpacer]
          , [100, 100]
          )
        ]
  forM_ cases $ \(inp, size, ui, want) -> do
    ids <- warmup2 ctx inp ui
    got <- mapM (fmap (maybe 0 size) . getPrevRect ctx) ids
    assertEq failed (length got) (length want)
    forM_ (zip got want) $ \(g, w) -> assert failed (abs (g - w) <= 0.5)

runLabelAlignEndTest :: Context -> IORef Int -> IO ()
runLabelAlignEndTest ctx failed = do
  let
    fm = ctxFontMetrics ctx
    tw = fmAdvance fm ' ' * 2
    boxW = tw + 4
    inp = emptyInput {inputWindowSize = Size (boxW + 8) 8}
    ui =
      rowWith (fixedW boxW . tight . gap 0) $
        labelWith' (fillW . alignEnd . tight) "ab"
  lab <- warmup2 ctx inp ui
  spans <- collectTextSpans ctx
  let
    Rect bx _ bw _ = respRect lab
    hits = [r | (r, txt, _, _, _) <- spans, T.isInfixOf (T.pack "ab") txt]
  case hits of
    [] -> assert failed False
    Rect x _ w _ : _ -> do
      assert failed (abs ((x + w) - (bx + bw)) <= 0.6)
      assert failed (abs (w - tw) <= 0.6)
  checkLabelAlignEndInk failed

runAspectLayoutTest :: Context -> IORef Int -> IO ()
runAspectLayoutTest ctx failed = do
  let inp = withInput 320 240
      ui = columnWith (fixedW 160 . tight) (labelWith' (fixedAspectW 160 2 . tight) "X")
  resp <- warmup2 ctx inp ui
  let Rect _ _ w h = respRect resp
  assert failed (abs (w - 160) <= 1 && abs (h - 80) <= 1)

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
  _ <- compactHost ctx ([0 .. 9999] :: [Int])
  let compactUi = do
        _ <- column (pure ())
        askCompact @[Int]
  (got, _, _, _) <- runFrame ctx inp compactUi
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
  let
    inp = withInput 80 80
    model0 = Counter 0
    view _ =
      column $
        Emit.emit Inc >> Emit.emit Dec >> Emit.emit Inc >> Emit.emit ("noise" :: String)
  ((), model1, msgs, _, dirty) <- runFrameReduce updateCounter ctx inp model0 view
  assert failed (msgs == [Inc, Dec, Inc] && model1 == Counter 1 && dirty)
  -- Messages that cancel out leave the model unchanged and not dirty.
  let
    identity _ = column (Emit.emit Inc >> Emit.emit Dec)
  ((), model2, msgs2, _, dirty2) <-
    runFrameReduce updateCounter ctx inp model0 identity
  assert failed (msgs2 == [Inc, Dec] && model2 == Counter 0 && not dirty2)
  -- Generic adapters run the control once and distinguish value changes from
  -- edit pulses. A response-only pulse cannot emit the unchanged value.
  calls <- newIORef (0 :: Int)
  let
    control value = uiIO (modifyIORef' calls (+ 1)) >> pure (value + 1)
    adapters = do
      Emit.emitWhen (pure False) (1 :: Int)
      Emit.emitWhen (pure True) (2 :: Int)
      Emit.emitChanged pure (3 :: Int) id
      Emit.emitChanged control (3 :: Int) id
      Emit.emitEdited (\v -> pure (mempty, v + 1)) (5 :: Int) id
      Emit.emitEdited (\v -> pure (mempty {rawRespChanged = True}, v)) (6 :: Int) id
      Emit.emitEdited
        (\v -> pure (mempty {rawRespChanged = True}, v + 1))
        (7 :: Int)
        id
  (_, emitted, _, _) <- runFrame ctx inp adapters
  assertEq failed [2, 4, 8] (decodeMessages emitted :: [Int])
  assertEq failed 1 =<< readIORef calls

runReduceClickTest :: Context -> IORef Int -> IO ()
runReduceClickTest ctx failed = do
  let inp0 = withInput 240 120
      view m = do
        resp <- button' "Go"
        when (respClicked resp) (Emit.emit Inc)
        label (T.pack (show (counterN m)))
        pure resp
  _ <- runFrameReduce updateCounter ctx inp0 (Counter 0) view
  (resp, model0, _, _, _) <- runFrameReduce updateCounter ctx inp0 (Counter 0) view
  assertEq failed model0 (Counter 0)
  (modelR, msgs, dirty) <- runClickReduce updateCounter ctx inp0 (Counter 0) view (centerOf resp)
  assert failed (msgs == [Inc] && modelR == Counter 1 && dirty)
  (_, model1, _, _, _) <- runFrameReduce updateCounter ctx inp0 modelR view
  assertEq failed model1 (Counter 1)

runWidgetNoStringEmitTest :: Context -> IORef Int -> IO ()
runWidgetNoStringEmitTest ctx failed = do
  let inp0 = withInput 240 120
  (resp, _, _, _) <- runFrame ctx inp0 (button' "Go")
  let (press, release) = clickPair inp0 (centerOf resp)
  _ <- runFrame ctx press (button "Go")
  (clicked, msgs, _, _) <- runFrame ctx release (button "Go")
  assert failed clicked
  assert failed (null msgs)

runPanelPaintsTest :: Context -> IORef Int -> IO ()
runPanelPaintsTest ctx failed = do
  let inp = withInput 200 200
      fat = padAll 16 . fillW
  (_, _, colDraw, _) <- runFrame ctx inp (columnWith fat (label "x"))
  (_, _, panDraw, _) <- runFrame ctx inp (panelWith fat (label "x"))
  assertGt failed (drawVertexCount panDraw) (drawVertexCount colDraw)

-- | Where 'seedMixedGrid' has got to.
data MixedSeed = SeedStart | SeedRight Word64 | SeedDone

-- | Called from every pane's 'pgViewPane', grows a fresh grid into the mixed
-- three-pane layout over its first frames: the first pane splits vertically,
-- then the new pane splits horizontally (one pane left, two stacked right).
seedMixedGrid :: IOE :> es => IORef MixedSeed -> Word64 -> PaneGridCtx es -> Eff es ()
seedMixedGrid ref pid pctx = do
  s <- liftIO (readIORef ref)
  case s of
    SeedStart -> do
      nb <- pgcSplit pctx AxisV
      liftIO (writeIORef ref (SeedRight nb))
    SeedRight nb | pid == nb -> do
      _ <- pgcSplit pctx AxisH
      liftIO (writeIORef ref SeedDone)
    _ -> pure ()

-- | Drag-drop previews must come from simulating the post-drop layout, not
-- from halving the target's pre-drop rect: in a grid mixing 'AxisV' and
-- 'AxisH' splits, dropping first removes the dragged pane, which collapses
-- its parent split and re-flows the sibling subtrees, so the naive highlight
-- lands at the wrong position and size.
-- | A filling grid with 40px minimum panes and 4px dividers.
testGridConfig :: PaneGridConfig es
testGridConfig = defaultPaneGridConfig {pgLayout = fillW . fillH, pgMinSize = 40, pgSpacing = 4}

runPaneGridMixedDragTest :: Context -> IORef Int -> IO ()
runPaneGridMixedDragTest ctx failed = do
  -- Model level: vertical root split with a horizontal split inside the right
  -- branch: pane 1 left, panes 2 (top right) and 3 (bottom right).
  let minSize = 40
      gutter = 4
      base = Rect 0 0 600 400
      tree0 = Split 100 AxisV 0.5 (Pane 1) (Split 101 AxisH 0.5 (Pane 2) (Pane 3))
      regions0 = fst (layoutNode minSize gutter tree0 base)
      r2 = regions0 M.! 2
      r3 = regions0 M.! 3
      preview dt = dropPreview minSize gutter tree0 1 base dt
  assertEq failed regions0 $
    M.fromList
      [ (1, Rect 0 0 298 400)
      , (2, Rect 302 0 298 198)
      , (3, Rect 302 202 298 198)
      ]
  -- Cross-axis edge drop on the bottom-right pane: removing pane 1 collapses
  -- the root split, so the right branch re-flows to the whole grid and pane 1
  -- lands in its bottom-right corner, not in a half of the target's old rect
  -- (which would be Rect 302 301 298 99).
  let dtA = dropTargetForPane r3 (V2 (rectX r3 + rectW r3 / 2) (rectY r3 + rectH r3 * 0.9)) 3
  assertEq failed dtA (DropSplit 3 AxisH False)
  assertEq failed (preview dtA) (Just (Rect 0 303 600 97, DropSplit 3 AxisH False))
  -- Edge drop on the top-right pane.
  let dtB = dropTargetForPane r2 (V2 (rectX r2 + rectW r2 * 0.9) (rectY r2 + rectH r2 / 2)) 2
  assertEq failed dtB (DropSplit 2 AxisV False)
  assertEq failed (preview dtB) (Just (Rect 302 0 298 198, DropSplit 2 AxisV False))
  -- Center drop swaps; the preview is the target's exact region.
  let dtC = dropTargetForPane r2 (spanCenter r2) 2
  assertEq failed dtC (DropSwap 2)
  assertEq failed (preview dtC) (Just (Rect 302 0 298 198, DropSwap 2))
  -- Top-level edge drops restructure the whole grid.
  assertEq failed (topLevelDropTarget 20 base (V2 5 200)) (Just (DropTop AxisV True))
  assertEq failed (topLevelDropTarget 20 base (V2 300 200)) Nothing
  let dtD = DropTop AxisV True
  assertEq failed (preview dtD) (Just (Rect 0 0 298 400, DropTop AxisV True))

  -- Size-preserving drops use the original pane's extent, including after
  -- removing it collapses a parent. Both sides and axes share the same rule.
  let sizedTree = Split 100 AxisV 0.25 (Pane 1) (Split 101 AxisH 0.3 (Pane 2) (Pane 3))
      source = fst (layoutNode minSize gutter sizedTree base) M.! 1
      sized dt = dropPreviewTreeSized (Just source) minSize gutter sizedTree 1 102 base dt
  forM_ [True, False] $ \onA -> do
    forM_ [DropTop AxisV onA, DropSplit 3 AxisV onA] $ \dt ->
      case sized dt of
        Nothing -> assert failed False
        Just dp -> do
          assert failed (abs (rectW (dpRect dp) - rectW source) < 0.01)
          assertEq failed (M.lookup 1 (fst (layoutNode minSize gutter (dpTree dp) base))) (Just (dpRect dp))
    -- A thin left pane transfers its width to height on top/bottom drops.
    forM_ [DropTop AxisH onA, DropSplit 3 AxisH onA] $ \dt ->
      case sized dt of
        Nothing -> assert failed False
        Just dp -> assert failed (abs (rectH (dpRect dp) - rectW source) < 0.01)
    let source2 = fst (layoutNode minSize gutter sizedTree base) M.! 2
    case dropPreviewTreeSized (Just source2) minSize gutter sizedTree 2 102 base (DropTop AxisH onA) of
      Nothing -> assert failed False
      Just dp -> assert failed (abs (rectH (dpRect dp) - rectH source2) < 0.01)
    -- Reverse orientation: a short top pane transfers height to width.
    forM_ [DropTop AxisV onA, DropSplit 3 AxisV onA] $ \dt ->
      case dropPreviewTreeSized (Just source2) minSize gutter sizedTree 2 102 base dt of
        Nothing -> assert failed False
        Just dp -> assert failed (abs (rectW (dpRect dp) - rectH source2) < 0.01)
    -- Transferred sizes still respect the destination subtree's minimum.
    case dropPreviewTreeSized (Just (Rect 0 0 500 400)) minSize gutter sizedTree 1 102 base (DropTop AxisH onA) of
      Nothing -> assert failed False
      Just dp -> assert failed (abs (rectH (dpRect dp) - 312) < 0.01)
  assertEq failed (fmap dpRect (sized (DropSwap 2))) (M.lookup 2 (fst (layoutNode minSize gutter sizedTree base)))

  -- Widget level: build the same mixed grid through a live paneGrid, drag the
  -- left pane onto the bottom-right pane's lower edge, and check the drop
  -- lands it below that pane (tree order of the restructured grid).
  rects <- newIORef IM.empty
  closeRects <- newIORef IM.empty
  stateUpdates <- newIORef IM.empty
  paneStates <- newIORef IM.empty
  seed <- newIORef SeedStart
  let inp0 = withInput 600 400
      cfg =
        testGridConfig
          { pgViewPane = \pid pctx -> do
              liftIO (modifyIORef' rects (IM.insert (fromIntegral pid) (pgcRect pctx)))
              (value, setValue) <- useInt 0
              marker <- nextId
              updates <- liftIO (readIORef stateUpdates)
              case IM.lookup (fromIntegral pid) updates of
                Nothing -> pure ()
                Just n -> do
                  setValue n
                  liftIO (modifyIORef' stateUpdates (IM.delete (fromIntegral pid)))
              liftIO (modifyIORef' paneStates (IM.insert (fromIntegral pid) (marker, value)))
              seedMixedGrid seed pid pctx
              close <- button' "x"
              liftIO (modifyIORef' closeRects (IM.insert (fromIntegral pid) (respRect close)))
              when (respClicked close) (pgcClose pctx)
              pure (PaneView "P" True Nothing)
          }
      ui = paneGrid cfg
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  pgr0 <- warmup2 ctx inp0 ui
  case pgrPanes pgr0 of
    [pa, pb, pc] -> do
      let expectedValues = IM.fromList [(fromIntegral p, fromIntegral p + 100) | p <- [pa, pb, pc]]
      writeIORef stateUpdates expectedValues
      _ <- warmup2 ctx inp0 ui
      initialStates <- readIORef paneStates
      assertEq failed (IM.map snd initialStates) expectedValues
      rs <- readIORef rects
      assertJust failed ((,) <$> IM.lookup (fromIntegral pa) rs <*> IM.lookup (fromIntegral pc) rs) $ \(ra, rc) -> do
          -- Inside the header, only 2px from the divider: the gutter's
          -- leeway must not extend into this pane and steal the drag.
          let grab = V2 (rectX ra + rectW ra - 2) (rectY ra + 12)
              -- 30px above the grid's bottom edge: inside the pane's bottom
              -- drop zone but clear of the 20px top-level band. Horizontally
              -- the grid's middle: targets are tested with the dragged pane
              -- lifted out, where this pane spans the full width, and its own
              -- middle there (3/4 across the grid) is already its right zone.
              dest = V2 (gx + gw / 2) (rectY rc + rectH rc - 30)
              ps = IM.elems rs
              gx = minimum (map rectX ps)
              gy = minimum (map rectY ps)
              gw = maximum (map (\r -> rectX r + rectW r) ps) - gx
              gh = maximum (map (\r -> rectY r + rectH r) ps) - gy
          -- The order assert below cannot tell a pane-level split from a
          -- top-level band drop ([pb, pc, pa] either way), so pin the pointer
          -- to the pane-split path first.
          assert failed (topLevelDropTarget 20 (Rect gx gy gw gh) dest == Nothing)
          let press =
                inp0
                  { inputMousePos = grab
                  , inputMouseDown = True
                  , inputMousePressed = True
                  , inputMouseReleased = False
                  }
              hold = press {inputMousePos = dest, inputMousePressed = False}
              release = hold {inputMouseDown = False, inputMouseReleased = True}
          _ <- runFrame ctx press ui
          writeIORef rects IM.empty
          writeIORef paneStates IM.empty
          _ <- runFrame ctx hold ui
          during <- readIORef rects
          duringStates <- readIORef paneStates
          assertEq failed duringStates (IM.delete (fromIntegral pa) initialStates)
          assert failed (not (IM.member (fromIntegral pa) during))
          assertEq failed (IM.size during) 2
          assert failed (all ((== gw) . rectW) (IM.elems during))
          -- Crossing back over the original grab point must not make the
          -- pane reappear or collapse the drag back into a click.
          writeIORef rects IM.empty
          _ <- runFrame ctx (hold {inputMousePos = grab}) ui
          returned <- readIORef rects
          assert failed (not (IM.member (fromIntegral pa) returned))
          -- Releasing outside the grid restores the committed layout.
          (cancelled, _, _, _) <- runFrame ctx (release {inputMousePos = V2 (-20) (-20)}) ui
          assertEq failed (pgrPanes cancelled) [pa, pb, pc]
          writeIORef rects IM.empty
          _ <- warmup2 ctx inp0 ui
          restored <- readIORef rects
          assertEq failed restored rs
          restoredStates <- readIORef paneStates
          assertEq failed restoredStates initialStates
          _ <- runFrame ctx press ui
          _ <- runFrame ctx hold ui
          (pgr1, _, _, _) <- runFrame ctx release ui
          assertEq failed (pgrPanes pgr1) [pb, pc, pa]
          _ <- warmup2 ctx inp0 ui
          droppedStates <- readIORef paneStates
          assertEq failed droppedStates initialStates
          buttons <- readIORef closeRects
          assertJust failed (IM.lookup (fromIntegral pa) buttons) $ \closeRect -> do
            let closePos = spanCenter closeRect
                closePress = press {inputMousePos = closePos}
                closeHold = hold {inputMousePos = V2 (v2X closePos + 60) (v2Y closePos + 40)}
            _ <- runFrame ctx closePress ui
            writeIORef rects IM.empty
            _ <- runFrame ctx closeHold ui
            duringClose <- readIORef rects
            assertEq failed (IM.size duringClose) 3
            (closed, _, _, _) <- runFrame ctx (release {inputMousePos = closePos}) ui
            assertEq failed (pgrPanes closed) [pb, pc]
    _ -> assert failed False


-- | While a pane is dragged over a drop target, the grid is laid out as the
-- drop will leave it, with the dragged pane's slot left empty for the
-- highlight. Highlighting the landing rect over the pre-drop panes is not
-- enough: a drop moves the other panes too, so once the splits are uneven the
-- highlight lines up with nothing on screen (a swap sends the target to the
-- dragged pane's old slot). Every other pane must therefore already sit, while
-- hovering, exactly where it sits after the release.
runPaneGridDropPreviewTest :: Context -> IORef Int -> IO ()
runPaneGridDropPreviewTest ctx failed = do
  -- Per pane: 'pgcRect', and the solved rect of a full-width strip inside it.
  seen <- newIORef IM.empty
  seed <- newIORef SeedStart
  let inp0 = withInput 600 400
      cfg =
        testGridConfig
          { pgViewPane = \pid pctx -> do
              strip <- labelWith' (fixedH 20 . fillW . tight) "H"
              liftIO (modifyIORef' seen (IM.insert (fromIntegral pid) (pgcRect pctx, respRect strip)))
              seedMixedGrid seed pid pctx
              pure (PaneView "P" True Nothing)
          }
      ui = paneGrid cfg
      -- The layout as drawn once the given input has settled (the strip's
      -- solved rect is a frame behind the layout that produced it).
      layoutAt inp = do
        _ <- warmup2 ctx inp ui
        writeIORef seen IM.empty
        _ <- runFrame ctx inp ui
        readIORef seen
      -- Lift pane @p@ out of the grid and hover at @dest@, a point chosen
      -- from the layout with @p@ removed (what drop targets are tested
      -- against). Returns the layout with @p@ lifted and no target, while
      -- hovering, and after the release.
      dragPane p dest = do
        before <- layoutAt inp0
        case IM.lookup p before of
          Nothing -> assert failed False >> pure (IM.empty, IM.empty, IM.empty)
          Just (Rect px py _ _, _) -> do
            _ <- runFrame ctx (pressAt inp0 (V2 (px + 10) (py + 10))) ui
            -- Parked outside the grid: no target, the pane's space closes up.
            lifted <- layoutAt (holdAt inp0 (V2 (-50) (-50)))
            during <- layoutAt (holdAt inp0 (dest lifted))
            _ <- runFrame ctx (releaseAt (holdAt inp0 (dest lifted))) ui
            after <- layoutAt inp0
            pure (lifted, during, after)
      at q (fx, fy) lifted = case IM.lookup q lifted of
        Just (Rect x y w h, _) -> V2 (x + w * fx) (y + h * fy)
        Nothing -> V2 (-50) (-50)
      -- Whole pixels: a dragged ratio leaves float dust (112.00001).
      wholePx (Rect x y w h) = Rect (r x) (r y) (r w) (r h) where r v = fromIntegral (round v :: Int)
      -- The hover showed the drop: nothing else moves on release, and the
      -- dragged pane (not rendered while lifted) takes the empty slot.
      previewed p (lifted, during, after) = do
        assert failed (not (IM.member p lifted) && not (IM.member p during))
        assert failed (IM.member p after)
        assertEq failed during (IM.delete p after)
  _ <- layoutAt inp0
  _ <- layoutAt inp0
  -- Pane 1 left, panes 3 (top) and 5 (bottom) right. Drag the root divider
  -- from the middle to x = 120 so the columns are uneven.
  _ <- runFrame ctx (pressAt inp0 (V2 300 200)) ui
  _ <- runFrame ctx (holdAt inp0 (V2 120 200)) ui
  _ <- runFrame ctx (releaseAt (holdAt inp0 (V2 120 200))) ui
  uneven <- layoutAt inp0
  assertEq failed (IM.map (wholePx . fst) uneven) $
    IM.fromList
      [ (1, Rect 0 0 112 400)
      , (3, Rect 128 0 472 192)
      , (5, Rect 128 208 472 192)
      ]
  -- The model rects are the solver's: the strip spans its pane.
  assert failed (all (\(m, s) -> (rectX (wholePx m), rectW (wholePx m)) == (rectX (wholePx s), rectW (wholePx s))) (IM.elems uneven))
  -- Center drop swaps. Lifted, pane 3 spans the whole top row; the swap sends
  -- it to pane 1's narrow column, which the hover must already show.
  swap@(liftedS, duringS, _) <- dragPane 1 (at 3 (0.5, 0.5))
  assertEq failed (fmap (wholePx . fst) (IM.lookup 3 liftedS)) (Just (Rect 0 0 600 192))
  assertEq failed (fmap (wholePx . fst) (IM.lookup 3 duringS)) (Just (Rect 0 0 112 400))
  previewed 1 swap
  -- Edge drops split the target; the target is shown already halved.
  previewed 1 =<< dragPane 1 (at 5 (0.1, 0.5))
  previewed 5 =<< dragPane 5 (at 3 (0.5, 0.9))
  -- The grid's outer band squeezes the whole grid into the other half.
  top@(_, duringT, _) <- dragPane 3 (const (V2 590 200))
  assert failed (all (\(Rect x _ w _, _) -> x + w <= 300) (IM.elems duringT))
  previewed 3 top
  -- A gutter between two panes still has a target (the nearer pane), so the
  -- preview does not drop out while the pointer crosses it.
  gut@(liftedG, duringG, _) <- dragPane 1 $ \lifted ->
    let panes = map fst (IM.elems lifted)
        gutterPoints =
          [ pt
          | Rect x y w h <- panes
          , pt@(V2 px py) <- [V2 (x + w + 2) (y + h / 2), V2 (x + w / 2) (y + h + 2)]
          , px > 20 && px < 580 && py > 20 && py < 380
          , not (any (`rectContains` pt) panes)
          ]
     in case gutterPoints of
          pt : _ -> pt
          [] -> V2 (-50) (-50)
  assert failed (duringG /= liftedG)
  previewed 1 gut
  -- Outside the grid there is no target: the release cancels, and the uneven
  -- layout comes back untouched (a top-level drop would re-split it 50/50).
  start <- layoutAt inp0
  (_, duringC, afterC) <- dragPane 1 (const (V2 (-20) 200))
  assert failed (not (IM.member 1 duringC))
  assertEq failed afterC start

-- | A pinned pane keeps its own width while the grid resizes: the other side
-- of its split takes the whole of the difference, from the first frame at the
-- new size. A ratio alone can only say that both sides scale, which is what
-- the same grid does once the pin comes off.
runPaneGridPinnedPaneTest :: Context -> IORef Int -> IO ()
runPaneGridPinnedPaneTest ctx failed = do
  rects <- newIORef IM.empty
  split <- newIORef False
  let inp0 = withInput 600 400
      cfg pinned =
        testGridConfig
          { pgFixedPanes = if pinned then (== 1) else const False
          , pgViewPane = \pid pctx -> do
              liftIO (modifyIORef' rects (IM.insert (fromIntegral pid) (pgcRect pctx)))
              -- One split on the first frame: pane 1 on the left, pane 3 (the
              -- seed's next id) on the right.
              done <- liftIO (readIORef split)
              when (not done) $ do
                _ <- pgcSplit pctx AxisV
                liftIO (writeIORef split True)
              pure (PaneView "P" False Nothing)
          }
      ui pinned = paneGrid (cfg pinned)
      -- @n@ frames at @inp@, then one more to report what they solved:
      -- 'pgcRect' is a frame behind, so @n = 1@ reads the very first frame at
      -- a new size and a larger @n@ reads the settled layout.
      settleAt pinned inp n = do
        forM_ [1 .. n :: Int] $ \_ -> void (runFrame ctx inp (ui pinned))
        writeIORef rects IM.empty
        _ <- runFrame ctx inp (ui pinned)
        readIORef rects
      widthNear what ms p want =
        assertJust failed (IM.lookup p ms) $ \r ->
          if abs (rectW r - want) <= 1
            then pure ()
            else assertEq failed (what :: String, rectW r) (what, want)
  _ <- warmup2 ctx inp0 (ui True)
  _ <- warmup2 ctx inp0 (ui True)
  -- Drag the divider left so the two panes are unmistakably uneven: with a
  -- 4px line and 6px of leeway on each side the gutter is 16, leaving 584 to
  -- share out, and the pointer at 120 puts the left pane at 112.
  _ <- runFrame ctx (pressAt inp0 (V2 300 200)) (ui True)
  _ <- runFrame ctx (holdAt inp0 (V2 120 200)) (ui True)
  _ <- runFrame ctx (releaseAt (holdAt inp0 (V2 120 200))) (ui True)
  start <- settleAt True inp0 2
  widthNear "dragged" start 1 112
  widthNear "dragged" start 3 472
  -- 300px wider. The pinned pane is already at its own width on the frame the
  -- new size arrives on, not a frame later, so a window dragged bigger cannot
  -- shimmer it.
  firstWide <- settleAt True (withInput 900 400) 1
  widthNear "the first frame of a wider grid" firstWide 1 112
  wide <- settleAt True (withInput 900 400) 3
  widthNear "a wider grid" wide 1 112
  widthNear "a wider grid" wide 3 772
  -- The same going the other way.
  narrow <- settleAt True inp0 3
  widthNear "a narrower grid" narrow 1 112
  widthNear "a narrower grid" narrow 3 472
  -- Unpinned, the same tree at the same ratio scales as it always did.
  loose <- settleAt False (withInput 900 400) 3
  widthNear "an unpinned grid" loose 1 169.5
  -- Pinned again, the width it was left at is the one it keeps.
  repinned <- settleAt True inp0 3
  widthNear "a pinned grid" repinned 1 169.5
  -- Too narrow to hold the pinned width and the neighbour's minimum both: the
  -- pinned pane gives way rather than pushing its neighbour off the grid.
  squeezed <- settleAt True (withInput 100 400) 3
  widthNear "a grid with no room" squeezed 1 44
  widthNear "a grid with no room" squeezed 3 40
  -- Trees the interactive case above cannot reach, against the pure re-ratio
  -- the widget runs. A pin holds the split its pane hangs off and no other,
  -- so two of them hold at once and an ancestor on the other axis is left to
  -- share as it always did.
  let minSize = 40
      gutter = 16
      area w h = Rect 0 0 w h
      lay t r = fst (layoutNode minSize gutter t r)
      reflow fixed old new t = reflowFixed (`elem` fixed) minSize gutter old new t
      sideOf pick what t r p want =
        assertJust failed (M.lookup p (lay t r)) $ \g ->
          if abs (pick g - want) <= 0.01
            then pure ()
            else assertEq failed (what :: String, pick g) (what, want)
      widthOf = sideOf rectW
      heightOf = sideOf rectH
  -- A pinned pane at each end: only the middle one takes the extra 300px.
  let ends = Split 10 AxisV 0.25 (Pane 1) (Split 11 AxisV 0.5 (Pane 2) (Pane 3))
      ends' = reflow [1, 3] (area 600 400) (area 900 400) ends
  widthOf "two pinned ends" ends (area 600 400) 1 146
  widthOf "two pinned ends" ends (area 600 400) 3 211
  widthOf "two pinned ends" ends' (area 900 400) 1 146
  widthOf "two pinned ends" ends' (area 900 400) 3 211
  widthOf "two pinned ends" ends' (area 900 400) 2 511
  -- A pinned width under a split on the other axis: the width holds, and the
  -- heights, which no pin hangs off, still share the grid out as before.
  let stacked = Split 20 AxisH 0.5 (Split 21 AxisV 0.5 (Pane 1) (Pane 2)) (Pane 3)
      stacked' = reflow [1] (area 600 400) (area 900 600) stacked
  widthOf "a pin under a stack" stacked (area 600 400) 1 292
  widthOf "a pin under a stack" stacked' (area 900 600) 1 292
  widthOf "a pin under a stack" stacked' (area 900 600) 2 592
  heightOf "a pin under a stack" stacked' (area 900 600) 1 292
  heightOf "a pin under a stack" stacked' (area 900 600) 3 292
  -- A pane pinned on the right holds through a drag that grows the grid by a
  -- fraction of a unit a frame, as a zoomed window does by a pixel: the
  -- rounding that keeps the pinned side whole must not walk it.
  let rightPin = Split 30 AxisV 0.5 (Pane 1) (Pane 2)
      at k = area (600 + k * 2 / 3) 400
      dragged = foldl (\t k -> reflow [2] (at k) (at (k + 1)) t) rightPin [0 .. 29 :: Float]
  widthOf "a right-hand pin through a fractional drag" rightPin (at 0) 2 292
  widthOf "a right-hand pin through a fractional drag" dragged (at 30) 2 292

-- A button scrolled above its viewport can geometrically overlap the header,
-- but its invisible rectangle must not claim the header's drag press.
runPaneGridClippedControlTest :: Context -> IORef Int -> IO ()
runPaneGridClippedControlTest ctx failed = do
  rendered <- newIORef False
  geometry <- newIORef Nothing
  let inp0 = withInput 300 240
      ui = paneGrid defaultPaneGridConfig
        { pgLayout = fillW . fillH
        , pgViewPane = \_ _ -> do
            liftIO (writeIORef rendered True)
            header <- labelWith' (fixedH 40 . fillW . tight) "Header"
            (sid, target) <- scrollArea (fixedH 120 . fillW . tight) $
              columnWith tight $ do
                b <- button' "Scrolled control"
                mapM_ (\_ -> void (label "Scroll content")) [1 .. 20 :: Int]
                pure b
            liftIO (writeIORef geometry (Just (respId header, sid, respId target)))
            pure (PaneView "Panel" True Nothing)
        }
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (readIORef geometry) $ \(headerId, sid, targetId) -> do
    headerRect <- getPrevRect ctx headerId
    targetRect <- getPrevRect ctx targetId
    assertJust failed ((,) <$> headerRect <*> targetRect) $ \(hr, br) -> do
        -- Place the button's invisible center exactly in the header.
        let headerY = rectY hr + rectH hr / 2
        setScrollOffset ctx sid (rectY br + rectH br / 2 - headerY)
        _ <- warmup2 ctx inp0 ui
        assertJustM failed (getPrevRect ctx targetId) $ \r -> do
          let grab = spanCenter r
              press = pressAt inp0 grab
              hold = press {inputMousePressed = False, inputMousePos = V2 (v2X grab + 30) (v2Y grab)}
          assert failed (rectContains hr grab)
          _ <- runFrame ctx press ui
          writeIORef rendered False
          _ <- runFrame ctx hold ui
          stillRendered <- readIORef rendered
          assert failed (not stillRendered)

-- Clicking the embedded clear (×) must empty the field, keep focus, and fire an
-- immediate (non-debounced) change pulse.
runSearchInputClearTest :: Context -> IORef Int -> IO ()
runSearchInputClearTest ctx failed = do
  queryRef <- newIORef "hello world"
  let inp0 = withInput 320 100
      ui = column (held queryRef (searchInput' "Search…"))
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
  assertJustM failed (scanClear (bx + bw - 6)) $ \cx -> do
    let press = inp0 {inputMousePos = V2 cx cy, inputMouseDown = True, inputMousePressed = True, inputMouseReleased = False}
    _ <- runFrame ctx press ui
    ((r1, t1), _, _, _) <- runFrame ctx inp0 ui
    assertEq failed t1 ""
    assert failed (respChanged r1)
    ((r2, _), _, _, _) <- runFrame ctx inp0 ui
    assert failed (not (respChanged r2))

-- Typing is echoed immediately but the change pulse only fires after the text
-- has been idle for the configured debounce window.
runSearchInputDebounceTest :: Context -> IORef Int -> IO ()
runSearchInputDebounceTest ctx failed = do
  queryRef <- newIORef ""
  let inp0 = withInput 320 100
      ui = column (held queryRef (searchInputConfigured' (defaultSearchInputConfig {sicDebounceMs = 40})))
  warmupFocused ctx inp0 ui
  ((rA, tA), _, _, _) <- runFrame ctx (inp0 {inputChars = "a"}) ui
  assertEq failed tA "a"
  assert failed (not (respChanged rA))
  ((rB, tB), _, _, _) <- runFrame ctx (inp0 {inputChars = "b"}) ui
  assertEq failed tB "ab"
  assert failed (not (respChanged rB))
  -- No input marks the end of the pause, so the field asks for the frame
  -- that commits: the loop sleeps until then instead of polling.
  typedAt <- getMonotonicTime
  wakeAt <- getWakeAt ctx
  assert failed (wakeAt > typedAt && wakeAt < typedAt + 0.06)
  threadDelay 80000
  ((rC, tC), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed tC "ab"
  assert failed (respChanged rC)
  -- Committed: nothing is pending.
  assertEq failed 0 =<< getWakeAt ctx
  threadDelay 50000
  ((rD, _), _, _, _) <- runFrame ctx inp0 ui
  assert failed (not (respChanged rD))

-- Text the caller puts in a focused search field, which nobody has typed in,
-- commits after one pause like typed text. With no edit time to age from it
-- once looked freshly edited on every frame: it never committed, and woke
-- the loop every debounce period for good.
runSearchInputSetTextDebounceTest :: Context -> IORef Int -> IO ()
runSearchInputSetTextDebounceTest ctx failed = do
  queryRef <- newIORef ""
  let inp0 = withInput 320 100
      ui = column (held queryRef (searchInputConfigured' (defaultSearchInputConfig {sicDebounceMs = 40})))
  warmupFocused ctx inp0 ui
  writeIORef queryRef "recent"
  ((rA, tA), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed tA "recent"
  assert failed (not (respChanged rA))
  assert failed . (> 0) =<< getWakeAt ctx
  threadDelay 80000
  ((rB, _), _, _, _) <- runFrame ctx inp0 ui
  assert failed (respChanged rB)
  assertEq failed 0 =<< getWakeAt ctx

runKvMultilineHeightTest :: Context -> IORef Int -> IO ()
runKvMultilineHeightTest ctx failed = do
  let
    inp0 = withInput 320 400
    ui = column $ do
      card $ do
        kv "Notes" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
        kv "Tree" "0"
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case (spanYOf "Line 5" spans, spanYOf "Tree" spans) of
    ([line5Y], [treeY]) ->
      assert failed (treeY > line5Y)
    _ -> assert failed False
