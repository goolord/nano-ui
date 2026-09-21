module Cases.Scroll (tests) where

import Control.Monad (forM, forM_, replicateM, replicateM_, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Context (ctxNodeArena, setDrawSnapScale)
import NanoUI.Internal.Layout.Arena
  ( NodeType (..)
  , findNodeM
  , getNodeValue
  , getNodeType
  , getRect
  , getScrollContentW
  , getWidgetId
  )
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, assertJust, assertJustM, withInput)
import NanoUI.Testing.Harness
  ( assertScrollGutterPad
  , centerOf
  , drawQuads
  , findGrabHover
  , runClick
  , spanCenter
  , spanYOf
  , tabInp
  , warmup2
  , withInputOff
  )
import Spec (Spec, pixelSpec, spec)

tests :: [Spec]
tests =
  [ spec "scroll-thumb-cursor" runScrollThumbCursorTest
  , pixelSpec "scroll-bar-gutter" runScrollBarGutterTest
  , spec "scroll-damage" runScrollDamageTest
  , pixelSpec "scroll-top-clip" runScrollTopClipTest
  , spec "nested-scroll" runNestedScrollTest
  , spec "nested-scroll-focus" runNestedScrollFocusTest
  , spec "scroll-hover-clip" runScrollHoverClipTest
  , spec "scroll-button-click" runScrollButtonClickTest
  , spec "scroll-scrolled-out" runScrolledOutImmunityTest
  , spec "scroll-lockstep-probe" runScrollLockstepProbeTest
  , spec "scroll-2d-grow-min-width" runScroll2DGrowMinWidthTest
  , spec "page-scroll-backdrop-coverage" runPageScrollBackdropCoverageTest
  , pixelSpec "scroll-2d-pad-fill-overflow" run2DPadFillOverflowTest
  , pixelSpec "scroll-2d-pad-overflow-scrolls" run2DPadOverflowScrollsTest
  , pixelSpec "scroll-step" runScrollStepTest
  , pixelSpec "scroll-smooth" runScrollSmoothTest
  , pixelSpec "scroll-metrics" runScrollMetricsTest
  , pixelSpec "scroll-into-view" runScrollIntoViewTest
  , pixelSpec "scroll-glide-clamp" runScrollGlideClampTest
  ]

runScrollThumbCursorTest :: Context -> IORef Int -> IO ()
runScrollThumbCursorTest ctx failed = do
  let inp0 = withInput 200 120
      ui = scrollArea (fillW . fixedH 80)
             (column (replicateM 8 (label "scroll line") >> pure ()))
  ((sid, ()), _, _, _) <- runFrame ctx inp0 ui >>= \_ -> runFrame ctx inp0 ui
  assertJustM failed (getPrevRect ctx sid) $ \(Rect rx ry rw rh) -> do
    let thumbX = rx + rw - scrollBarGutter ScrollBarList 0 / 2
        tryYs = [ry + rh * n / 8 | n <- [1 .. 7]]
    assertJustM failed (findGrabHover ctx ui inp0 thumbX tryYs) $ \hover -> do
      kind <- uiCursorKind ctx hover
      assertEq failed kind UiCursorGrab
      let press = hover {inputMouseDown = True, inputMousePressed = True}
      _ <- runFrame ctx press ui
      grabbing <- cursorKindIs ctx press UiCursorGrabbing
      assert failed grabbing

-- The scroll content's right edge stops at the scrollbar gutter, one gap
-- before the bar. The gap matches the scroller's right padding and is never
-- under the side gap. A list bar keeps that gap to its well's edge as well;
-- a page bar sits a side gap inside the page's edge.
runScrollBarGutterTest :: Context -> IORef Int -> IO ()
runScrollBarGutterTest ctx failed = do
  let listPad = padR (layoutPadding defaultLayout)
      wideThen n = do
        r <- labelWith' fillW "Wide"
        _ <- replicateM n (label "scroll line")
        pure r
      cases =
        [ ( withInput 200 120
          , scrollArea (fillW . fixedH 60) (wideThen 8)
          , scrollBarGutter ScrollBarList listPad
          , listPad
          )
        , ( withInput 240 140
          , scrollArea (tight . grow) (wideThen 20)
          , scrollBarGutter ScrollBarPage 0
          , 0
          )
        , ( withInput 240 140
          , scrollArea (padAll 12 . grow) (wideThen 20)
          , scrollBarGutter ScrollBarPage 12
          , 12
          )
        , ( withInput 240 140
          , panelWith grow (scrollArea (tight . grow) (wideThen 20))
          , scrollBarGutter ScrollBarList 0
          , 0
          )
        ]
  forM_ cases $ \(inp0, ui, gutter, endPad) -> do
    (sid, child) <- warmup2 ctx inp0 ui
    assertScrollGutterPad failed ctx sid child gutter endPad
  -- The padded page's bar takes the pointer just inside the page's edge. The
  -- sliver past it and the gap before it stay clear.
  let inp0 = withInputOff 240 140
      page = scrollArea (padAll 12 . grow) (wideThen 20)
  (sid, _) <- warmup2 ctx inp0 page
  assertJustM failed (getPrevRect ctx sid) $ \(Rect sx sy sw sh) -> do
    let ys = [sy + sh * n / 8 | n <- [1 .. 7]]
    let barLeft = sx + sw - scrollBarGutter ScrollBarPage 12
    onBar <- findGrabHover ctx page inp0 (barLeft + scrollBarWidth / 2) ys
    assert failed (isJust onBar)
    past <- findGrabHover ctx page inp0 (sx + sw - 1) ys
    assert failed (isNothing past)
    gapBefore <- findGrabHover ctx page inp0 (barLeft - 6) ys
    assert failed (isNothing gapBefore)

-- Each change of a scroll offset damages the scroll viewport only.
runScrollDamageTest :: Context -> IORef Int -> IO ()
runScrollDamageTest ctx failed = do
  let scrollUi =
        fmap fst $
          scrollArea (fillW . fixedH 60) $
            column (replicateM 8 (label "scroll line") >> pure ())
      inp0 = withInputOff 200 120
  sid <- warmup2 ctx inp0 scrollUi
  forM_ [24, 48] $ \off -> do
    _ <- runFrame ctx inp0 (scrollUi >> uiIO (setScrollOffset ctx sid off))
    dScroll <- takeDamage ctx
    case dScroll of
      DamageFull -> assert failed False
      DamageClip r -> assert failed (rectW r > 0 && rectH r > 0 && rectH r <= 60 + defaultDamageSlop * 2 && not (damageIsEmpty dScroll))

-- Ghosting guard: a grow×grow (page-level) scroll container paints no well,
-- so on clip frames the strip vacated by scrolled content has no covering
-- command and the retained texture would show stale pixels, a ghost of a
-- previous scroll position. Every frame must emit a full-viewport fill (the
-- window-color backdrop) so clip replay repaints the whole viewport.
runPageScrollBackdropCoverageTest :: Context -> IORef Int -> IO ()
runPageScrollBackdropCoverageTest ctx failed = do
  let inp0 = withInputOff 300 220
      ui = fmap fst $
        scrollArea
          grow
          (column (replicateM 20 (label "scroll backdrop line") >> pure ()))
  sid <- warmup2 ctx inp0 ui
  setScrollOffset ctx sid 120
  _ <- runFrame ctx inp0 ui
  (_, _, draw, _) <- runFrame ctx inp0 ui
  assertJustM failed (getPrevRect ctx sid) $ \(Rect rx ry rw rh) -> do
    quads <- drawQuads draw
    let covered =
          any
            (\(Rect qx qy qw qh, _) ->
              abs (qx - rx) <= 0.6
                && abs (qy - ry) <= 0.6
                && abs (qx + qw - (rx + rw)) <= 0.6
                && abs (qy + qh - (ry + rh)) <= 0.6)
            quads
    assert failed covered

runScrollTopClipTest :: Context -> IORef Int -> IO ()
runScrollTopClipTest ctx failed = do
  cbRef <- newIORef Nothing
  let inp0 = withInputOff 400 160
      ui = do
        scrollWith (tight . grow) $
          columnWith (padAll 8 . gap 8 . fillW) $
            card $ do
              heading "Controls"
              (cb, _) <- checkbox' "Feature" False
              _ <- slider 0 100 50
              mapM_ (\i -> void (label (T.pack ("pad line " <> show (i :: Int))))) [1 .. 16]
              uiIO $ writeIORef cbRef (Just cb)
              pure ()
      clipFits dmg = case dmg of
        DamageFull -> True
        DamageClip (Rect _ y _ h) -> y >= -1 && y + h <= 160 + 1
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (readIORef cbRef) $ \cb -> do
    assertJustM failed (getPrevRect ctx (respId cb)) $ \r -> do
      let hover = inp0 {inputMousePos = spanCenter r}
      _ <- runFrame ctx hover ui
      dHover <- takeDamage ctx
      assert failed (clipFits dHover)

runNestedScrollTest :: Context -> IORef Int -> IO ()
runNestedScrollTest ctx failed = do
  let inp0 = withInput 200 200
      ui = scrollArea (fillW . fixedH 90) $
             column $ do
               (inner, ()) <- scrollArea (fillW . fixedH 40) $
                                column (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12])
               mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 12]
               pure inner
  (outer, inner) <- warmup2 ctx inp0 ui
  mInner <- getPrevRect ctx inner
  mOuter <- getPrevRect ctx outer
  case (mInner, mOuter) of
    (Just r@(Rect ix iy iw ih), Just (Rect _ oy _ oh)) | iw > 0 && ih > 0 -> do
      let hoverInner = inp0 {inputMousePos = spanCenter r}
          wheelInner = hoverInner {inputScroll = V2 0 1}
      offI0 <- getScrollOffset ctx inner
      offO0 <- getScrollOffset ctx outer
      _ <- runFrame ctx wheelInner ui
      offI1 <- getScrollOffset ctx inner
      offO1 <- getScrollOffset ctx outer
      assertGt failed offI1 offI0
      assertEq failed offO1 offO0
      let pumpInner = do
            before <- getScrollOffset ctx inner
            _ <- runFrame ctx wheelInner ui
            after <- getScrollOffset ctx inner
            if after > before then pumpInner else pure ()
      pumpInner
      offO2 <- getScrollOffset ctx outer
      assertEq failed offO2 offO1
      -- Hit rects follow the scroll offset: a wheel just above the fully
      -- scrolled inner viewport must not reach the inner scroller.
      offIMax <- getScrollOffset ctx inner
      _ <- runFrame ctx (inp0 {inputMousePos = V2 (ix + iw / 2) (iy - 6), inputScroll = V2 0 (-1)}) ui
      offIAbove <- getScrollOffset ctx inner
      assertEq failed offIAbove offIMax
      let hoverOuterY = min (oy + oh - 4) (iy + ih + 8)
          wheelOuter = inp0 {inputMousePos = V2 (ix + iw / 2) hoverOuterY, inputScroll = V2 0 1}
      offO3 <- getScrollOffset ctx outer
      _ <- runFrame ctx wheelOuter ui
      offO4 <- getScrollOffset ctx outer
      assertGt failed offO4 offO3
    _ -> assert failed False

runScrollHoverClipTest :: Context -> IORef Int -> IO ()
runScrollHoverClipTest ctx failed = do
  let inp0 = withInput 200 200
      ui = scrollArea (fillW . fixedH 80) $
             column $ do
                mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
                (inner, ()) <- scrollArea (fillW . fixedH 36) $
                                 column (mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 8])
                pure inner
  (_, inner) <- warmup2 ctx inp0 ui
  mInner <- getPrevRect ctx inner
  case mInner of
    Just r@(Rect _ _ iw ih) | iw > 0 && ih > 0 -> do
      let hoverHidden = inp0 {inputMousePos = spanCenter r, inputScroll = V2 0 1}
      offI0 <- getScrollOffset ctx inner
      _ <- runFrame ctx hoverHidden ui
      offI1 <- getScrollOffset ctx inner
      assert failed (offI1 <= offI0)
    _ -> assert failed False

-- A button scrolled into view inside a fixed-height or a page-level (grow)
-- scroller receives the click at its visual position.
runScrollButtonClickTest :: Context -> IORef Int -> IO ()
runScrollButtonClickTest ctx failed = do
  pixel <- newPixelContext
  forM_
    [ (ctx, withInput 240 160, fillW . fixedH 80)
    , (pixel, withInput 640 120, tight . grow)
    ] $ \(c, inp0, scrollLayout) -> do
      let ui = do
            (hit, setHit) <- useText ""
            (sid, resp) <- scrollArea scrollLayout $
                             column $ do
                               mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 6]
                               b <- button' "Target"
                               when (respClicked b) (setHit "yes")
                               pure b
            pure (sid, hit, resp)
      (sid, hit0, _) <- warmup2 c inp0 ui
      assertEq failed hit0 ""
      assertJustM failed (getPrevRect c sid) $ \r -> do
        let wheel = inp0 {inputMousePos = spanCenter r, inputScroll = V2 0 1}
        forM_ [(1 :: Int) .. 8] $ \_ -> void (runFrame c wheel ui)
        off <- getScrollOffset c sid
        assertGt failed off 0
        ((_, _, resp1), _, _, _) <- runFrame c inp0 ui
        (_, hit1, _) <- runClick c inp0 ui (centerOf resp1)
        assertEq failed hit1 "yes"

runNestedScrollFocusTest :: Context -> IORef Int -> IO ()
runNestedScrollFocusTest ctx failed = do
  let inp0 = withInput 240 220
      ui = scrollArea (fillW . fixedH 90) $
             column $ do
               pair <- scrollArea (fillW . fixedH 50) $
                         column $ do
                           b <- button' "In"
                           mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 10]
                           pure b
               mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
               pure pair
  (_, (inner, _)) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  focus <- getFocusId ctx
  assert failed (focus /= WidgetId 0)
  offI0 <- getScrollOffset ctx inner
  -- Wheel events scroll only the scroller under the mouse; owning focus is not
  -- enough, so scrolling away from the inner scroller must not move it.
  let away = inp0 {inputMousePos = V2 230 210, inputScroll = V2 0 1}
  _ <- runFrame ctx away ui
  offI1 <- getScrollOffset ctx inner
  assertEq failed offI1 offI0

-- A button scrolled out of its viewport keeps its last rect, but a pointer
-- there must not hover it, show its cursor, or click it.
runScrolledOutImmunityTest :: Context -> IORef Int -> IO ()
runScrolledOutImmunityTest ctx failed = do
  let inp0 = withInput 240 160
      ui = do
        (hit, setHit) <- useText ""
        (sid, b) <- scrollArea (fillW . fixedH 8) $
                      column $ do
                        mapM_ (\_ -> void (label "pad")) [(1 :: Int) .. 40]
                        btn <- button' "Target"
                        when (respClicked btn) (setHit "yes")
                        pure btn
        pure (sid, b, hit)
  (sid, b, hit0) <- warmup2 ctx inp0 ui
  assertEq failed hit0 ""
  assertJustM failed (getPrevRect ctx sid) $ \r -> do
    let wheel = inp0 {inputMousePos = spanCenter r, inputScroll = V2 0 1}
    forM_ [(1 :: Int) .. 80] $ \_ -> void (runFrame ctx wheel ui)
    assertJustM failed (getPrevRect ctx (respId b)) $ \br -> do
      let pos = spanCenter br
          hover = inp0 {inputMousePos = pos}
      kind <- uiCursorKind ctx hover
      assertEq failed kind UiCursorDefault
      _ <- runFrame ctx hover ui
      hot <- getHotId ctx
      assert failed (hot /= respId b)
      (_, _, hit1) <- runClick ctx hover ui pos
      assertEq failed hit1 ""

-- | Probe for the reported scroll stair-stepping artifact. Steps a scroll
-- container through fractional offsets at a simulated display scale of 2 and
-- decodes the final (snapped) vertex buffer, comparing the per-row motion of
-- text (glyph quads) against geometry (fill quads such as separators).
-- Asserts every row's text and the adjacent geometry moved by the exact same
-- delta on every transition (text/geometry lockstep).
runScrollLockstepProbeTest :: Context -> IORef Int -> IO ()
runScrollLockstepProbeTest ctx failed = do
  setDrawSnapScale ctx 2
  let inp0 = withInput 300 220
      rows =
        [ ("Feature", "Enabled")
        , ("Volume", "50")
        , ("Quality", "High")
        , ("Accent", "#3D7EFF")
        , ("Theme", "Tomorrow at Midnight Min")
        , ("Theme radio", "Theme radio value")
        , ("Name", "Ada Lovelace")
        , ("Notes", "short note")
        , ("Tree", "1 visible item")
        , ("Table sort", "Name")
        ]
      kvRow k v =
        rowWith (tight . gap 12 . alignMid . fillW) $ do
          labelWith (minW 88 . tight) (T.pack k)
          labelWith (tight . fillW . alignEnd) (T.pack v)
      keys = map (T.pack . fst) rows
      ui =
        scrollArea
          (fillW . fixedH 200)
          (column (mapM_ (\(k, v) -> kvRow k v >> separator) rows))
  (sid, ()) <- warmup2 ctx inp0 ui
  let steps = [0.0, 0.3, 0.6, 1.0, 1.3, 1.7, 2.0, 2.4, 2.7, 3.1, 3.4, 3.8]
  yss <- forM steps $ \off -> do
    setScrollOffset ctx sid off
    _ <- runFrame ctx inp0 ui
    (_, _, draw, _) <- runFrame ctx inp0 ui
    spans <- collectTextSpans ctx
    let keyYs = [listToMaybe (spanYOf k spans) | k <- keys]
    quads <- decodeQuads draw
    let fillTops =
          [ qy1
          | (qx1, qy1, qx2, _, u, v) <- quads
          , abs (u - whitePixelU) < 1.0e-6
          , abs (v - whitePixelV) < 1.0e-6
          , qx2 - qx1 > 60.0
          ]
    pure (keyYs, fillTops)
  forM_ (zip yss (drop 1 yss)) $ \((keyA, fillA), (keyB, fillB)) -> do
    let sKeyA = sort [y | Just y <- keyA]
        sKeyB = sort [y | Just y <- keyB]
        sFillA = sort (filter (> 1.0) fillA)
        sFillB = sort (filter (> 1.0) fillB)
        textDs = [x2 - x1 | (x1, x2) <- zip sKeyA sKeyB]
        fillDs = [x2 - x1 | (x1, x2) <- zip sFillA sFillB]
        n = min (length textDs) (length fillDs)
        t0 = case textDs of
          d : _ -> d
          [] -> 0
        f0 = case fillDs of
          d : _ -> d
          [] -> 0
        textUniform = null (take n [i | i <- textDs, abs (i - t0) > 1.0e-3])
        fillUniform = null (take n [i | i <- fillDs, abs (i - f0) > 1.0e-3])
        sync = null textDs || null fillDs || abs (t0 - f0) <= 1.0e-3
    assert failed (textUniform && fillUniform && sync)

-- | Decode the final (post-snap) quad list from a DrawData vertex buffer.
-- The harness emits each Quad as 4 consecutive vertices of 8 floats:
-- x, y, r, g, b, a, u, v at a 32 byte stride (vertexSize).
decodeQuads :: DrawData -> IO [(Float, Float, Float, Float, Float, Float)]
decodeQuads dd =
  withForeignPtr (drawVertices dd) $ \vp -> do
    let n = drawVertexCount dd `div` 4
        fptr = castPtr vp :: Ptr Float
    forM [0 .. n - 1] $ \q -> do
      let vBase = q * 8 * 4
      xs <- forM [0 .. 3] $ \k -> do
        let o = vBase + k * 8
        x <- peekElemOff fptr o
        y <- peekElemOff fptr (o + 1)
        u <- peekElemOff fptr (o + 6)
        v <- peekElemOff fptr (o + 7)
        pure (x, y, u, v)
      let x1 = minimum [x | (x, _, _, _) <- xs]
          y1 = minimum [y | (_, y, _, _) <- xs]
          x2 = maximum [x | (x, _, _, _) <- xs]
          y2 = maximum [y | (_, y, _, _) <- xs]
          (_u, _v) =
            case xs of
              (_, _, u, v) : _ -> (u, v)
              [] -> (0, 0)
      pure (x1, y1, x2, y2, _u, _v)

whitePixelU :: Float
whitePixelU = 1.5 / 1024.0

whitePixelV :: Float
whitePixelV = 1.5 / 1024.0

-- A padded 2D scroller whose fill-width child fits the viewport must not
-- report horizontal overflow: content size is measured from the content
-- origin (after the leading padding), not from the padding-box origin,
-- which double-counts the padding and makes a fitting child look padX
-- wider than the viewport every frame. Same for the main axis of a padded
-- 1D vertical scroller. Fitting content must not wheel-scroll either: the
-- trailing padding extends the scroll range only once an axis genuinely
-- overflows.
run2DPadFillOverflowTest :: Context -> IORef Int -> IO ()
run2DPadFillOverflowTest ctx failed = do
  let inp0 = withInput 320 240
      ui =
        scrollArea2D (padAll 6 . fixedH 168 . fillW) $
          columnWith (tight . fillW) $
            mapM_ (void . label) (map T.pack ["alpha", "beta", "gamma"])
  (wid, ()) <- warmup2 ctx inp0 ui
  assertJustM failed (scrollNodeState ctx wid True) $ \(contentW, innerW) -> assert failed (contentW <= innerW + overflowEps)
  -- No phantom scroll range: wheeling must not move either axis.
  let wheel = inp0 {inputScroll = V2 5 5}
  _ <- runFrame ctx wheel ui
  V2 offX offY <- getScrollOffset2D ctx wid
  assert failed (offX == 0 && offY == 0)
  let ui1 = scrollArea (padAll 6 . fixedH 80 . fillW) (labelWith tight (T.pack "fits"))
  (wid1, ()) <- warmup2 ctx inp0 ui1
  assertJustM failed (scrollNodeState ctx wid1 False) $ \(contentH, innerH) -> assert failed (contentH <= innerH + overflowEps)

-- Padding (padAll 6) used by the 2D pad tests, and the resulting reduction
-- of the scroller rect to the padded inner size.
padTestPx, padTestBoth :: Float
padTestPx = 6
padTestBoth = padTestPx * 2

-- Same overflow epsilon as scrollAxisOverflows / scrollAxisRange.
overflowEps :: Float
overflowEps = 0.5

scrollNodeState :: Context -> WidgetId -> Bool -> IO (Maybe (Float, Float))
scrollNodeState ctx wid is2D = do
  let na = ctxNodeArena ctx
  found <- findNodeM na $ \i -> do
    nt <- getNodeType na i
    if nt == NodeScrollContainer then (== wid) <$> getWidgetId na i else pure False
  forM found $ \i -> do
    contentMain <-
      if is2D
        then getScrollContentW na i
        else getNodeValue na i
    (_, _, rw, rh) <- getRect na i
    pure (contentMain, if is2D then rw - padTestBoth else rh - padTestBoth)

-- The other side of the pad fix: a padded 2D scroller whose child really is
-- wider and taller than the viewport must still report overflow on both
-- axes, wheel-scroll vertically, and let scrolling reach the trailing
-- padding at the end (the range extends past the last child by padB).
run2DPadOverflowScrollsTest :: Context -> IORef Int -> IO ()
run2DPadOverflowScrollsTest ctx failed = do
  let inp0 = (withInput 320 240) {inputMousePos = V2 100 100}
      ui =
        scrollArea2D (padAll 6 . fixedH 168 . fillW) $
          columnWith (tight . fillW) $ do
            labelWith (tight . fixedW 500) (T.pack "wide child")
            mapM_ (void . label) (map T.pack (replicate 30 "scroll line"))
  (wid, ()) <- warmup2 ctx inp0 ui
  assertJustM failed (scrollNodeState ctx wid True) $ \(contentW, innerW) -> do
    -- The 500px child genuinely overflows the ~308px inner width.
    assertGt failed contentW (innerW + 40)
  assertJustM failed (scrollNodeState ctx wid False) $ \(contentH, innerH) -> do
    assertGt failed contentH (innerH + 100)
    -- Scroll far past the end: the clamp must land on the trailing-pad
    -- extended range (content + padB - view), not the flush content - view,
    -- so the bottom padding is reachable. The horizontal bar is active
    -- (the 500px child overflows), so it takes its lane out of the vertical
    -- viewport: view = innerH - laneH.
    let laneH =
          scrollBarGutter ScrollBarList padTestPx
        wheelDown = inp0 {inputScroll = V2 0 50}
    replicateM_ 40 (runFrame ctx wheelDown ui)
    V2 _ offEnd <- getScrollOffset2D ctx wid
    assert failed (abs (offEnd - (contentH + padTestPx - (innerH - laneH))) < 1.5)

-- The wheel covers the configured step per notch: the context's by default,
-- and the scroller's own once it is given one.
runScrollStepTest :: Context -> IORef Int -> IO ()
runScrollStepTest ctx failed = do
  let inp0 = withInput 200 120
      ui = scrollArea (fillW . fixedH 80) (column (replicateM_ 16 (label "scroll line")))
  setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 40}
  (sid, ()) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx sid) $ \r -> do
    let wheel = inp0 {inputMousePos = spanCenter r, inputScroll = V2 0 1}
    _ <- runFrame ctx wheel ui
    assertEq failed 40 =<< getScrollOffset ctx sid
    -- The scroller's own step overrides the context's from the next notch on.
    setScrollStep ctx sid 12
    _ <- runFrame ctx wheel ui
    assertEq failed 52 =<< getScrollOffset ctx sid
    -- Back to the context's step.
    setScrollStep ctx sid 0
    _ <- runFrame ctx wheel ui
    assertEq failed 92 =<< getScrollOffset ctx sid

-- With a glide time set, a notch eases onto its target over several frames,
-- and the frame loop counts the scroller as animating until it lands.
runScrollSmoothTest :: Context -> IORef Int -> IO ()
runScrollSmoothTest ctx failed = do
  let inp0 = withInput 200 120
      ui = scrollArea (fillW . fixedH 80) (column (replicateM_ 16 (label "scroll line")))
  setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 60, scrollSmoothTime = 0.2}
  (sid, ()) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx sid) $ \r -> do
    let tick = inp0 {inputMousePos = spanCenter r, inputDeltaTime = 1 / 60}
        wheel = tick {inputScroll = V2 0 1}
    _ <- runFrame ctx wheel ui
    partial <- getScrollOffset ctx sid
    assertGt failed partial 0
    assert failed (partial < 60)
    assert failed =<< scrollGliding ctx sid
    assert failed =<< anyAnimating ctx
    -- It settles exactly on the target, and stops asking for frames there.
    replicateM_ 30 (runFrame ctx tick ui)
    assertEq failed 60 =<< getScrollOffset ctx sid
    gliding <- scrollGliding ctx sid
    assert failed (not gliding)
    -- A notch mid-glide adds to the throw instead of restarting it.
    _ <- runFrame ctx wheel ui
    _ <- runFrame ctx wheel ui
    replicateM_ 30 (runFrame ctx tick ui)
    assertEq failed 180 =<< getScrollOffset ctx sid
    -- Setting an offset outright wins over whatever was in flight.
    _ <- runFrame ctx wheel ui
    setScrollOffset ctx sid 20
    replicateM_ 5 (runFrame ctx tick ui)
    assertEq failed 20 =<< getScrollOffset ctx sid

-- The metrics a scroller publishes each frame, and the commands that read
-- them: to the end, back to the start, and by whole pages.
runScrollMetricsTest :: Context -> IORef Int -> IO ()
runScrollMetricsTest ctx failed = do
  let inp0 = withInput 200 160
      ui = scrollArea (fillW . fixedH 80) (column (replicateM_ 16 (label "scroll line")))
  (sid, ()) <- warmup2 ctx inp0 ui
  assertJustM failed (getScrollMetrics ctx sid) $ \m -> do
    assertEq failed (scrollAxes m) ScrollAxisY
    assertEq failed (scrollOffset m) (V2 0 0)
    assertGt failed (v2Y (scrollRange m)) 0
    assertEq failed (v2X (scrollRange m)) 0
    -- The viewport is the scroller's box inside its padding and bar lane.
    assertJustM failed (getPrevRect ctx sid) $ \r -> do
      assert failed (rectW (scrollViewport m) <= rectW r)
      assert failed (rectH (scrollViewport m) <= rectH r)
    scrollToEnd ctx sid ScrollInstant
    _ <- runFrame ctx inp0 ui
    assertEq failed (v2Y (scrollRange m)) =<< getScrollOffset ctx sid
    scrollToStart ctx sid ScrollInstant
    _ <- runFrame ctx inp0 ui
    assertEq failed 0 =<< getScrollOffset ctx sid
    scrollPages ctx sid (V2 0 1) ScrollInstant
    _ <- runFrame ctx inp0 ui
    paged <- getScrollOffset ctx sid
    assertEq failed (min (v2Y (scrollRange m)) (rectH (scrollViewport m))) paged

-- Scrolling a widget into view, by widget and by content rectangle.
runScrollIntoViewTest :: Context -> IORef Int -> IO ()
runScrollIntoViewTest ctx failed = do
  let inp0 = withInput 200 160
      ui =
        scrollArea (fillW . fixedH 80) $
          column (forM [1 .. 16 :: Int] (\i -> label' (T.pack ("line " <> show i))))
  (sid, rows) <- warmup2 ctx inp0 ui
  let target = respId (rows !! 11)
  scrollIntoView ctx sid target ScrollStart ScrollInstant
  _ <- runFrame ctx inp0 ui
  mAfter <- getScrollMetrics ctx sid
  mRow <- getPrevRect ctx target
  assertJust failed ((,) <$> mAfter <*> mRow) $ \(m, r) -> do
      -- The row sits against the top of the viewport, whole.
      assert failed (abs (rectY r - rectY (scrollViewport m)) < 1.5)
      -- Already in view: the nearest alignment leaves the offset alone.
      before <- getScrollOffset ctx sid
      scrollIntoView ctx sid target ScrollNearest ScrollInstant
      _ <- runFrame ctx inp0 ui
      assertEq failed before =<< getScrollOffset ctx sid
      -- A row above the viewport comes back to the top edge.
      let above = respId (rows !! 1)
      scrollIntoView ctx sid above ScrollNearest ScrollInstant
      _ <- runFrame ctx inp0 ui
      assertJustM failed (getPrevRect ctx above) $ \ra -> assert failed (abs (rectY ra - rectY (scrollViewport m)) < 1.5)
      -- A content rectangle no widget was built for (a virtualized row)
      -- lands the same way.
      let rowH = rectH r
      scrollRectIntoView ctx sid (Rect 0 (rowH * 8) 10 rowH) ScrollStart ScrollInstant
      _ <- runFrame ctx inp0 ui
      off <- getScrollOffset ctx sid
      assert failed (abs (off - rowH * 8) < 1.5)

-- Content that shrinks under a glide pulls the glide back with it: the
-- scroller must not coast to an offset the shorter content cannot reach and
-- sit there showing nothing.
runScrollGlideClampTest :: Context -> IORef Int -> IO ()
runScrollGlideClampTest ctx failed = do
  rows <- newIORef (40 :: Int)
  let inp0 = withInput 200 120
      ui = do
        n <- uiIO (readIORef rows)
        scrollArea (fillW . fixedH 80) (column (replicateM_ n (label "scroll line")))
  setScrollTuning ctx defaultScrollTuning {scrollWheelStep = 60, scrollSmoothTime = 0.2}
  (sid, ()) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx sid) $ \r -> do
    let tick = inp0 {inputMousePos = spanCenter r, inputDeltaTime = 1 / 60}
        wheel = tick {inputScroll = V2 0 8}
    _ <- runFrame ctx wheel ui
    assert failed =<< scrollGliding ctx sid
    writeIORef rows 12
    replicateM_ 40 (runFrame ctx tick ui)
    mMetrics <- getScrollMetrics ctx sid
    off <- getScrollOffset ctx sid
    assertJust failed mMetrics $ \m -> do
      assertGt failed (v2Y (scrollRange m)) 0
      assert failed (off <= v2Y (scrollRange m) + 0.5)

-- A grow cell with its own minimum width counts as that minimum in a 2D
-- scroller, not as its widest label: the row fits once the window clears the
-- cell's minimum and the fixed cells beside it. Without a minimum, a grow
-- wrapper's content still counts, so wide content keeps its sideways scroll.
runScroll2DGrowMinWidthTest :: Context -> IORef Int -> IO ()
runScroll2DGrowMinWidthTest ctx failed = do
  let longName = T.replicate 12 (T.pack "long name ")
      rows cell =
        scrollArea2D (fillW . fixedH 120) $
          columnWith (fillW . tight) $
            replicateM_ 3 $
              rowWith (fillW . gap 10 . tight) $ do
                void cell
                rowWith (fixedW 80 . tight) (label (T.pack "size"))
      minCell = columnWith (grow . minW 100 . tight) (label longName)
      rangeX inp ui = do
        (wid, ()) <- warmup2 ctx inp ui
        maybe (-1) (v2X . scrollRange) <$> getScrollMetrics ctx wid
  -- The row's least width is 100 + 10 + 80 = 190: fits at 400, not at 150.
  wide <- rangeX (withInput 400 200) (rows minCell)
  assertEq failed wide 0
  narrow <- rangeX (withInput 150 200) (rows minCell)
  assertGt failed narrow 0
  wrapped <- rangeX (withInput 400 200) (rows (columnWith (grow . tight) (rowWith (fixedW 600 . tight) (label (T.pack "wide")))))
  assertGt failed wrapped 0
