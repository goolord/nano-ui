module Cases.Cache (tests) where

import Spec
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import Data.Text qualified as T
import Data.Primitive.SmallArray (smallArrayFromList)
import NanoUI.Internal.Context (Context (..), registerCustomDrawing)
import NanoUI.Internal.Layout.Arena
  ( NodeType (..), addNodeFromLayout, getNodeRect, setNodeText
  , setNodeValue, setStyleIdx, setWidgetId
  )
import NanoUI.Internal.Store (ptrEq)
import System.Mem.StableName (makeStableName)

tests :: [Spec]
tests =
  [ spec "metric-cache-invalidation" runMetricCacheInvalidationTest
  , spec "widget-placement-cache" runWidgetPlacementCacheTest
  , spec "layout-cache-paint-state" runLayoutPaintStateTest
  , spec "draw-reuse" runDrawReuseTest
  , spec "draw-reuse-continuous" runContinuousDrawReuseTest
  , spec "partial-measure-ancestor-width" runPartialMeasureAncestorTest
  , spec "wrap-width-bounds" runWrapBoundsTest
  , spec "wrap-keeps-spaces" runWrapKeepsSpacesTest
  ]

-- | Wrapping drops the run of spaces at a line break but keeps indentation
-- and runs of spaces inside a line, which code needs.
runWrapKeepsSpacesTest :: Context -> IORef Int -> IO ()
runWrapKeepsSpacesTest _ failed = do
  let lineW t = pure (fromIntegral (T.length t))
  assertEq failed ["    let x  = 1", "in  x + 1"] =<< wrapTextLinesIO lineW "    let x  = 1   in  x + 1  " 14
  assertEq failed ["    a", "b  c"] =<< wrapTextLinesIO lineW "    a  b  c" 6

-- | A wrap holds for every width from its widest fitting line up to, not
-- including, its break width: the wrap cache hands it out for all of them.
-- Characters of different widths, words broken by character, forced single
-- characters, blank paragraphs and runs of spaces.
runWrapBoundsTest :: Context -> IORef Int -> IO ()
runWrapBoundsTest _ failed = do
  let charW :: Char -> Float
      charW c = case c of
        ' ' -> 3
        'i' -> 2
        'l' -> 2.5
        'm' -> 9
        'W' -> 11
        _ -> 5 + fromIntegral (fromEnum c `mod` 3)
      lineW t = pure (sum (map charW (T.unpack t)))
      texts =
        [ "the quick brown fox jumps over the lazy dog"
        , "a supercalifragilisticexpialidocious word among small ones"
        , "two\n\nparagraphs, the second with Wide Words mmm www"
        , "nospacesatalljustonelongrunoflettersWWWmmm"
        , "  leading and  double spaced  words  "
        , "    indented    code  = aligned   -- and a comment"
        ]
  forM_ texts $ \txt -> forM_ [1, 3 .. 320 :: Float] $ \w -> do
    r <- wrapTextIO lineW txt w
    let hi = min (wrBreakW r) (w + 400)
        probes = filter (\v -> v > 0 && v >= wrFitW r && v < wrBreakW r) [wrFitW r, w, (wrFitW r + hi) / 2, hi - 0.01]
    assert failed (wrFitW r <= w && w < wrBreakW r)
    forM_ probes $ \v -> do
      again <- wrapTextLinesIO lineW txt v
      assertEq failed (txt, w, v, wrLines r) (txt, w, v, again)

-- Copy the mutable draw buffers before another frame can reuse them. Counts
-- alone cannot detect stale geometry, colors or translated text.
snapshotDraw :: DrawData -> IO (BS.ByteString, BS.ByteString, [DrawCmd])
snapshotDraw draw = do
  vertices <- withForeignPtr (drawVertices draw) $ \p ->
    BS.packCStringLen (castPtr p, drawVertexCount draw * vertexSize)
  indices <- withForeignPtr (drawIndices draw) $ \p ->
    BS.packCStringLen (castPtr p, drawIndexCount draw * indexSize)
  pure (vertices, indices, drawCmdElems draw)

runMetricCacheInvalidationTest :: Context -> IORef Int -> IO ()
runMetricCacheInvalidationTest ctx failed = do
  let inp = withInputOff 400 300
      width c = do
        void $ runFrame c inp (button "ABC")
        Rect _ _ w _ <- getNodeRect (ctxNodeArena c) 0
        pure w
      a = withMeasureText ctx (\_ -> pure (200, 20))
      b = withMeasureText ctx (\_ -> pure (80, 12))
  original <- width ctx
  wa <- width a
  wb <- width b
  assertGt failed wa original
  assertGt failed wa wb
  -- Both variants derive from the same parent and share mutable caches. An
  -- incremented pure Int revision would collide here; returning to A matters.
  assertEq failed wa =<< width a
  assertEq failed original =<< width ctx

  -- Each supported pure modifier must invalidate both layout and placement.
  forM_
    [ (\c -> withFontMetrics c (monospaceMetrics 24), void (button "ABC"))
    , (\c -> withMonoFontMetrics c (monospaceMetrics 24), void (labelWith fontMono "ABC"))
    , (\c -> withFontResolver c (\_ _ _ _ -> pure (monospaceMetrics 24, False))
            (\_ _ _ _ _ -> pure (200, 24)), void (labelWith (fontSize 24) "ABC"))
    ] $ \(configure, ui) -> do
      warm <- newContext
      void $ runFrame warm inp ui
      let configured = configure warm
      (_, _, draw, _) <- runFrame configured inp ui
      actual <- snapshotDraw draw
      spans <- collectTextSpans configured
      fresh <- configure <$> newContext
      (_, _, coldDraw, _) <- runFrame fresh inp ui
      assertEq failed actual =<< snapshotDraw coldDraw
      assertEq failed spans =<< collectTextSpans fresh

-- A table header's width and style stay fixed while alignment and its parent
-- origin change independently. This exercises the placement cache's key.
header :: Context -> AlignX -> Float -> Float -> NanoUI ()
header ctx ax x y = uiIO $ do
  let na = ctxNodeArena ctx
  parent <- addNodeFromLayout na NodeContainer (-1) $
    (fixedWH 320 160 defaultLayout) {layoutPadding = Padding x 0 y 0}
  i <- addNodeFromLayout na NodeButton parent $
    (fixedWH 200 30 defaultLayout) {layoutAlignX = ax}
  setWidgetId na i (WidgetId 123)
  setNodeText na i "Header"
  setStyleIdx na i 0x80000000

runWidgetPlacementCacheTest :: Context -> IORef Int -> IO ()
runWidgetPlacementCacheTest _ctx failed =
  forM_ [1, 1.5, 2] $ \scale -> do
    base <- newContext
    let ctx = withFontMetrics base ((monospaceMetrics 12) {fmSnapScale = scale})
        inp = withInputOff 400 300
    void $ runFrame ctx inp (header ctx AlignStart 0 0)
    start <- collectTextSpans ctx
    (_, _, draw, _) <- runFrame ctx inp (header ctx AlignEnd 0 0)
    aligned <- snapshotDraw draw
    end <- collectTextSpans ctx
    assert failed (start /= end)
    freshBase <- newContext
    let fresh = withFontMetrics freshBase ((monospaceMetrics 12) {fmSnapScale = scale})
    (_, _, expectedDraw, _) <- runFrame fresh inp (header fresh AlignEnd 0 0)
    assertEq failed aligned =<< snapshotDraw expectedDraw

    cache <- readIORef (ctxWidgetTextCache ctx) >>= evaluate >>= makeStableName
    forM_ [(0.25, 0.5), (9.75, 3.25), (0, 0)] $ \(x, y) -> do
      (_, _, movedDraw, _) <- runFrame ctx inp (header ctx AlignEnd x y)
      moved <- snapshotDraw movedDraw
      cache' <- readIORef (ctxWidgetTextCache ctx) >>= evaluate >>= makeStableName
      assert failed (cache == cache')
      -- Force a fresh placement at the same origin and compare actual bytes.
      clearMeasureCache fresh
      (_, _, coldDraw, _) <- runFrame fresh inp (header fresh AlignEnd x y)
      assertEq failed moved =<< snapshotDraw coldDraw

runLayoutPaintStateTest :: Context -> IORef Int -> IO ()
runLayoutPaintStateTest ctx failed = do
  let inp = withInputOff 400 300
      ui value color = do
        column $ do
          box (fixedWH 30 30) color
          uiIO $ do
            let na = ctxNodeArena ctx
            i <- addNodeFromLayout na NodeSlider 0 (fixedWH 200 30 defaultLayout)
            setWidgetId na i (WidgetId 123)
            setNodeText na i ""
            setNodeValue na i value
          void (labelWith (fontColor color) "paint only")
      red = colorRGBA 255 0 0 255
      blue = colorRGBA 0 0 255 255
  (_, _, firstDraw, _) <- runFrame ctx inp (ui 0.2 red)
  first <- snapshotDraw firstDraw
  cache <- readIORef (ctxLayoutCache ctx) >>= evaluate >>= makeStableName
  (_, _, changedDraw, _) <- runFrame ctx inp (ui 0.8 blue)
  changed <- snapshotDraw changedDraw
  cache' <- readIORef (ctxLayoutCache ctx) >>= evaluate >>= makeStableName
  assert failed (cache == cache')
  assert failed (first /= changed)
  -- A cache hit must preserve this frame's slider value and paint colors.
  writeIORef (ctxLayoutCache ctx) Nothing
  (_, _, coldDraw, _) <- runFrame ctx inp (ui 0.8 blue)
  assertEq failed changed =<< snapshotDraw coldDraw

-- | A full frame with no damage, built from the same view output as the
-- last, returns the last frame's draw data itself, unpainted. A change to
-- paint state alone (a box's colour, a slider's value, a font colour, an
-- image or its look, a custom drawing on a container) paints again and
-- draws the change, as does every frame with reuse off, the layout overlay
-- on, or a clip.
runDrawReuseTest :: Context -> IORef Int -> IO ()
runDrawReuseTest ctx failed = do
  let px = BS.replicate (4 * 4 * 4) 200
  okA <- registerImage ctx (ImageId 1) 4 4 px
  okB <- registerImage ctx (ImageId 2) 4 4 px
  assert failed (okA && okB)
  let inp = withInputOff 400 300
      red = colorRGBA 255 0 0 255
      blue = colorRGBA 0 0 255 255
      ui (boxColor, value, fontCol, iid, opacity, stray) = column $ do
        box (fixedWH 30 30) boxColor
        void (slider 0 1 value)
        void (labelWith (fontColor fontCol) "paint only")
        imageConfigured defaultImageConfig {icLayout = fixedWH 20 20, icOpacity = opacity} (ImageId iid)
        -- A custom drawing on a container, which paint builds afresh.
        uiIO $ do
          let na = ctxNodeArena ctx
          i <- addNodeFromLayout na NodeContainer 0 (fixedWH 40 40 defaultLayout)
          setWidgetId na i (WidgetId 777)
          when (stray > 0) $
            registerCustomDrawing ctx (WidgetId 777) 0 $ \_ r ->
              smallArrayFromList [FillRect r (colorRGBA stray 0 0 255)]
      base = (red, 0.2, red, 1, 1, 0)
      frame s = (\(_, _, dd, _) -> dd) <$> runFrame ctx inp (ui s)
      -- Two frames of @s@ after @from@: the first paints @s@, and the second
      -- returns the first's draw data when @reused@.
      check from s reused = do
        before <- frame from >>= snapshotDraw
        d1 <- frame s
        d2 <- frame s
        assertEq failed reused (ptrEq d1 d2)
        now <- snapshotDraw d2
        assert failed (s == from || now /= before)
  replicateM_ 2 (frame base)
  check base base True
  check base (blue, 0.2, red, 1, 1, 0) True
  check base (red, 0.8, red, 1, 1, 0) True
  check base (red, 0.2, blue, 1, 1, 0) True
  check base (red, 0.2, red, 2, 1, 0) True
  check base (red, 0.2, red, 1, 0.5, 0) True
  check base (red, 0.2, red, 1, 1, 100) False
  check (red, 0.2, red, 1, 1, 100) (red, 0.2, red, 1, 1, 200) False
  setDrawReuse ctx False
  check base base False
  setDrawReuse ctx True
  setExplainLayout ctx True
  check base base False
  setExplainLayout ctx False
  writeIORef (ctxPaintFull ctx) False
  check base base False
  writeIORef (ctxPaintFull ctx) True
  check base base True

-- | A continuous session: every frame paints in full and the host does not
-- read damage ('ctxDamageWanted' off around each frame, as the SDL runner
-- sets it). Every frame draws what a context that never reuses draws: a
-- hover, a focus move, each step of an animation drawn by a drawing alone
-- or moving a widget, and each wheel notch paint again and show the
-- change. Idle frames,
-- and the frames after each change once it settles, return the last frame's
-- draw data.
runContinuousDrawReuseTest :: Context -> IORef Int -> IO ()
runContinuousDrawReuseTest ctx failed = do
  ref <- newContext
  setDrawReuse ref False
  lastDraw <- newIORef Nothing
  let off = (withInputOff 300 200) {inputDeltaTime = 0.016}
      ui (barTo, gapTo) = column $ do
        b <- buttonWith' (fixedWH 60 20) "a"
        -- A drawing of an animated value: no node value or style holds it.
        v <- withKey ("bar" :: String) (animateTo (Tween EaseLinear 0.2 0) barTo)
        void (progressBarWith' (fixedW 100) 12 v)
        -- An animated gap moves the widget below it.
        g <- withKey ("gap" :: String) (animateTo (Tween EaseLinear 0.2 0) gapTo)
        void (spacer (Fixed 4) (Fixed (4 + 30 * g)))
        void (buttonWith' (fixedWH 40 20) "b")
        void $ scrollWith (fixedWH 100 40) $ column $ forM_ [1 .. 10 :: Int] $ \i -> label (T.pack (show i))
        pure b
      frame c inp target = do
        writeIORef (ctxDamageWanted c) False
        (b, _, dd, _) <- runFrame c inp (ui target)
        writeIORef (ctxDamageWanted c) True
        pure (b, dd)
      -- One frame on both contexts: its response, whether it reused the
      -- last draw, and whether it draws something else.
      step inp target = do
        (b, dd) <- frame ctx inp target
        now <- snapshotDraw dd
        assertEq failed now . snd =<< (traverse snapshotDraw =<< frame ref inp target)
        prev <- readIORef lastDraw
        writeIORef lastDraw (Just (dd, now))
        pure $ case prev of
          Just (lastDd, lastNow) -> (b, ptrEq dd lastDd, now /= lastNow)
          Nothing -> (b, False, True)
      steps n inp target = replicateM n (step inp target)
      reused (_, r, _) = r
      repainted (_, r, changed) = not r && changed
      -- The first frame after a change paints it, and the last has settled.
      paintsThenSettles xs = case (xs, reverse xs) of
        (first : _, final : _) -> repainted first && reused final
        _ -> False
      -- The frame that starts an animation still draws its start; each after
      -- paints a step, and the settled value is reused.
      animates inp target = do
        moving <- steps 6 inp target
        assert failed (not (any reused moving) && all repainted (drop 1 moving))
        assert failed . all reused . drop 16 =<< steps 20 inp target
  settle <- steps 4 off (0, 0)
  let hover = off {inputMousePos = case reverse settle of (b, _, _) : _ -> centerOf b; [] -> V2 0 0}
  assert failed . all reused =<< steps 3 off (0, 0)
  assert failed . paintsThenSettles =<< steps 12 hover (0, 0)
  assert failed . paintsThenSettles =<< ((:) <$> step (tabInp hover) (0, 0) <*> steps 12 hover (0, 0))
  animates hover (1, 0)
  animates hover (1, 1)
  let wheel = off {inputMousePos = V2 30 120, inputScroll = V2 0 (-1)}
  assert failed . all repainted =<< steps 3 wheel (1, 1)
  assert failed . paintsThenSettles =<< steps 12 wheel {inputScroll = V2 0 0} (1, 1)

-- A label wraps at its container's width. A frame that changes only the
-- container's width must measure the label again, not restore the size it
-- wrapped to under the old width.
runPartialMeasureAncestorTest :: Context -> IORef Int -> IO ()
runPartialMeasureAncestorTest ctx failed = do
  let inp = withInputOff 400 300
      ui c w = uiIO $ do
        let na = ctxNodeArena c
        parent <- addNodeFromLayout na NodeContainer (-1) (fixedWH w 200 defaultLayout)
        i <- addNodeFromLayout na NodeText parent defaultLayout
        setNodeText na i "a long label that wraps onto several lines at a narrow width"
      rects = arenaRects
  void $ runFrame ctx inp (ui ctx 360)
  wide <- rects ctx
  void $ runFrame ctx inp (ui ctx 120)
  narrow <- rects ctx
  assert failed (wide /= narrow)
  fresh <- newContext
  void $ runFrame fresh inp (ui fresh 120)
  assertEq failed narrow =<< rects fresh
