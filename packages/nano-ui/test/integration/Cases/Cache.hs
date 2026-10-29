{-# OPTIONS_GHC -O0 #-}
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
  ( addNodeFromLayout, getNodeRect, setNodeText
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
  , spec "wrap-and-truncate" runWrapTruncateTest
  ]

-- | Wrapping keeps indentation and inner runs of spaces; breaks and cuts keep
-- marks, joined emoji and flags whole; a cut never overflows, dropping the
-- dots when only a first character fits.
runWrapTruncateTest :: Context -> IORef Int -> IO ()
runWrapTruncateTest ctx failed = do
  let flags = "\x1F1FA\x1F1F8\x1F1EB\x1F1F7"
      england = "\x1F3F4\xE0067\xE0062\xE0065\xE006E\xE0067\xE007F"
      narrowDots = (monospaceMetrics 10) {fmAdvance = \case 'W' -> 14; '.' -> 4; ' ' -> 5; _ -> 10}
  forM_
    [ (1, "    let x  = 1   in  x + 1  ", 14, ["    let x  = 1", "in  x + 1"])
    , (1, "    a  b  c", 6, ["    a", "b  c"])
    , (10, "ae\x0301\&b", 20, ["a", "e\x0301", "b"])
    , (10, "\x1F469\x200D\x1F4BBx", 20, ["\x1F469\x200D\x1F4BB", "x"])
    , (10, flags, 30, ["\x1F1FA\x1F1F8", "\x1F1EB\x1F1F7"])
    , (10, "a" <> england, 30, ["a", england])
    ]
    $ \(k, txt, w, want) ->
      assertEq failed want =<< wrapTextLinesIO (\t -> pure (k * fromIntegral (T.length t))) txt w
  forM_
    [ (monospaceMetrics 10, 55, "ae\x0301\&bcd", "a...")
    , (monospaceMetrics 10, 65, flags <> "xyz", "\x1F1FA\x1F1F8...")
    , (narrowDots, 16, "Wide", "W")
    , (narrowDots, 36, "Wide", "Wi...")
    ]
    $ \(fm, w, txt, want) -> assertEq failed want =<< evalUi ctx (withInput 100 100) (truncateTextUi fm w txt)

-- | A wrap holds for every width from its widest line up to its break width,
-- since the wrap cache hands it out for all of them.
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
        , "cafe\x0301 nai\x0308ve e\x0301\&e\x0301\&e\x0301\&e\x0301 \x0928\x092E\x0938\x094D\x0924\x0947 \x1F469\x200D\x1F4BB\x1F469\x200D\x1F4BB"
        ]
  forM_ texts $ \txt -> forM_ [1, 3 .. 320 :: Float] $ \w -> do
    r <- wrapTextIO lineW txt w
    let hi = min (wrBreakW r) (w + 400)
        probes = filter (\v -> v > 0 && v >= wrFitW r && v < wrBreakW r) [wrFitW r, w, (wrFitW r + hi) / 2, hi - 0.01]
    assert failed (wrFitW r <= w && w < wrBreakW r)
    forM_ probes $ \v -> do
      again <- wrapTextLinesIO lineW txt v
      assertEq failed (txt, w, v, wrLines r) (txt, w, v, again)

-- | A copy of the draw buffers, taken before another frame reuses them:
-- counts alone cannot see stale geometry, colours or translated text.
snapshotDraw :: DrawData -> IO (BS.ByteString, BS.ByteString, [DrawCmd])
snapshotDraw draw = do
  vertices <- withForeignPtr (drawVertices draw) $ \p ->
    BS.packCStringLen (castPtr p, drawVertexCount draw * vertexSize)
  indices <- withForeignPtr (drawIndices draw) $ \p ->
    BS.packCStringLen (castPtr p, drawIndexCount draw * indexSize)
  pure (vertices, indices, drawCmdElems draw)

-- | One frame of @ui@, and a copy of what it drew.
drawnBy :: Context -> Input -> NanoUI a -> IO (BS.ByteString, BS.ByteString, [DrawCmd])
drawnBy c inp ui = runFrame c inp ui >>= \(_, dd, _) -> snapshotDraw dd

-- | Contexts derived with other measures share their parent's caches yet lay
-- out with their own (an incremented revision would collide on returning);
-- each font modifier draws as a fresh context does; a new size repaints.
runMetricCacheInvalidationTest :: Context -> IORef Int -> IO ()
runMetricCacheInvalidationTest ctx failed = do
  let inp = withInputOff 400 300
      width c = do
        void $ runFrame c inp (button "ABC")
        rectW <$> getNodeRect (ctxNodeArena c) 0
      a = withMeasureText ctx (\_ -> pure (200, 20))
      b = withMeasureText ctx (\_ -> pure (80, 12))
  original <- width ctx
  wa <- width a
  wb <- width b
  assertGt failed wa original
  assertGt failed wa wb
  assertEq failed wa =<< width a
  assertEq failed original =<< width ctx
  forM_
    [ (\c -> withFontMetrics c (monospaceMetrics 24), void (button "ABC"))
    , (\c -> withMonoFontMetrics c (monospaceMetrics 24), labelWith fontMono "ABC")
    , ( \c -> withFontResolver c (\_ _ _ _ -> pure (monospaceMetrics 24, False)) (\_ _ _ _ _ -> pure (200, 24))
      , labelWith (fontSize 24) "ABC"
      )
    ]
    $ \(configure, ui) -> do
      warm <- newContext
      void $ runFrame warm inp ui
      let configured = configure warm
      actual <- drawnBy configured inp ui
      spans <- collectTextSpans configured
      fresh <- configure <$> newContext
      assertEq failed actual =<< drawnBy fresh inp ui
      assertEq failed spans =<< collectTextSpans fresh
  sized <- newContext
  _ <- warmup2 sized inp (label "ABC")
  _ <- runFrame (withFontSize sized 24) inp (label "ABC")
  assertEq failed DamageFull =<< takeDamage sized

-- | A table header of fixed width and style, at an alignment and parent
-- origin that change independently, as keys the placement cache.
header :: Context -> AlignX -> Float -> Float -> NanoUI ()
header ctx ax x y = liftIO $ do
  let na = ctxNodeArena ctx
  parent <- addNodeFromLayout na NodeContainer (-1) $
    (fixedWH 320 160 defaultLayout) {layoutPadding = Padding x 0 y 0}
  i <- addNodeFromLayout na NodeButton parent $
    (fixedWH 200 30 defaultLayout) {layoutAlignX = ax}
  setWidgetId na i (WidgetId 123)
  setNodeText na i "Header"
  setStyleIdx na i 0x80000000

-- | At each snap scale a new alignment places afresh, and a moved parent
-- reuses the text cache yet draws as a cold placement.
runWidgetPlacementCacheTest :: Context -> IORef Int -> IO ()
runWidgetPlacementCacheTest _ failed =
  forM_ [1, 1.5, 2] $ \scale -> do
    let scaled c = withFontMetrics c ((monospaceMetrics 12) {fmSnapScale = scale})
        inp = withInputOff 400 300
        drawn c ax x y = drawnBy c inp (header c ax x y)
    ctx <- scaled <$> newContext
    fresh <- scaled <$> newContext
    void $ runFrame ctx inp (header ctx AlignStart 0 0)
    start <- collectTextSpans ctx
    aligned <- drawn ctx AlignEnd 0 0
    assert failed . (start /=) =<< collectTextSpans ctx
    assertEq failed aligned =<< drawn fresh AlignEnd 0 0
    let cacheName = readIORef (ctxWidgetTextCache ctx) >>= evaluate >>= makeStableName
    cache <- cacheName
    forM_ [(0.25, 0.5), (9.75, 3.25), (0, 0)] $ \(x, y) -> do
      moved <- drawn ctx AlignEnd x y
      assert failed . (cache ==) =<< cacheName
      clearMeasureCache fresh
      assertEq failed moved =<< drawn fresh AlignEnd x y

-- | A layout cache hit keeps the frame's own paint state, as a cold layout.
runLayoutPaintStateTest :: Context -> IORef Int -> IO ()
runLayoutPaintStateTest ctx failed = do
  let inp = withInputOff 400 300
      ui value color = column $ do
        box (fixedWH 30 30) color
        liftIO $ do
          let na = ctxNodeArena ctx
          i <- addNodeFromLayout na NodeSlider 0 (fixedWH 200 30 defaultLayout)
          setWidgetId na i (WidgetId 123)
          setNodeText na i ""
          setNodeValue na i value
        labelWith (fontColor color) "paint only"
      drawn value color = drawnBy ctx inp (ui value color)
      cacheName = readIORef (ctxLayoutCache ctx) >>= evaluate >>= makeStableName
  first <- drawn 0.2 (colorRGBA 255 0 0 255)
  cache <- cacheName
  changed <- drawn 0.8 (colorRGBA 0 0 255 255)
  assert failed . (cache ==) =<< cacheName
  assert failed (first /= changed)
  writeIORef (ctxLayoutCache ctx) Nothing
  assertEq failed changed =<< drawn 0.8 (colorRGBA 0 0 255 255)

-- | A full undamaged frame of unchanged view output returns the last draw
-- data itself; paint-state changes still draw. Container drawings, reuse
-- off, the layout overlay and clip frames always paint.
runDrawReuseTest :: Context -> IORef Int -> IO ()
runDrawReuseTest ctx failed = do
  let px = BS.replicate (4 * 4 * 4) 200
  assert failed . and =<< mapM (\i -> registerImage ctx (ImageId i) 4 4 px) [1, 2]
  let inp = withInputOff 400 300
      red = colorRGBA 255 0 0 255
      blue = colorRGBA 0 0 255 255
      ui (boxColor, value, fontCol, iid, opacity, stray) = column $ do
        box (fixedWH 30 30) boxColor
        void (snd <$> withValue value (slider 0 1))
        labelWith (fontColor fontCol) "paint only"
        imageConfigured defaultImageConfig {icLayout = fixedWH 20 20, icOpacity = opacity} (ImageId iid)
        liftIO $ do
          let na = ctxNodeArena ctx
          i <- addNodeFromLayout na NodeContainer 0 (fixedWH 40 40 defaultLayout)
          setWidgetId na i (WidgetId 777)
          when (stray > 0) $
            registerCustomDrawing ctx (WidgetId 777) 0 $ \_ r ->
              smallArrayFromList [FillRect r (colorRGBA stray 0 0 255)]
      base = (red, 0.2, red, 1, 1, 0)
      -- The case returns the field itself: a selector thunk from fmap would
      -- make two reads of one draw data unequal under 'ptrEq' unoptimised.
      frame s = runFrame ctx inp (ui s) >>= \(_, dd, _) -> pure dd
      -- Two frames of @s@ after @from@: the first draws @s@, and the second
      -- returns the first's draw data when @reused@.
      check from s reused = do
        before <- frame from >>= snapshotDraw
        d1 <- frame s
        d2 <- frame s
        assertEq failed reused (ptrEq d1 d2)
        now <- snapshotDraw d2
        assert failed (s == from || now /= before)
  replicateM_ 2 (frame base)
  forM_
    [ (base, base, True)
    , (base, (blue, 0.2, red, 1, 1, 0), True)
    , (base, (red, 0.8, red, 1, 1, 0), True)
    , (base, (red, 0.2, blue, 1, 1, 0), True)
    , (base, (red, 0.2, red, 2, 1, 0), True)
    , (base, (red, 0.2, red, 1, 0.5, 0), True)
    , (base, (red, 0.2, red, 1, 1, 100), False)
    , ((red, 0.2, red, 1, 1, 100), (red, 0.2, red, 1, 1, 200), False)
    ]
    $ \(from, s, reused) -> check from s reused
  forM_
    [ (setDrawReuse ctx False, setDrawReuse ctx True)
    , (setExplainLayout ctx True, setExplainLayout ctx False)
    , (writeIORef (ctxPaintFull ctx) False, writeIORef (ctxPaintFull ctx) True)
    ]
    $ \(off, on) -> off >> check base base False >> on
  check base base True

-- | In a continuous session not reading damage (as the SDL runner), each
-- frame draws what a never-reusing context draws: hovers, focus, animation
-- steps and wheel notches paint, settled frames reuse.
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
        spacer (Fixed 4) (Fixed (4 + 30 * g))
        void (buttonWith' (fixedWH 40 20) "b")
        void $ scrollWith (fixedWH 100 40) $ column $ forM_ [1 .. 10 :: Int] $ \i -> label (T.pack (show i))
        pure b
      frame c inp target = do
        writeIORef (ctxDamageWanted c) False
        (b, dd, _) <- runFrame c inp (ui target)
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

-- | A wrapping label measures again when only its container's width changes.
runPartialMeasureAncestorTest :: Context -> IORef Int -> IO ()
runPartialMeasureAncestorTest ctx failed = do
  let inp = withInputOff 400 300
      ui c w = liftIO $ do
        let na = ctxNodeArena c
        parent <- addNodeFromLayout na NodeContainer (-1) (fixedWH w 200 defaultLayout)
        i <- addNodeFromLayout na NodeText parent defaultLayout
        setNodeText na i "a long label that wraps onto several lines at a narrow width"
      rectsAt c w = runFrame c inp (ui c w) >> arenaRects c
  wide <- rectsAt ctx 360
  narrow <- rectsAt ctx 120
  assert failed (wide /= narrow)
  fresh <- newContext
  assertEq failed narrow =<< rectsAt fresh 120
