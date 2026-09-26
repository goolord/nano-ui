module Cases.Cache (tests) where

import Spec
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena
  ( NodeType (..), addNodeFromLayout, getNodeRect, setNodeText
  , setNodeValue, setStyleIdx, setWidgetId
  )
import System.Mem.StableName (makeStableName)

tests :: [Spec]
tests =
  [ spec "metric-cache-invalidation" runMetricCacheInvalidationTest
  , spec "widget-placement-cache" runWidgetPlacementCacheTest
  , spec "layout-cache-paint-state" runLayoutPaintStateTest
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
