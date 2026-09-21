module Cases.Cache (tests) where

import Spec
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena
  ( NodeType (..), addNodeFromLayout, getRect, setNodeText, setNodeValue
  , setStyleIdx, setWidgetId
  )
import System.Mem.StableName (makeStableName)

tests :: [Spec]
tests =
  [ spec "metric-cache-invalidation" runMetricCacheInvalidationTest
  , spec "widget-placement-cache" runWidgetPlacementCacheTest
  , spec "layout-cache-paint-state" runLayoutPaintStateTest
  ]

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
        (_, _, w, _) <- getRect (ctxNodeArena c) 0
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
      expected <- snapshotDraw coldDraw
      assertEq failed actual expected
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
