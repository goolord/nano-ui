module Cases.Grid (tests) where

import Spec
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (arenaCount, getStyleIdx, getText)

tests :: [Spec]
tests =
  [ spec "grid-placement" runGridPlacementTest
  , spec "grid-heights" runGridHeightsTest
  , pixelSpec "font-styles" runFontStylesTest
  ]

red :: Color
red = colorRGBA 255 0 0 255

-- | Each named span's rect and colour, in order, once the view settles.
spansOf :: Context -> Input -> [T.Text] -> NanoUI a -> IO [Maybe (Rect, Color)]
spansOf ctx inp names ui = do
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  pure [listToMaybe [(r, fg) | (r, t, fg, _, _) <- spans, t == n] | n <- names]

-- | Cells keep their own columns with a font colour on the grid (the two
-- once shared a node slot) and around a nested grid (which reuses solver
-- scratch); an aligned fit panel keeps its height; grow children share
-- what a column's gaps leave.
runGridPlacementTest :: Context -> IORef Int -> IO ()
runGridPlacementTest ctx failed = do
  let cells = ["c0", "c1", "c2", "c3"]
      coloured = gridWith 4 (fontColor red . fillW) (mapM_ (labelWith (fontColor red)) cells)
  spansOf ctx (withInput 800 200) cells coloured >>= \hits -> do
    assertEq failed (map (\x -> Just (Rect x 3 24 12)) [3, 204, 404, 605]) (map (fmap fst) hits)
    assert failed (all ((== Just red) . fmap snd) hits)
  let inner tag = gridWith 2 fillW $ mapM_ (\i -> label (tag <> T.pack (show i))) [0 .. 3 :: Int]
      nested = gridWith 2 fillW (inner "a" >> inner "b" >> label "tail0" >> label "tail1")
  spansOf ctx (withInput 800 400) ["a0", "b0", "b3", "tail0", "tail1"] nested
    >>= assertEq failed (map Just [Rect 6 6 24 12, Rect 407 6 24 12, Rect 605 26 24 12, Rect 3 49 60 12, Rect 404 49 60 12])
      . map (fmap fst)
  -- The short cell, the tall one and the next row's.
  forM_ [(alignMid, 30), (alignBottom, 60)] $ \(align, y) -> do
    _ <- warmup2 ctx (withInput 400 400) . columnWith tight $ gridWith 2 (tight . gap 0) $ do
      panelWith align (label "a")
      panelWith id (replicateM_ 4 (label "x"))
      label "next"
    (\rs -> map (rs !!) [2, 4, 9]) <$> arenaRects ctx >>= assertEq failed [Rect 0 y 18 18, Rect 48 0 18 78, Rect 0 78 48 12]
  _ <- warmup2 ctx (withInput 800 400) (columnWith (gap 10 . fixedH 200 . fixedW 100) (replicateM_ 3 (box fillH red)))
  arenaRects ctx >>= assertEq failed [Rect 0 0 100 200, Rect 3 3 32 58, Rect 3 71 32 58, Rect 3 139 32 58]

-- | A grid, also with 'gridMinColW', is as tall as its rows, not its cells
-- stacked. A wrapped cell sets its row's height, and a label wider than its
-- 2D scroller is as tall as paint draws it.
runGridHeightsTest :: Context -> IORef Int -> IO ()
runGridHeightsTest ctx failed = do
  let adaptive = columnWith (gridMinColW 140 . fillW) (replicateM_ 7 (button "x"))
      -- Cell count, columns at least, rows, and room left under the cells;
      -- four or five 140-wide adaptive columns fit inside the padding.
      wrappers =
        [ (400, 9, 3, 3, 30, column (gridWith 3 fillW (replicateM_ 9 (button "x")) >> label "after"))
        , (760, 7, 4, 2, 40, columnWith fillW (adaptive >> label "after"))
        , (760, 7, 4, 2, 40, panelWith (padAll 12 . gap 8 . fillW) (adaptive >> label "after"))
        , ( 760, 7, 4, 2, 40, scrollWith (padAll 12 . grow) $ columnWith (tight . gap 20 . fillW) $
              columnWith (tight . gap 6 . fillW) (panelWith (padAll 12 . gap 8 . fillW) adaptive >> label "after") )
        ]
  forM_ wrappers $ \(w, n, minCols, rows, margin, ui) -> do
    _ <- warmup2 ctx (withInput w 600) ui
    spans <- collectTextSpans ctx
    let xs = [r | (r, "x", _, _, _) <- spans]
    assertEq failed n (length xs)
    assert failed (length (nub (map rectX xs)) >= minCols)
    assertEq failed rows (length (nub (map rectY xs)))
    assertJust failed (spanRect "after" spans) $ \after ->
      assert failed (rectY after < maximum [rectY r + rectH r | r <- xs] + margin)
  let text = "words that wrap across several lines in a narrow cell"
      cellGrid width = columnWith tight $ do
        gridWith 2 (tight . fixedW width . gap 1) $
          mapM_ (labelWith (tight . fillW)) ["short", text, "next"]
        labelWith tight "after"
  forM_ [240, 600, 180, 240, 240] $ \width -> do
    _ <- runFrame ctx (withInput 800 1200) (cellGrid width)
    arenaRects ctx >>= \case
      [_, g, short, wrapped, next, after] -> do
        assert failed (rectH wrapped > rectH after && rectY short == rectY wrapped)
        assert failed (rectY next >= rectY wrapped + rectH wrapped && rectY after >= rectY next + rectH next)
        assertEq failed (rectY g + rectH g) (rectY next + rectH next)
        assertPaintedLines failed ctx ["short", "next", "after"] wrapped
      rs -> assertEq failed 6 (length rs)
  _ <- warmup2 ctx (withInput 1000 400) . scrollArea2D (fixedWH 300 200) $ do
    spacer (Fixed 800) (Fixed 20)
    labelWith fillW "a label wider than its scroller but not its content"
    label "after"
  arenaRects ctx >>= \case
    [_, _, wide, _] -> assert failed (rectW wide > 300) >> assertPaintedLines failed ctx ["after"] wide
    rs -> assertEq failed 4 (length rs)

-- | No stale font colour from last frame's node; font modifiers compose on
-- a label as the helpers do; a baseline row lines up labels, a button and a
-- column's first child (the test font's ascent is 0.8 of its line).
runFontStylesTest :: Context -> IORef Int -> IO ()
runFontStylesTest ctx failed = do
  let inp = withInput 800 600
  _ <- runFrame ctx inp (column (labelWith (fontColor red) "painted"))
  spansOf ctx inp ["plain"] (column (button "plain")) >>= assertEq failed [Just True] . map (fmap ((/= red) . snd))
  let custom = colorRGBA 12 34 56 255
      composed = column $ do
        label "Standard Text"
        labelWith (fontSize 24.0 . fontBold . fontItalic . fontUnderline . fontColor custom) "Composed"
        labelWith fontStrike "Strike Text"
        bold "Bold Helper"
        italic "Italic Helper"
        underline "Underline Helper"
  spansOf ctx inp ["Standard Text", "Composed"] composed >>= \case
    [Just (std, stdFg), Just (r, fg)] -> assertEq failed (16, 24, custom, True) (rectH std, rectH r, fg, stdFg /= custom)
    other -> assertEq failed 2 (length other)
  let na = ctxNodeArena ctx
  n <- arenaCount na
  styles <- mapM (\i -> (,) <$> getText na i <*> getStyleIdx na i) [0 .. n - 1]
  assertEq failed (Just [WeightBold, WeightBold]) (map textNodeFontWeight <$> mapM (`lookup` styles) ["Composed", "Bold Helper"])
  assertEq failed (Just [FontStyleItalic, FontStyleItalic]) (map textNodeFontStyle <$> mapM (`lookup` styles) ["Composed", "Italic Helper"])
  assertEq failed (Just [DecorationUnderline, DecorationStrikethrough, DecorationUnderline]) $
    map textNodeTextDecoration <$> mapM (`lookup` styles) ["Composed", "Strike Text", "Underline Helper"]
  let baselined = column $ do
        rowWith (tight . gap 8) $ do
          labelWith (tight . alignBaseline . fontSize 32) "Big"
          labelWith (tight . alignBaseline) "small"
          void (buttonWith alignBaseline "Go")
          columnWith (tight . alignBaseline . padXY 0 5) (label "nested" >> labelWith (tight . fontSize 24) "second")
        label "after"
  spansOf ctx (withInput 400 200) ["Big", "small", "Go", "nested", "second", "after"] baselined >>= \hits ->
    case map fst <$> sequence hits of
      Just [big, small, go, nested, second, after] -> do
        let baseline r = rectY r + 0.8 * rectH r
        assert failed (all (\r -> abs (baseline r - baseline big) < 0.5) [small, go, nested] && rectY small > rectY big)
        assert failed (rectY after >= maximum [rectY r + rectH r | r <- [big, small, go, second]])
      _ -> assert failed False
