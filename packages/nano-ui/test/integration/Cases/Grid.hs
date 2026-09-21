module Cases.Grid
  ( runGridColumnsWithFontColorTest
  , runNestedGridTest
  , runStaleFontColorTest
  , runFontCompositionTest
  , runAlignBaselineTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.List (nub)
import qualified Data.Text as T
import NanoUI
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (arenaCount, getStyleIdx, getText)
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertJust, withInput)

spanOf :: T.Text -> [(Rect, T.Text, Color, Color, Rect)] -> Maybe (Rect, Color)
spanOf txt spans =
  case [(r, fg) | (r, t, fg, _, _) <- spans, t == txt] of
    (hit : _) -> Just hit
    [] -> Nothing

-- | Font colour and grid column count once shared a node slot, so a coloured
-- grid child set after 'gridWith' collapsed the grid to one column.
runGridColumnsWithFontColorTest :: Context -> IORef Int -> IO ()
runGridColumnsWithFontColorTest ctx failed = do
  let red = colorRGBA 255 0 0 255
      cells = ["c0", "c1", "c2", "c3"]
      ui =
        gridWith 4 (fontColor red . fillW) $
          mapM_ (void . labelWith (fontColor red)) cells
  _ <- runFrame ctx (withInput 800 200) ui
  spans <- collectTextSpans ctx
  let hits = [spanOf c spans | c <- cells]
      xs = [rectX r | Just (r, _) <- hits]
      ys = [rectY r | Just (r, _) <- hits]
  assertEq failed (length xs) 4
  assertEq failed (length (nub xs)) 4
  assertEq failed (length (nub ys)) 1
  assert failed (and [fg == red | Just (_, fg) <- hits])

-- | A grid child that is itself a grid reuses the solver scratch arrays; the
-- parent must still place its remaining cells in their own columns and rows.
runNestedGridTest :: Context -> IORef Int -> IO ()
runNestedGridTest ctx failed = do
  let inner tag = gridWith 2 fillW $ mapM_ (\i -> void (label (tag <> T.pack (show i)))) [0 .. 3 :: Int]
      ui =
        gridWith 2 fillW $ do
          inner "a"
          inner "b"
          void (label "tail0")
          void (label "tail1")
  _ <- runFrame ctx (withInput 800 400) ui
  spans <- collectTextSpans ctx
  case (spanOf "a0" spans, spanOf "b0" spans, spanOf "b3" spans, spanOf "tail0" spans, spanOf "tail1" spans) of
    (Just (a0, _), Just (b0, _), Just (b3, _), Just (t0, _), Just (t1, _)) -> do
      -- Outer columns: the second inner grid sits right of the first.
      assert failed (rectX b0 > rectX a0)
      assertEq failed (rectY b0) (rectY a0)
      -- The outer second row starts below both inner grids.
      assert failed (rectY t0 > rectY b3)
      assertEq failed (rectY t1) (rectY t0)
      -- Tail cells take the outer columns, not the inner grid's columns.
      assert failed (rectX t0 < rectX b0)
      assert failed (rectX t1 > rectX b0 - 8 && rectX t1 <= rectX b0)
    _ -> assert failed False

-- | Nodes created without a layout descriptor (widgets, popups, windows) must
-- not inherit the font colour of whatever node held their index last frame.
runStaleFontColorTest :: Context -> IORef Int -> IO ()
runStaleFontColorTest ctx failed = do
  let red = colorRGBA 255 0 0 255
      inp = withInput 400 200
  _ <- runFrame ctx inp (column (void (labelWith (fontColor red) "painted")))
  _ <- runFrame ctx inp (column (void (button "plain")))
  spans <- collectTextSpans ctx
  assertJust failed (spanOf "plain" spans) $ \(_, fg) -> assert failed (fg /= red)

-- | Font size, colour, weight, style and decoration modifiers compose on one
-- label, and the bold/italic/underline helpers set the same style bits.
runFontCompositionTest :: Context -> IORef Int -> IO ()
runFontCompositionTest ctx failed = do
  let inp = withInput 800 600
      customCol = colorRGBA 12 34 56 255
      ui = column $ do
        void $ label "Standard Text"
        void $ labelWith (fontSize 24.0 . fontBold . fontItalic . fontUnderline . fontColor customCol) "Composed"
        void $ labelWith fontStrike "Strike Text"
        bold "Bold Helper"
        italic "Italic Helper"
        underline "Underline Helper"
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  assertJust failed ((,) <$> spanOf "Standard Text" spans <*> spanOf "Composed" spans) $ \((std, stdFg), (r, fg)) -> do
      assertEq failed (rectH std) 16.0
      assertEq failed (rectH r) 24.0
      assertEq failed fg customCol
      assert failed (stdFg /= customCol)
  let na = ctxNodeArena ctx
  n <- arenaCount na
  styles <- mapM (\i -> (,) <$> getText na i <*> getStyleIdx na i) [0 .. n - 1]
  case mapM (`lookup` styles) ["Composed", "Strike Text", "Bold Helper", "Italic Helper", "Underline Helper"] of
    Just [composed, strike, b, i, u] -> do
      assertEq failed (textNodeFontWeight composed) WeightBold
      assertEq failed (textNodeFontStyle composed) FontStyleItalic
      assertEq failed (textNodeTextDecoration composed) DecorationUnderline
      assertEq failed (textNodeTextDecoration strike) DecorationStrikethrough
      assertEq failed (textNodeFontWeight b) WeightBold
      assertEq failed (textNodeFontStyle i) FontStyleItalic
      assertEq failed (textNodeTextDecoration u) DecorationUnderline
    _ -> assert failed False

-- | Labels of different sizes, a button and a padded column holding a label all
-- share one baseline on a baseline-aligned row: the button by its label and the
-- column by its first child, not its larger last one. The row is tall enough
-- to hold them, so what follows starts below all of them. The test font's
-- ascent is 0.8 of its line height.
runAlignBaselineTest :: Context -> IORef Int -> IO ()
runAlignBaselineTest ctx failed = do
  let ui = column $ do
        rowWith (tight . gap 8) $ do
          void $ labelWith (tight . alignBaseline . fontSize 32) "Big"
          void $ labelWith (tight . alignBaseline) "small"
          void $ buttonWith alignBaseline "Go"
          columnWith (tight . alignBaseline . padXY 0 5) $ do
            void $ label "nested"
            void $ labelWith (tight . fontSize 24) "second"
        void $ label "after"
  _ <- runFrame ctx (withInput 400 200) ui
  spans <- collectTextSpans ctx
  case mapM (`spanOf` spans) ["Big", "small", "Go", "nested", "second", "after"] of
    Just [(big, _), (small, _), (go, _), (nested, _), (second, _), (after, _)] -> do
      let baseline r = rectY r + 0.8 * rectH r
          near r = abs (baseline r - baseline big) < 0.5
      assert failed (near small)
      assert failed (near go)
      assert failed (near nested)
      assert failed (rectY small > rectY big)
      assert failed (rectY after >= maximum [rectY r + rectH r | r <- [big, small, go, second]])
    _ -> assert failed False
