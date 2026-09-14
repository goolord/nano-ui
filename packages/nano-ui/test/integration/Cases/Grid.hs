module Cases.Grid
  ( runGridColumnsWithFontColorTest
  , runNestedGridTest
  , runStaleFontColorTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.List (nub)
import qualified Data.Text as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)

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
  case spanOf "plain" spans of
    Just (_, fg) -> assert failed (fg /= red)
    Nothing -> assert failed False
