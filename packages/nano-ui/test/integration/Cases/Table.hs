module Cases.Table (runTableSortTest, runTableReorderTest) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Vector qualified as V
import NanoUI
import NanoUI.Testing (Context, collectTextSpans, runFrame)
import NanoUI.Testing.Assert (assertEq)
import NanoUI.Testing.Harness
  ( clickPos
  , dragPos
  , findHeader
  , requireSpan
  , warmup2
  , withInputOff
  )

runTableSortTest :: Context -> IORef Int -> IO ()
runTableSortTest _ failed = do
  let
    columns = headed "Key" fst
    rows = [("b", 1), ("a", 2), ("b", 3), ("a", 4)] :: [(Text, Int)]
  assertEq
    failed
    [2, 4, 1, 3]
    (map snd (sortRows columns (SortCol 0 SortAsc) (V.fromList rows)))
  assertEq
    failed
    [1, 3, 2, 4]
    (map snd (sortRows columns (SortCol 0 SortDesc) rows))
  assertEq failed rows (sortRows mempty (SortCol 0 SortAsc) rows)
  assertEq failed rows (sortRows mempty (SortCol 0 SortDesc) rows)

runTableReorderTest :: Context -> IORef Int -> IO ()
runTableReorderTest ctx failed = do
  let
    input = withInputOff 500 240
    ui = simpleTable ["First", "Second", "Third"] (V.fromList [["a", "b", "c"], ["short"], []])
    draw inp = void (runFrame ctx inp ui)
    header name = do
      spans <- collectTextSpans ctx
      requireSpan "missing table header" (findHeader name spans)
  _ <- warmup2 ctx input ui
  first <- header "First"
  third <- header "Third"
  -- A click is not a reorder, even when its absolute x coordinate exceeds
  -- the drag threshold.
  clickPos draw input first
  clicked <- warmup2 ctx input ui
  assertEq failed [0, 1, 2] (tableColOrder clicked)
  dragPos draw input first third
  moved <- warmup2 ctx input ui
  assertEq failed [1, 0, 2] (tableColOrder moved)
  -- The released drag must not stay latched on subsequent frames.
  draw input {inputMousePos = first}
  settled <- warmup2 ctx input ui
  assertEq failed [1, 0, 2] (tableColOrder settled)
