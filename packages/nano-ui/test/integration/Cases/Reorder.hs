-- | 'useReorder': a drag past the threshold moves an item, with a live
-- preview, over wrapped rows too, and a click moves nothing.
module Cases.Reorder (tests) where

import Spec

tests :: [Spec]
tests =
  [ spec "reorder-drag" runReorderDragTest
  , spec "reorder-click" runReorderClickTest
  , spec "reorder-wrapped" runReorderWrappedTest
  , spec "reorder-preview-stable" runReorderPreviewStableTest
  ]

-- | Five 40 by 30 slots in a row, 50 apart.
slotsInRow :: [Rect]
slotsInRow = [Rect (fromIntegral i * 50) 0 40 30 | i <- [0 .. 4 :: Int]]

-- | Run @frames@ against a fixed order and rects, returning each result.
drive :: Context -> [Int] -> [(Int, Rect)] -> [Input] -> IO [Reorder]
drive ctx order items = mapM (\i -> evalFrame i)
 where
  evalFrame i = (\(r, _, _, _) -> r) <$> runFrame ctx i (useReorder order items)

runReorderDragTest :: Context -> IORef Int -> IO ()
runReorderDragTest ctx failed = do
  let
    inp = withInput 400 200
    order = [0 .. 4]
    press = pressAt inp (V2 20 15)
    nudge = holdAt press (V2 23 15)
    far = holdAt press (V2 230 15)
  rs <-
    drive
      ctx
      order
      (zip order slotsInRow)
      [inp, press, nudge, far, releaseAt far, inp]
  case rs of
    [_, pressed, nudged, dragged, dropped, after] -> do
      assertEq failed (Just 0) (reorderDragging pressed)
      -- Under the threshold: no preview yet.
      assertEq failed (False, order) (reorderMoved nudged, reorderPreview nudged)
      assertEq
        failed
        (True, [1, 2, 3, 4, 0], order)
        (reorderMoved dragged, reorderPreview dragged, reorderOrder dragged)
      assertEq
        failed
        (True, [1, 2, 3, 4, 0])
        (reorderMoved dropped, reorderOrder dropped)
      assertEq failed (False, Nothing) (reorderMoved after, reorderDragging after)
    _ -> assertEq failed (length rs) 6

-- | A press and release in place is a click: nothing moves.
runReorderClickTest :: Context -> IORef Int -> IO ()
runReorderClickTest ctx failed = do
  let
    inp = withInput 400 200
    order = [0 .. 4]
    press = pressAt inp (V2 120 15)
  rs <- drive ctx order (zip order slotsInRow) [inp, press, releaseAt press]
  case rs of
    [_, _, released] ->
      assertEq failed (False, order) (reorderMoved released, reorderOrder released)
    _ -> assertEq failed (length rs) 3

-- | Items wrapped over two rows: the item takes the place of the one nearest
-- the pointer on the second row, even in the gap beside it.
runReorderWrappedTest :: Context -> IORef Int -> IO ()
runReorderWrappedTest ctx failed = do
  let
    inp = withInput 400 200
    order = [0 .. 4]
    rects =
      [ Rect 0 0 40 30
      , Rect 50 0 40 30
      , Rect 100 0 40 30
      , Rect 0 40 40 30
      , Rect 50 40 40 30
      ]
    press = pressAt inp (V2 20 15)
    -- In the gap right of item 3, nearer it than item 4.
    over = holdAt press (V2 43 55)
  rs <- drive ctx order (zip order rects) [inp, press, over, releaseAt over]
  case rs of
    [_, _, _, dropped] -> assertEq failed [1, 2, 3, 0, 4] (reorderOrder dropped)
    _ -> assertEq failed (length rs) 4

-- | Drawn from the preview, the dragged item sits in the slot it is over,
-- so the target stays put frame after frame.
runReorderPreviewStableTest :: Context -> IORef Int -> IO ()
runReorderPreviewStableTest ctx failed = do
  shown <- newIORef [0 .. 4 :: Int]
  let
    inp = withInput 400 200
    ui = do
      drawn <- liftIO (readIORef shown)
      r <- useReorder [0 .. 4] (zip drawn slotsInRow)
      liftIO (writeIORef shown (reorderPreview r))
      pure r
    press = pressAt inp (V2 20 15)
    over = holdAt press (V2 170 15)
    frame i = (\(r, _, _, _) -> reorderPreview r) <$> runFrame ctx i ui
  _ <- frame inp
  _ <- frame press
  previews <- mapM frame [over, over, over]
  assertEq failed (replicate 3 [1, 2, 3, 0, 4]) previews
