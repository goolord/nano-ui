-- | 'useReorder': a drag past the threshold moves an item, with a live
-- preview, over wrapped rows too, and a click moves nothing.
module Cases.Reorder (tests) where

import Spec
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "reorder-drag" runReorderDragTest
  , spec "reorder-preview-stable" runReorderPreviewStableTest
  , spec "reorder-stale-drag" runReorderStaleDragTest
  , spec "reorder-owned-press" runReorderOwnedPressTest
  , spec "insertion-index-bounds" runInsertionIndexTest
  , spec "drag-idle-and-abort" runDragIdleAndAbortTest
  ]

-- | Five 40 by 30 slots in a row, 50 apart.
slotsInRow :: [Rect]
slotsInRow = [Rect (fromIntegral i * 50) 0 40 30 | i <- [0 .. 4 :: Int]]

-- Synthetic owned responses isolate the reorder math from widget layout.
responses :: Input -> [(Int, Rect)] -> [(Int, Response)]
responses inp = map $ \(i, rect) -> (i, mempty
  { rawRespRect = rect
  , rawRespHeld = if heldIn MouseLeft inp && rectContains rect (inputMousePos inp)
      then buttonsFromList [MouseLeft] else noButtons
  })

-- | 'useReorder' over fixed rects on a fresh context, for an idle frame, a
-- press at @from@, a hold at each of @path@, the release and an idle frame.
dragThrough :: [Int] -> [Rect] -> V2 -> [V2] -> IO [Reorder]
dragThrough order rects from path = do
  ctx <- newContext
  let inp = withInput 400 200
      down = pressAt inp from
      holds = map (holdAt down) path
      frames = [inp, down] ++ holds ++ [releaseAt (last (down : holds)), inp]
  mapM (\i -> evalUi ctx i (useReorder order (responses i (zip order rects)))) frames

runReorderDragTest :: Context -> IORef Int -> IO ()
runReorderDragTest _ failed = do
  let order = [0 .. 4]
      moved = [1, 2, 3, 4, 0]
  [_, pressed, nudged, dragged, dropped, after] <- dragThrough order slotsInRow (V2 20 15) [V2 23 15, V2 230 15]
  assertEq failed (Just 0) (reorderDragging pressed)
  -- Under the threshold: no preview yet.
  assertEq failed (False, order) (reorderMoved nudged, reorderPreview nudged)
  assertEq failed (True, moved, order) (reorderMoved dragged, reorderPreview dragged, reorderOrder dragged)
  assertEq failed (True, moved) (reorderMoved dropped, reorderOrder dropped)
  assertEq failed (False, Nothing) (reorderMoved after, reorderDragging after)
  -- Item ids can be any Int: negative ones drag like the rest.
  [_, _, _, negDropped, _] <- dragThrough [-1, -2, 0, 3, -7] slotsInRow (V2 20 15) [V2 230 15]
  assertEq failed [-2, 0, 3, -7, -1] (reorderOrder negDropped)
  -- A press and release in place is a click: nothing moves.
  [_, _, clicked, _] <- dragThrough order slotsInRow (V2 120 15) []
  assertEq failed (False, order) (reorderMoved clicked, reorderOrder clicked)
  -- Wrapped over two rows: the item takes the place of the one nearest the
  -- pointer on the second row, even in the gap right of it.
  let wrapped = take 3 slotsInRow ++ [Rect 0 40 40 30, Rect 50 40 40 30]
  [_, _, _, wrapDropped, _] <- dragThrough order wrapped (V2 20 15) [V2 43 55]
  assertEq failed [1, 2, 3, 0, 4] (reorderOrder wrapDropped)

-- | Drawn from the preview, the dragged item sits in the slot it is over,
-- so the target stays put frame after frame.
runReorderPreviewStableTest :: Context -> IORef Int -> IO ()
runReorderPreviewStableTest ctx failed = do
  shown <- newIORef [0 .. 4 :: Int]
  let ui = do
        drawn <- liftIO (readIORef shown)
        input <- askInput
        r <- useReorder [0 .. 4] (responses input (zip drawn slotsInRow))
        liftIO (writeIORef shown (reorderPreview r))
        pure r
  runSim ctx failed (withInput 400 200) ui $ do
    _ <- press (At (V2 20 15))
    previews <- replicateM 3 (reorderPreview <$> moveTo (At (V2 170 15)))
    checkEq (replicate 3 [1, 2, 3, 0, 4]) previews

-- | A drag released while the hook did not run is over: a press elsewhere,
-- still held when the hook runs again, carries no item and drops none.
runReorderStaleDragTest :: Context -> IORef Int -> IO ()
runReorderStaleDragTest ctx failed = do
  let inp = withInput 400 200
      order = [0 .. 4]
      reorderAt i = evalUi ctx i (useReorder order (responses i (zip order slotsInRow)))
      skip i = evalUi ctx i (pure ())
      down = pressAt inp (V2 20 15)
      far = holdAt down (V2 230 15)
      elsewhere = pressAt inp (V2 300 150)
      later = holdAt elsewhere (V2 230 160)
  mapM_ reorderAt [inp, down]
  assertEq failed (Just 0) . reorderDragging =<< reorderAt far
  mapM_ skip [releaseAt far, elsewhere]
  assertEq failed Nothing . reorderDragging =<< reorderAt later
  assertEq failed order . reorderOrder =<< reorderAt (releaseAt later)

-- | Covered and clipped controls cannot start a reorder gesture.
runReorderOwnedPressTest :: Context -> IORef Int -> IO ()
runReorderOwnedPressTest _ failed = do
  let contents = columnWith tight $ do
        items <- rowWith (tight . gap 10) $
          forM [0, 1 :: Int] $ \i -> (i,) <$> buttonWith' (fixedWH 40 30) "Item"
        useReorder [0, 1] items
      covered = layersWith (fixedWH 100 50) $ do
        r <- contents
        panelWith (pointer PointerBlock . fixedWH 40 30) (pure ())
        pure r
      clipped = scrollWith (fixedWH 40 30) contents
  forM_ [(covered, V2 20 15), (clipped, V2 70 15)] $ \(ui, pos) -> do
    ctx <- newContext
    runSim ctx failed (withInputOff 300 100) ui $ do
      checkEq Nothing . reorderDragging =<< press (At pos)
      _ <- moveTo (At (V2 90 15))
      checkEq [0, 1] . reorderOrder =<< release

runInsertionIndexTest :: Context -> IORef Int -> IO ()
runInsertionIndexTest _ failed = do
  let bounds = Rect 0 0 250 30
      hit = insertionIndex DragAxisX bounds slotsInRow
  assertEq failed [Just 0, Just 2, Just 5] (map hit [V2 1 15, V2 100 15, V2 249 15])
  forM_ [V2 (-1) 15, V2 251 15, V2 20 (-1), V2 20 31] $ \p ->
    assertEq failed Nothing (hit p)
  assertEq failed (Just 0) (insertionIndex DragAxisX bounds [] (V2 10 10))
  let column2 = [Rect 0 0 30 40, Rect 0 50 30 40]
  assertEq failed (Just 1) (insertionIndex DragAxisY (Rect 0 0 30 100) column2 (V2 15 55))
  assertEq failed (Just 1) (insertionIndex DragAxisX (Rect 50 0 100 30) slotsInRow (V2 51 15))

-- | A drag whose pointer leaves for nowhere ends, and one held still asks for
-- no more frames.
runDragIdleAndAbortTest :: Context -> IORef Int -> IO ()
runDragIdleAndAbortTest ctx failed = do
  dragHandle <- newDrag
  let inp = withInput 400 200
      ui = useDrag dragHandle [(1 :: Int, mempty {rawRespHeld = buttonsFromList [MouseLeft]})]
      down = pressAt inp (V2 10 10)
      moved = holdAt down (V2 40 10)
  _ <- evalUi ctx down ui
  assertEq failed Nothing =<< evalUi ctx (inp {inputMousePos = V2 (-10000) (-10000)}) ui
  mapM_ (\i -> runFrame ctx i ui) [down, moved, moved]
  (gesture, _, dirty) <- runFrame ctx moved ui
  assertEq failed (Just Dragging) (dragPhase <$> gesture)
  assert failed (not dirty)
