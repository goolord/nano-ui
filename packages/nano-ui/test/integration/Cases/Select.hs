{-# OPTIONS_GHC -O0 #-}
module Cases.Select (tests) where

import Spec
import Data.Bifunctor (first)
import Data.Primitive.SmallArray qualified as SA
import Data.Text qualified as T
import NanoUI.Internal.Store (anySelectOpen)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ pixelSimSpec "slider-cursor" 300 80 (column (withValue 50 (slider' 0 100))) sliderCursor
  , spec "select-mouse-and-keys" runSelectMouseAndKeysTest
  , simSpec "select-dropdown-paint" 640 480 (column (withValue 0 (select' lowHigh))) selectDropdownPaint
  , spec "select-dropdown-placement" runSelectDropdownPlacementTest
  , spec "select-dropdown-scrolled-away" runSelectDropdownScrolledAwayTest
  , spec "select-keys-after-focus-moves" runSelectKeysAfterFocusMovesTest
  , spec "select-dropdown-clips-rows" runSelectDropdownClipsRowsTest
  , spec "choices-unreached-index" runChoicesUnreachedIndexTest
  , spec "tree-mouse-and-keys" runTreeTest
  , spec "tree-late-items-expanded" runTreeLateItemsExpandedTest
  , spec "radio-groups" runRadioGroupsTest
  , simSpec "sliders-drag-apart" 300 120 (column ((,) <$> twenty <*> twenty)) slidersDragApart
  , spec "slider-bad-values-settle" runSliderBadValuesTest
  , spec "slider-stale-drag" (runStaleDragTest (slider' 0 100) (40, 0))
  , spec "knob-stale-drag" (runStaleDragTest (knob' 0 100) (0, -30))
  ]

twenty :: NanoUI (Response, Float)
twenty = withValue 20 (slider' 0 100)

lowHigh :: [T.Text]
lowHigh = ["Low", "Medium", "High"]

selectOpen :: Sim a Bool
selectOpen = simContext >>= \ctx -> liftIO (anySelectOpen <$> getStore ctx)

-- | A slider's track shows the grab cursor, and grabbing while pressed, even
-- dragged off it; with the button up off it, the default.
sliderCursor :: Sim (Response, Float) ()
sliderCursor = do
  Rect rx ry rw rh <- respRect . fst <$> result
  checkEq UiCursorGrab =<< cursorAt (Within (sliderTrackBounds rx ry rw rh))
  _ <- press (Within (sliderTrackBounds rx ry rw rh))
  checkEq UiCursorGrabbing =<< pointerCursor
  checkEq UiCursorGrabbing =<< cursorAt (At (V2 (rx + rw + 20) (ry + rh + 20)))
  checkEq UiCursorDefault =<< (release >> frame >> pointerCursor)

-- | A select opens on the press, and a release on the row it was dragged to
-- picks that row. Open, the arrows move the pick and Escape closes it, and a
-- click on its own field closes it and keeps focus. Closed and focused, the
-- arrows step without opening it, Home and Page Up pick the first option, End
-- and Page Down the last, and Space or Enter opens and closes it. Rows show
-- the pointer cursor, hovered and pressed, and a pick reports one change.
runSelectMouseAndKeysTest :: Context -> IORef Int -> IO ()
runSelectMouseAndKeysTest ctx failed = do
  ref <- newVar 1
  runSim ctx failed (withInput 320 200) (column (held ref (select' (SA.smallArrayFromList lowHigh)))) $ do
    (resp, _) <- result
    let value = liftIO (readVarIO ref)
        -- The value lands on the frame after the key.
        valuesAfter = mapM (\k -> pressKey k >> frame >> value)
    _ <- press (On resp)
    expect "High"
    checkEq UiCursorPointer =<< cursorAt "Low"
    checkEq (0, False) =<< (release >> (,) <$> value <*> selectOpen)
    _ <- click (On resp)
    checkEq [1, 0] =<< valuesAfter [KeyDown, KeyUp]
    let openAfter = mapM (>> selectOpen)
    checkEq [False, True, False] =<< openAfter [pressKey KeyEscape, click (On resp), click (On resp)]
    checkEq (respId resp) =<< liftIO (getFocusId ctx)
    checkEq [1, 0, 2, 0, 2, 0] =<< valuesAfter [KeyRight, KeyLeft, KeyEnd, KeyHome, KeyPageDown, KeyPageUp]
    check . not =<< selectOpen
    checkEq [True, False, True, False] =<< openAfter (map pressKey [KeySpace, KeySpace, KeyEnter, KeyEnter])
    _ <- click (On resp)
    checkEq UiCursorPointer =<< cursorAt "High"
    (pressed, _) <- press "High"
    checkEq UiCursorPointer =<< pointerCursor
    rest <- sequence [release, frame, frame, frame]
    checkEq [2, 2, 2, 2] (map snd rest)
    checkEq 1 (length (filter respChanged (pressed : map fst rest)))
    checkEq [False, False, False] (map (respChanged . fst) (drop 1 rest))

-- | An open dropdown's spans carry the colours it paints, the picked row in
-- the accent colour. A hover moving inside it, which the arena does not hold,
-- repaints the dropdown as a clip, not the window.
selectDropdownPaint :: Sim (Response, Int) ()
selectDropdownPaint = do
  ctx <- simContext
  theme <- liftIO (getTheme ctx)
  (resp, _) <- click . On . fst =<< result
  modifyInput (\i -> i {inputDeltaTime = 1})
  _ <- moveTo (At (V2 600 400))
  overlays <- liftIO . collectOverlayTextSpans ctx =<< getInput
  let fgOf lbl = [fg | (_, txt, fg, _, _) <- overlays, txt == lbl]
  checkEq [themeAccent theme] (fgOf "Low")
  check (length (fgOf "Medium") == 1 && themeAccent theme `notElem` fgOf "Medium")
  high <- rectOf "High"
  let overHigh i = i {inputMousePos = V2 (v2X (centerOf resp)) (rectY high + 0.5)}
  check =<< liftIO . (\i -> needsRedraw ctx i (overHigh i)) =<< getInput
  _ <- liftIO (takeDamage ctx) >> frameWith overHigh
  dmg <- liftIO (takeDamage ctx)
  forM_ overlays $ \(_, _, _, _, dropRect) -> check (clipCovers dmg dropRect)

-- | A select near the window's bottom opens its dropdown above itself, where
-- it fits, and a click there picks the row under it. One that fits on
-- neither side is not slid back over the field, so the click that opens it
-- leaves the value alone.
runSelectDropdownPlacementTest :: Context -> IORef Int -> IO ()
runSelectDropdownPlacementTest _ failed = do
  let below n opts = column (replicateM_ 4 (label "line") >> withValue n (select' opts))
      tenOpts = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"]
  flips <- newContext
  runSim flips failed (withInput 320 140) (below 1 lowHigh) $ do
    (resp, _) <- click . On . fst =<< result
    low <- rectOf "Low"
    check (rectY low + rectH low <= rectY (respRect resp))
    checkEq 0 . snd =<< click (Within low)
  tall <- newContext
  runSim tall failed (withInput 320 300) (below 3 tenOpts) $ do
    checkEq 3 . snd =<< click . On . fst =<< result
    check =<< selectOpen

-- | A dropdown goes with its field when the field scrolls out of view: it is
-- not drawn, and a release where it would be picks nothing.
runSelectDropdownScrolledAwayTest :: Context -> IORef Int -> IO ()
runSelectDropdownScrolledAwayTest ctx failed = do
  let field = withValue 0 (selectWith' (fixedW 100) lowHigh) <* replicateM_ 30 (label "line")
  runSim ctx failed (withInput 320 300) (column (label "toolbar" >> scrollArea (fixedWH 300 120) field)) $ do
    (sid, (resp, _)) <- result
    _ <- click (On resp)
    medium <- rectOf "Medium"
    -- The wheel over the scroller, beside the dropdown.
    _ <- wheel (At (V2 250 (rectY (respRect resp) + 10))) (V2 0 5) >> replicateM_ 3 frame
    expectNot "Medium"
    -- Pressed open and held while its field scrolls just past the top, the
    -- dropdown would lie in the window: a release on its middle row there
    -- picks nothing.
    let past = rectH (respRect resp) + 20
        V2 cx cy = spanCenter medium
    liftIO (setScrollOffset ctx sid 0)
    _ <- click (At (V2 250 280)) >> press (On resp)
    check =<< selectOpen
    liftIO (setScrollOffset ctx sid past)
    _ <- moveTo (At (V2 cx (cy - past)))
    checkEq 0 . snd . snd =<< release

-- | Keys reach an open select only while it has focus: a right-click on a
-- text field closes it, and a field that takes focus by 'holdFocus' keeps
-- the arrows to itself. A modal opened over it closes it too, as the modal
-- hides it, so Escape then reaches the modal.
runSelectKeysAfterFocusMovesTest :: Context -> IORef Int -> IO ()
runSelectKeysAfterFocusMovesTest ctx failed = do
  grabRef <- newIORef False
  dialog <- newVar False
  -- The field above the select, clear of its dropdown.
  let ui = column $ do
        (t, _) <- withValue "text" textInput'
        (s, v) <- withValue 0 (select' lowHigh)
        whenM (liftIO (readIORef grabRef)) (holdFocus (respId t))
        modal dialog "Title" (label "body")
        pure (s, t, v)
      valueAfterDown = (\(_, _, v) -> v) <$> (pressKey KeyDown >> frame)
  runSim ctx failed (withInput 320 300) ui $ do
    (s, t, _) <- result
    check =<< (click (On s) >> selectOpen)
    liftIO (writeVarIO dialog True)
    check . not =<< (frame >> selectOpen)
    _ <- pressKey KeyEscape
    checkEq False =<< liftIO (readVarIO dialog)
    _ <- click (On s) >> rightClick (On t)
    check . not =<< selectOpen
    checkEq 0 =<< valueAfterDown
    _ <- pressKey KeyEscape >> click (On s)
    liftIO (writeIORef grabRef True)
    checkEq (respId t) =<< (frame >> liftIO (getFocusId ctx))
    checkEq 0 =<< valueAfterDown

-- | A select's dropdown is as wide as the select, and an option wider than
-- that is clipped to the dropdown rather than drawn over the page, where
-- closing the dropdown would not repaint it.
runSelectDropdownClipsRowsTest :: Context -> IORef Int -> IO ()
runSelectDropdownClipsRowsTest ctx failed = do
  let long = "A rather long option label"
      ui = column (withValue 0 (selectWith' (fixedW 80) [long, "Short"]))
      inp0 = (withInput 640 480) {inputMousePos = V2 600 400}
  (resp, _) <- warmup2 ctx inp0 ui
  let away = (snd (clickPair inp0 (centerOf resp))) {inputButtonsReleased = noButtons}
  _ <- runClick ctx inp0 ui (centerOf resp)
  (_, dd, _) <- runFrame ctx away ui
  overlays <- collectOverlayTextSpans ctx away
  case [(r, dropRect) | (r, txt, _, _, dropRect) <- overlays, txt == long] of
    (r, dropRect) : _ -> do
      assertGt failed (rectX r + rectW r) (rectX dropRect + rectW dropRect)
      let clipOf c = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)
          overlayCmds = [c | c <- drawCmdElems dd, cmdLayer c == LayerOverlay, cmdIndexCount c > 0]
      assert failed (any ((rectInflate 1 dropRect `covers`) . clipOf) overlayCmds)
    [] -> assert failed False

-- | A select, a radio group and a tree given an index their options do not
-- reach show it clamped but leave the caller's index alone, so a choice
-- restored before the options load is still there when they do. Picking the
-- option shown is still a pick, by a click or, on the select, by End, which
-- reports the change.
runChoicesUnreachedIndexTest :: Context -> IORef Int -> IO ()
runChoicesUnreachedIndexTest ctx failed = do
  loaded <- newIORef False
  refs@[selRef, radioRef, treeRef] <- mapM newVar [2, 2, 2]
  let ui = do
        shown <- liftIO (readIORef loaded)
        let opts os = if shown then os else []
        column $ do
          s <- select' (opts lowHigh) selRef
          radio (opts ["One", "Two", "Three"]) radioRef
          tree "t" [TreeItem o [] | o <- opts ["Red", "Green", "Blue"]] treeRef
          pure s
      -- The values, and the marks of the radio's options and the tree's rows.
      state = liftIO ((,) <$> mapM readVarIO refs <*> buttonValues ctx)
  runSim ctx failed (withInput 320 400) ui $ do
    checkEq [2, 2, 2] . fst =<< state
    liftIO (writeIORef loaded True)
    checkEq ([2, 2, 2], [0, 0, 1, 0, 0, 1]) =<< (settle >> state)
    liftIO (mapM_ (`writeVarIO` 5) refs)
    checkEq ([5, 5, 5], [0, 0, 1, 0, 0, 1]) =<< (settle >> state)
    _ <- click "Three" >> click "Blue" >> (click . On =<< result) >> click "High"
    checkEq [2, 2, 2] . fst =<< state
    liftIO (writeVarIO selRef 5)
    focusOn . respId =<< settle
    check . respChanged =<< (pressKey KeyEnd >> frame)
    checkEq 2 =<< liftIO (readVarIO selRef)

-- | A tree draws a clicked row selected on the click's frame, though its rows
-- were added before the click was read. The arrows move the selection, Left
-- goes to the parent, and Enter collapses it.
runTreeTest :: Context -> IORef Int -> IO ()
runTreeTest ctx failed = do
  ref <- newVar 0
  let items = SA.smallArrayFromList [TreeItem "root" [TreeItem "child" []], TreeItem "leaf" []]
      value = liftIO (readVarIO ref)
  runSim ctx failed (withInput 200 120) (column (held ref (tree' "k" items))) $ do
    _ <- click "child"
    checkEq (1, [0, 1, 0]) =<< ((,) <$> value <*> liftIO (buttonValues ctx))
    -- Tab focuses the first row; the arrows move focus and selection.
    pressTab 1
    checkEq [1, 0, 1, 0] =<< mapM (\k -> pressKey k >> value) [KeyDown, KeyUp, KeyDown, KeyLeft]
    _ <- pressKey KeyEnter >> frame
    expectNot "child"
    checkEq 2 =<< (pressKey KeyDown >> value)

-- | A tree whose parents arrive after its first frame, empty or holding a
-- placeholder leaf, opens every parent, as one built with them does.
runTreeLateItemsExpandedTest :: Context -> IORef Int -> IO ()
runTreeLateItemsExpandedTest ctx failed = do
  itemsRef <- newIORef []
  let ui = liftIO (readIORef itemsRef) >>= \items -> column (withValue 0 (tree' "late" items))
  runSim ctx failed (withInput 200 120) ui $ do
    forM_ [[TreeItem "Loading" []], [TreeItem "root" [TreeItem "child" []]]] $ \items ->
      liftIO (writeIORef itemsRef items) >> settle
    expect "child"

-- | Radio groups side by side keep separate selections, a steady frame asks
-- for nothing, and a click moves a mark on the frame it lands.
runRadioGroupsTest :: Context -> IORef Int -> IO ()
runRadioGroupsTest ctx failed = do
  aRef <- newVar (0 :: Int)
  bRef <- newVar 2
  let ui = column ((,) <$> held aRef (radio' ["x", "y"]) <*> held bRef (radio' ["p", "q", "r"]))
  runSim ctx failed (withInput 200 240) ui $ do
    check . not =<< wantsFrame
    checkEq [1, 0, 0, 0, 1] =<< liftIO (buttonValues ctx)
    _ <- click "q"
    checkEq ([1, 0, 0, 1, 0], [0, 1]) =<< liftIO ((,) <$> buttonValues ctx <*> mapM readVarIO [aRef, bRef])

-- | Sibling sliders keep separate drags: dragging the second leaves the first.
slidersDragApart :: Sim ((Response, Float), (Response, Float)) ()
slidersDragApart = do
  (_, (second, _)) <- result
  let Rect rx ry rw rh = respRect second
      Rect tx ty tw th = sliderTrackBounds rx ry rw rh
  _ <- press (At (V2 (tx + tw * 0.25) (ty + th / 2)))
  ((_, v1), (_, v2)) <- moveTo (At (V2 (tx + tw * 0.75) (ty + th / 2)))
  checkEq 20 v1
  checkLt 50 v2

-- | A slider given a value outside its range shows it clamped, and one given
-- a NaN bound, equal bounds or a reversed range, which keeps a value inside
-- it, settles rather than adopting the value afresh on every frame.
runSliderBadValuesTest :: Context -> IORef Int -> IO ()
runSliderBadValuesTest _ failed = do
  let nan = 0 / 0
      cases = [(0, 1, 1.2, Just 1), (nan, 1, 0.5, Nothing), (0, nan, 0.5, Nothing), (5, 5, 5, Nothing)]
  forM_ (cases ++ [(100, 0, 50, Just 50)]) $
    \(lo, hi, given, shown) -> do
      ctx <- newContext
      runSim ctx failed (withInput 300 80) (column (withValue given (slider' lo hi))) $ do
        (resp, v) <- frame
        again <- wantsFrame
        check (not (respChanged resp) && not again)
        forM_ shown (`checkEq` v)

-- | A drag released while its slider, or its knob (whose drag 'useDrag2DOn'
-- runs), is not built stays over: a later press elsewhere, held while the
-- widget comes back, leaves its value alone. The drag moves the pointer by
-- @(dx, dy)@.
runStaleDragTest :: (Var Float -> NanoUI Response) -> (Float, Float) -> Context -> IORef Int -> IO ()
runStaleDragTest widget (dx, dy) ctx failed = do
  shownRef <- newIORef True
  let built shown = if shown then first Just <$> withValue 50 widget else pure (Nothing, 50)
      ui = column (liftIO (readIORef shownRef) >>= built)
      showing = liftIO . writeIORef shownRef
  runSim ctx failed (withInput 300 200) ui $ do
    Just resp <- fst <$> result
    let V2 x y = centerOf resp
    _ <- press (On resp)
    checkLt 50 . snd =<< moveTo (At (V2 (x + dx) (y + dy)))
    showing False
    _ <- frame >> release >> press (At (V2 200 150))
    showing True
    checkEq 50 . snd =<< moveTo (At (V2 200 100))
