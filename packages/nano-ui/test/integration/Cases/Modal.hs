module Cases.Modal (tests) where

import Spec
import Data.Maybe (isJust, listToMaybe)
import Data.Text qualified as T
import NanoUI.Internal.Context (setDrawSquareGeometry)

tests :: [Spec]
tests =
  [ spec "modal-overlay" runModalOverlayTest
  , spec "modal-var" runModalVarTest
  , spec "modal-fits-text" runModalFitsTextTest
  , spec "modal-no-phantom-scroll" runModalNoPhantomScrollTest
  , spec "modal-close-damage" runModalCloseDamageTest
  , spec "modal-closed-by-rerun" runModalClosedByRerunTest
  , spec "modal-fractional-scale-no-scroll" runModalFractionalScaleNoScrollTest
  , spec "modal-fill-label-fits" runModalFillLabelFitsTest
  , spec "modal-tree-selection" runModalTreeSelectionTest
  , spec "modal-nested-dismiss" runModalNestedDismissTest
  , spec "modal-over-inner-window" runModalOverInnerWindowTest
  , spec "modal-over-inner-window-drag" runModalOverInnerWindowDragTest
  ]

-- | A modal declared inside another's body closes first: Escape, then a
-- click on the outer modal, each close only the inner one.
runModalNestedDismissTest :: Context -> IORef Int -> IO ()
runModalNestedDismissTest ctx failed = do
  outer <- newVar True
  inner <- newVar True
  let inp0 = withInput 640 480
      ui = column $ do
        _ <- button "Behind"
        (dlg, _) <- modal' outer "Settings" $ do
          forM_ [1 .. 10 :: Int] (\i -> label (T.pack ("Setting " <> show i)))
          modal inner "Confirm" (label "confirm body")
        pure dlg
      reopen = writeVarIO inner True >> warmup ctx inp0 ui
      states = (,) <$> readVarIO outer <*> readVarIO inner
  reopen
  _ <- runFrame ctx (keyInp KeyEscape inp0) ui
  assertEq failed (True, False) =<< states
  reopen
  Rect dx dy dw _ <- respRect <$> evalUi ctx inp0 ui
  _ <- runClick ctx inp0 ui (V2 (dx + dw / 2) (dy + 8))
  assertEq failed (True, False) =<< states

-- | A window declared in a modal's body is painted under the modal panel.
-- Where they overlap, a click goes to the modal's button, not to the
-- window's button under it.
runModalOverInnerWindowTest :: Context -> IORef Int -> IO ()
runModalOverInnerWindowTest ctx failed = do
  let inp0 = withInputOff 500 400
      ui = fmap snd . modal' (constVar True) "Settings" $ do
        (_, hidden) <- window' (constVar True) "Inspector" (buttonWith' (fixedWH 380 300) "Hidden")
        ok <- button' "OK"
        pure (ok, hidden)
  mr <- warmup2 ctx inp0 ui
  assertJust failed mr $ \(ok, mHidden) -> assertJust failed mHidden $ \hidden -> do
    assert failed (isJust (rectIntersect (respRect hidden) (respRect ok)))
    let (press, release) = clickPair inp0 (centerOf ok)
    _ <- runFrame ctx press ui
    assertJustM failed (evalUi ctx release ui) $ \(ok', hidden') -> do
      assert failed (respClicked ok')
      assert failed (not (any respClicked hidden'))

-- | Nor does a drag on that window's title bar, where the modal panel covers
-- it, move the window.
runModalOverInnerWindowDragTest :: Context -> IORef Int -> IO ()
runModalOverInnerWindowDragTest ctx failed = do
  let inp0 = withInputOff 500 400
      ui = modalWith (fixedWH 460 380) (constVar True) "Settings" (window (constVar True) "Inspector" (label "body"))
  _ <- warmup2 ctx inp0 ui
  before <- map snd <$> nodesOfType NodeWindow ctx
  assertJust failed (listToMaybe before) $ \(Rect x y w _) -> do
    let press = pressAt inp0 (V2 (x + w / 2) (y + 10))
        moved = holdAt press (V2 (x + w / 2 - 40) (y + 40))
    mapM_ (\i -> runFrame ctx i ui) [press, moved, releaseAt moved]
    assertEq failed before . map snd =<< nodesOfType NodeWindow ctx

runModalOverlayTest :: Context -> IORef Int -> IO ()
runModalOverlayTest ctx failed = do
  let
    inp0 = withInput 320 200
    ui = column $ do
      outside <- button' "Outside"
      (dlg, mInside) <- modal' (constVar True) "Title" (button' "Inside")
      pure (outside, dlg, mInside)
    closedUi = column $ do
      _ <- button "Outside"
      (dlg, mInside) <- modal' (constVar False) "Title" (button' "Inside")
      pure (dlg, mInside)

  (dlgClosed, mInsideClosed) <- evalUi ctx inp0 closedUi
  assert failed (not (respClicked dlgClosed))
  assert failed (case mInsideClosed of Nothing -> True; _ -> False)
  closedSpans <- collectOverlayTextSpans ctx inp0
  assert failed (not (hasText "Title" closedSpans))

  (_, _, mInside0) <- warmup2 ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (hasText "Title" overlays && hasText "Inside" overlays)
  assert failed (not (any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays))

  assertJust failed mInside0 $ \inside -> do
    (_, _, mClicked) <- runClick ctx inp0 ui (centerOf inside)
    assert failed (maybe False respClicked mClicked)

    (_, dlgHit, _) <- evalUi ctx (pressAt inp0 (V2 4 4)) ui
    assert failed (respClicked dlgHit)

    let esc = keyInp KeyEscape inp0
    ((_, dlgEsc, _), _, _) <- runFrame ctx esc ui
    assert failed (respClicked dlgEsc)
    assert failed =<< overlayConsumesQuit ctx esc
    _ <- runFrame ctx esc closedUi
    assert failed . not =<< overlayConsumesQuit ctx esc

  let tallUi = modal' (constVar True) "Tall" $ do
        forM_ [1 .. 40 :: Int] (\i -> label (T.pack ("Row " <> show i)))
        button "Close"
  (dlgTall, _) <- warmup2 ctx inp0 tallUi
  assert failed (rectH (respRect dlgTall) <= 200)

-- A modal on a Var closes itself: a dismissal writes False and the view runs
-- again, so the frame that saw Escape already draws it closed. A button in
-- the body closes it by writing the Var, and a constant reports the request
-- and stays open.
runModalVarTest :: Context -> IORef Int -> IO ()
runModalVarTest ctx failed = do
  let inp0 = withInput 320 200
  open <- newVar True
  let ui = column $ do
        _ <- button "Outside"
        modal open "Title" (label "body")
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (keyInp KeyEscape inp0) ui
  assertEq failed False =<< readVarIO open
  assertEq failed 2 =<< lastFramePasses ctx
  assert failed . not . hasText "Title" =<< collectOverlayTextSpans ctx inp0

  writeVarIO open True
  let withClose = column $ do
        (_, close) <- modal' open "Title" $ do
          r <- button' "Close"
          r <$ when (respClicked r) (writeVar open False)
        pure close
  mClose <- warmup2 ctx inp0 withClose
  assertJust failed mClose $ \close -> do
    _ <- runClick ctx inp0 withClose (centerOf close)
    assertEq failed False =<< readVarIO open

  let fixed = fst <$> modal' (constVar True) "Title" (label "body")
  _ <- warmup2 ctx inp0 fixed
  (dlg, _, _) <- runFrame ctx (keyInp KeyEscape inp0) fixed
  assert failed (respClicked dlg)
  assert failed . hasText "Title" =<< collectOverlayTextSpans ctx inp0

runModalNoPhantomScrollTest :: Context -> IORef Int -> IO ()
runModalNoPhantomScrollTest ctx failed = do
  let inp0 = withInput 400 300
      ui = modal' (constVar True) "About" $ do
        _ <- label "Immediate-mode GUI for Haskell."
        rowWith fillW $ do
          _ <- spacer (Grow 1) Fit
          _ <- button "Close"
          pure ()
  (dlg, _) <- warmup2 ctx inp0 ui
  let Rect _ _ mw mh = respRect dlg
  assert failed (mw > 0 && mh > 0)
  off0 <- getScrollOffset ctx (respId dlg)
  let wheel = inp0 {inputMousePos = centerOf dlg, inputScroll = V2 0 1}
  _ <- runFrame ctx wheel ui
  off1 <- getScrollOffset ctx (respId dlg)
  assertEq failed off0 0
  assertEq failed off1 0

-- Opening and closing a modal each repaint the whole window on the next idle
-- frame.
runModalCloseDamageTest :: Context -> IORef Int -> IO ()
runModalCloseDamageTest ctx failed = do
  let ui = do
        openVar <- useVar False
        open <- readVar openVar
        resp <- button' "Open"
        when (respClicked resp) (writeVar openVar True)
        (dlg, _) <- modal' (constVar open) "Title" (label "body")
        when (respClicked dlg) (writeVar openVar False)
        pure resp
      inp0 = withInputOff 320 240
      esc = keyInp KeyEscape inp0
      idle = inp0 {inputDeltaTime = 1}
  resp <- warmup2 ctx inp0 ui
  -- Opening: the click frame runs the view a second time with the flag set,
  -- builds the modal, and repaints the whole window (floating panel change).
  _ <- runClick ctx inp0 ui (centerOf resp)
  assertEq failed DamageFull =<< takeDamage ctx
  -- The idle frame after only settles the button's release, as a clip: the
  -- store write behind the open is damaged per key, not whole-window.
  _ <- runFrame ctx idle ui
  dmgIdle <- takeDamage ctx
  assert failed (dmgIdle /= DamageFull)
  assert failed (not (damageIsEmpty dmgIdle))
  -- Closing: the escape frame removes the modal and repaints the whole window.
  _ <- runFrame ctx esc ui
  assertEq failed DamageFull =<< takeDamage ctx

-- | A modal its Var closes is gone from the frame that closed it, whose
-- second view pass no longer builds it: the next frame does not repaint the
-- whole window for it a second time.
runModalClosedByRerunTest :: Context -> IORef Int -> IO ()
runModalClosedByRerunTest ctx failed = do
  let inp0 = withInputOff 320 240
  open <- newVar True
  let ui = column $ do
        _ <- button "Outside"
        modal open "Title" (label "body")
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (keyInp KeyEscape inp0) ui
  assertEq failed 2 =<< lastFramePasses ctx
  _ <- runFrame ctx inp0 ui
  assert failed . (/= DamageFull) =<< takeDamage ctx

-- At fractional display scales, fixed-size content must fit a content-sized
-- modal without accumulating rounding error through nested containers.
runModalFractionalScaleNoScrollTest :: Context -> IORef Int -> IO ()
runModalFractionalScaleNoScrollTest _ failed =
  forM_ [(scale, rows, nested) | scale <- [1, 1.25, 1.5, 1.75], rows <- [4 .. 8 :: Int], nested <- [False, True]] $ \(scale, rows, nested) -> do
    base <- newContext
    let ctx = withFontMetrics base ((monospaceMetrics 12) {fmSnapScale = scale})
        inp = withInputOff 1000 1000
        field i = rowWith (fillW . fixedH 30 . alignMid) (label (T.pack ("Field " <> show i)))
        section = columnWith (fillW . gap 6) $ do
          label "Section"
          columnWith (fillW . gap 0) $ do
            spacer Fit (Fixed 3)
            forM_ [1 .. rows] field
            spacer Fit (Fixed 3)
        body = columnWith (gap 10) (section >> section >> section >> button "Close")
        -- As the arena's root, and inside other content as an app opens one.
        ui
          | nested = column (label "Behind" >> fst <$> modal' (constVar True) "Details" body)
          | otherwise = fst <$> modal' (constVar True) "Details" body
        -- Whether a wheel over the modal moves its first field.
        scrolls c i = do
          dlg <- warmup2 c i ui
          let wheel = i {inputMousePos = centerOf dlg, inputScroll = V2 0 3}
          spans0 <- collectOverlayTextSpans c i
          _ <- runFrame c wheel ui
          spans1 <- collectOverlayTextSpans c wheel
          assert failed (not (null (spanYOf "Field 1" spans0)))
          pure (spanYOf "Field 1" spans1 /= spanYOf "Field 1" spans0)
    assertEq failed False =<< scrolls ctx inp
    -- The same body in a short window does scroll, so the check can fail.
    short <- newContext
    assertEq failed True =<< scrolls (withFontMetrics short ((monospaceMetrics 12) {fmSnapScale = scale})) (withInputOff 1000 300)

-- A modal widens for a filling label so it stays on one line when space permits.
runModalFitsTextTest :: Context -> IORef Int -> IO ()
runModalFitsTextTest ctx failed = do
  let inp = withInput 800 600
      sentence = T.pack "A sentence that is wider than the smallest modal allows."
      ui = fst <$> modal' (constVar True) "About" (muted sentence)
  dlg <- warmup2 ctx inp ui
  spans <- collectOverlayTextSpans ctx inp
  let Rect _ _ dw _ = respRect dlg
      whole = [r | (r, t, _, _, _) <- spans, t == sentence]
  assertEq failed (length whole) 1
  forM_ whole $ \(Rect _ _ tw _) -> assertGt failed (dw + 0.5) tw

-- A modal measures each label in its requested font size and keeps the last
-- row visible when the measured body fits.
runModalFillLabelFitsTest :: Context -> IORef Int -> IO ()
runModalFillLabelFitsTest _ failed = forM_ [12, 17] $ \base -> do
  ctx <- (`withFontMetrics` monospaceMetrics base) <$> newContext
  -- Wide enough for the label on one line in its own font, but not in the
  -- base font.
  let inp = withInput 2000 800
      body =
        columnWith (gap 14 . minW 560 . \l -> l {layoutPadding = Padding 0 0 0 0}) $ do
          labelWith (tight . fillW) "How cabal should log in to Hackage for uploads:"
          _ <- (snd <$> withValue (0 :: Int) (radio ["cabal's config file (no login found in it)", "A username and password", "An API token"]))
          labelWith (tight . fillW . fontSize 14) "Kept in memory for this session only. The password goes to cabal on its standard input."
          separator
          rowWith (fillW . gap 8 . alignMid . tight) $ do
            flex
            _ <- button "Cancel"
            button' "Use this login"
      ui = modal' (constVar True) "Hackage login" body
  (dlg, mOk) <- warmup2 ctx inp ui
  spans0 <- collectOverlayTextSpans ctx inp
  let wheel = inp {inputMousePos = centerOf dlg, inputScroll = V2 0 3}
  _ <- runFrame ctx wheel ui
  spans1 <- collectOverlayTextSpans ctx wheel
  assert failed (not (null (spanYOf "An API token" spans0)))
  assertEq failed (spanYOf "An API token" spans1) (spanYOf "An API token" spans0)
  assertJust failed mOk $ \ok -> do
    let Rect _ dy _ dh = respRect dlg
        Rect _ by _ bh = respRect ok
    assertGt failed (dy + dh + 0.5) (by + bh)

-- | A tree in a modal draws its selected row apart from the others, as one
-- on the page does.
runModalTreeSelectionTest :: Context -> IORef Int -> IO ()
runModalTreeSelectionTest ctx failed = do
  setDrawSquareGeometry ctx True
  let inp0 = withInput 320 200
      items = [TreeItem "alpha" [], TreeItem "beta" []]
      -- The last fill under a row's whole label is that row's background.
      rowFills sel = do
        (_, draw) <- warmupDraw ctx inp0 (modal' (constVar True) "Pick" (tree' "t" items (constVar sel)))
        quads <- drawQuads draw
        spans <- collectOverlayTextSpans ctx inp0
        let under lbl = spanRectOf lbl spans >>= \r -> listToMaybe (reverse [c | (q, c) <- quads, covers q r])
        pure (under "alpha", under "beta")
  (alpha0, beta0) <- rowFills 0
  (alpha1, beta1) <- rowFills 1
  assert failed (isJust alpha0 && isJust beta0)
  assert failed (alpha0 /= alpha1 && beta0 /= beta1)
