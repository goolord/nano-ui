module Cases.Modal
  ( runModalCloseDamageTest
  , runModalNoPhantomScrollTest
  , runModalOverlayTest
  , runModalFitsTextTest
  , runModalFractionalScaleNoScrollTest
  , runModalFillLabelFitsTest
  ) where

import Control.Monad (forM_, when)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, assertJust, evalUi, withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , checkIdleFullDamage
  , clickPair
  , keyInp
  , runClick
  , spanYOf
  , warmup2
  , withInputOff
  )

runModalOverlayTest :: Context -> IORef Int -> IO ()
runModalOverlayTest ctx failed = do
  let
    inp0 = withInput 320 200
    ui = column $ do
      outside <- button' "Outside"
      (dlg, mInside) <- modal True "Title" (button' "Inside")
      pure (outside, dlg, mInside)
    closedUi = column $ do
      _ <- button "Outside"
      (dlg, mInside) <- modal False "Title" (button' "Inside")
      pure (dlg, mInside)

  (dlgClosed, mInsideClosed) <- evalUi ctx inp0 closedUi
  assert failed (not (respClicked dlgClosed))
  assert failed (case mInsideClosed of Nothing -> True; _ -> False)
  closedSpans <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> "Title" `T.isInfixOf` txt) closedSpans))

  (_, _, mInside0) <- warmup2 ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (any (\(_, txt, _, _, _) -> "Title" `T.isInfixOf` txt) overlays)
  assert failed (any (\(_, txt, _, _, _) -> "Inside" `T.isInfixOf` txt) overlays)
  assert failed (not (any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays))

  assertJust failed mInside0 $ \inside -> do
    let (pressIn, releaseIn) = clickPair inp0 (centerOf inside)
    _ <- runFrame ctx pressIn ui
    ((_, _, mClicked), _, _, _) <- runFrame ctx releaseIn ui
    assert failed (maybe False respClicked mClicked)

    let (backdrop, _) = clickPair inp0 (V2 4 4)
    ((_, dlgHit, _), _, _, _) <- runFrame ctx backdrop ui
    assert failed (respClicked dlgHit)

    let esc = keyInp KeyEscape inp0
    ((_, dlgEsc, _), _, _, _) <- runFrame ctx esc ui
    assert failed (respClicked dlgEsc)
    consumed <- overlayConsumesQuit ctx esc
    assert failed consumed
    _ <- runFrame ctx esc closedUi
    leftover <- overlayConsumesQuit ctx esc
    assert failed (not leftover)

  let tallUi = modal True "Tall" $ do
        forM_ [1 .. 40 :: Int] (\i -> label (T.pack ("Row " <> show i)))
        button "Close"
  (dlgTall, _) <- warmup2 ctx inp0 tallUi
  assert failed (rectH (respRect dlgTall) <= 200)

runModalNoPhantomScrollTest :: Context -> IORef Int -> IO ()
runModalNoPhantomScrollTest ctx failed = do
  let inp0 = withInput 400 300
      ui = modal True "About" $ do
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
        (open, setOpen) <- useFlag False
        resp <- button' "Open"
        when (respClicked resp) (setOpen True)
        (dlg, _) <- modal open "Title" (label "body")
        when (respClicked dlg) (setOpen False)
        pure resp
      inp0 = withInputOff 320 240
      esc = keyInp KeyEscape inp0
      idle = inp0 {inputDeltaTime = 1}
  resp <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf resp)
  checkIdleFullDamage failed ctx idle idle ui
  _ <- runFrame ctx esc ui
  checkIdleFullDamage failed ctx idle idle ui

-- At a fractional display scale, a modal sized to its content does not scroll
-- when that content uses fixed sizes off the device-pixel grid (regression:
-- the solve rounded the measured sizes inside the modal before placement laid
-- it out from them, and snapping a child's origin pushed its bottom below its
-- measured bottom so content-sized parents grew level after level; the body
-- overflowed its viewport by more than the scroll tolerance).
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
          | nested = column (label "Behind" >> fst <$> modal True "Details" body)
          | otherwise = fst <$> modal True "Details" body
        -- Whether a wheel over the modal moves its first field.
        scrolls c i = do
          dlg <- warmup2 c i ui
          let wheel = i {inputMousePos = centerOf dlg, inputScroll = V2 0 3}
          spans0 <- collectOverlayTextSpans c i
          _ <- runFrame c wheel ui
          spans1 <- collectOverlayTextSpans c wheel
          assert failed (not (null (spanYOf "Field 1" spans0)))
          pure (spanYOf "Field 1" spans1 /= spanYOf "Field 1" spans0)
    fits <- scrolls ctx inp
    assertEq failed fits False
    -- The same body in a short window does scroll, so the check can fail.
    short <- newContext
    clipped <- scrolls (withFontMetrics short ((monospaceMetrics 12) {fmSnapScale = scale})) (withInputOff 1000 300)
    assertEq failed clipped True

-- A modal widens for a filling label instead of wrapping it, so the label
-- stays one line inside the modal (regression: the label reported no width, the
-- modal stayed at its minimum, and the wrapped body overflowed into a scroll).
runModalFitsTextTest :: Context -> IORef Int -> IO ()
runModalFitsTextTest ctx failed = do
  let inp = withInput 800 600
      sentence = T.pack "A sentence that is wider than the smallest modal allows."
      ui = fst <$> modal True "About" (muted sentence)
  dlg <- warmup2 ctx inp ui
  spans <- collectOverlayTextSpans ctx inp
  let Rect _ _ dw _ = respRect dlg
      whole = [r | (r, t, _, _, _) <- spans, t == sentence]
  assertEq failed (length whole) 1
  forM_ whole $ \(Rect _ _ tw _) -> assertGt failed (dw + 0.5) tw

-- A modal fits a body with a filling label set in a smaller font than the
-- base, with its last row in view (regression: placing the modal measured
-- every label in the base font, so the smaller label, sized for its own font,
-- wrapped onto a second line the modal had not measured; the modal scrolled
-- and clipped its buttons).
runModalFillLabelFitsTest :: Context -> IORef Int -> IO ()
runModalFillLabelFitsTest _ failed = forM_ [12, 17] $ \base -> do
  ctx <- (`withFontMetrics` monospaceMetrics base) <$> newContext
  -- Wide enough for the label on one line in its own font, but not in the
  -- base font.
  let inp = withInput 2000 800
      body =
        columnWith (gap 14 . minW 560 . \l -> l {layoutPadding = Padding 0 0 0 0}) $ do
          labelWith (tight . fillW) "How cabal should log in to Hackage for uploads:"
          _ <- radio ["cabal's config file (no login found in it)", "A username and password", "An API token"] (0 :: Int)
          labelWith (tight . fillW . fontSize 14) "Kept in memory for this session only. The password goes to cabal on its standard input."
          separator
          rowWith (fillW . gap 8 . alignMid . tight) $ do
            flex
            _ <- button "Cancel"
            button' "Use this login"
      ui = modal True "Hackage login" body
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
