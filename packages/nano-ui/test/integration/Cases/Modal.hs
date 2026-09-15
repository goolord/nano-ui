module Cases.Modal
  ( runModalCloseDamageTest
  , runModalNoPhantomScrollTest
  , runModalOverlayTest
  , runModalFitsTextTest
  ) where

import Control.Monad (forM_, when)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, evalUi, withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , checkIdleFullDamage
  , clickPair
  , runClickRelease
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

  case mInside0 of
    Nothing -> assert failed False
    Just inside -> do
      let (pressIn, releaseIn) = clickPair inp0 (centerOf inside)
      _ <- runFrame ctx pressIn ui
      ((_, _, mClicked), _, _, _) <- runFrame ctx releaseIn ui
      assert failed (maybe False respClicked mClicked)

      let (backdrop, _) = clickPair inp0 (V2 4 4)
      ((_, dlgHit, _), _, _, _) <- runFrame ctx backdrop ui
      assert failed (respClicked dlgHit)

      let esc = inp0 {inputKeys = inputKeysFromList [KeyEscape]}
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
  let Rect mx my mw mh = respRect dlg
  assert failed (mw > 0 && mh > 0)
  off0 <- getScrollOffset ctx (respId dlg)
  let wheel = inp0 {inputMousePos = V2 (mx + mw / 2) (my + mh / 2), inputScroll = V2 0 1}
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
      esc = inp0 {inputKeys = inputKeysFromList [KeyEscape]}
      idle = inp0 {inputDeltaTime = 1}
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  _ <- runClickRelease ctx inp0 ui (centerOf resp)
  checkIdleFullDamage failed ctx idle idle ui
  _ <- runFrame ctx esc ui
  checkIdleFullDamage failed ctx idle idle ui

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
