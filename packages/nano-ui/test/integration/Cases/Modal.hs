module Cases.Modal
  ( runModalCloseDamageTest
  , runModalNoPhantomScrollTest
  , runModalOpenDamageTest
  , runModalOverlayTest
  ) where

import Control.Monad (forM_)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, eval2Ui, evalUi, withInput)
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
    ui = column defaultLayout $ do
      outside <- button "Outside"
      (dlg, mInside) <- modal True "Title" (button "Inside")
      pure (outside, dlg, mInside)
    closedUi = column defaultLayout $ do
      _ <- button "Outside"
      (dlg, mInside) <- modal False "Title" (button "Inside")
      pure (dlg, mInside)

  (dlgClosed, mInsideClosed) <- evalUi ctx inp0 closedUi
  assert failed (not (respClicked dlgClosed))
  assert failed (case mInsideClosed of Nothing -> True; _ -> False)
  closedSpans <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> "Title" `T.isInfixOf` txt) closedSpans))

  (outside0, _, mInside0) <- warmup2 ctx inp0 ui
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

      let (pressOut, _) = clickPair inp0 (centerOf outside0)
      ((outsideHit, _, _), _, _, _) <- runFrame ctx pressOut ui
      assert failed (not (respClicked outsideHit))

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
  (dlgTall, _) <- eval2Ui ctx inp0 tallUi
  assert failed (rectH (respRect dlgTall) <= 200)

runModalNoPhantomScrollTest :: Context -> IORef Int -> IO ()
runModalNoPhantomScrollTest ctx failed = do
  let inp0 = withInput 400 300
      ui = modal True "About" $ do
        _ <- label "Immediate-mode GUI for Haskell."
        row (defaultLayout {layoutWidth = Grow 1}) $ do
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

runModalCloseDamageTest :: Context -> IORef Int -> IO ()
runModalCloseDamageTest _ failed = do
  ctx <- newContext
  let ui = do
        (open, setOpen) <- useFlag True
        (resp, _) <- modal open "Title" (label "body")
        onClick resp (setOpen False)
      inp0 = (withInput 320 240) {inputMousePos = V2 1 1}
      esc = inp0 {inputKeys = inputKeysFromList [KeyEscape]}
      idle = inp0 {inputDeltaTime = 1}
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx esc ui
  checkIdleFullDamage failed ctx idle idle ui

runModalOpenDamageTest :: Context -> IORef Int -> IO ()
runModalOpenDamageTest _ failed = do
  ctx <- newContext
  let ui = do
        (open, setOpen) <- useFlag False
        resp <- button "Open"
        onClick resp (setOpen True)
        _ <- modal open "Title" (label "body")
        pure resp
      inp0 = withInputOff 320 240
      idle = inp0 {inputDeltaTime = 1}
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  _ <- runClickRelease ctx inp0 ui (centerOf resp)
  checkIdleFullDamage failed ctx idle idle ui



