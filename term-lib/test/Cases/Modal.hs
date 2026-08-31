module Cases.Modal
  ( runModalCloseDamageTest
  , runModalNoPhantomScrollTest
  , runModalOpenDamageTest
  , runModalOverlayTest
  ) where

import Control.Monad (when)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , checkIdleFullDamage
  , runClickRelease
  , warmup2
  , withInputOff
  )
runModalOverlayTest :: Context -> IORef Int -> IO ()
runModalOverlayTest ctx failed = do
  let
    inp0 = withInput 320 200
    ui =
      column defaultLayout $ do
        outside <- button "Outside"
        (dlg, mInside) <-
          modal True "Title" $ do
            button "Inside"
        pure (outside, dlg, mInside)
    closedUi =
      column defaultLayout $ do
        _ <- button "Outside"
        (dlg, mInside) <-
          modal False "Title" $ do
            button "Inside"
        pure (dlg, mInside)
  do
    ((dlg, mInside), _, _, _) <- runFrame ctx inp0 closedUi
    when (respClicked dlg) $ bump failed
    case mInside of
      Nothing -> pure ()
      Just _ -> bump failed
    closedSpans <- collectOverlayTextSpans ctx inp0
    let
      closedTitle = any (\(_, txt, _, _, _) -> "Title" `T.isInfixOf` txt) closedSpans
    when closedTitle $ bump failed
  (outside0, _, mInside0) <- warmup2 ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  let
    hasTitle = any (\(_, txt, _, _, _) -> "Title" `T.isInfixOf` txt) overlays
    hasInside = any (\(_, txt, _, _, _) -> "Inside" `T.isInfixOf` txt) overlays
    hasCloseGlyph = any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays
  when (not (hasTitle && hasInside)) $ bump failed
  when hasCloseGlyph $ bump failed
  case mInside0 of
    Just inside -> do
      let
        Rect ix iy iw ih = respRect inside
        clickIn =
          inp0
            { inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)
            , inputMouseDown = True
            , inputMousePressed = True
            }
      _ <- runFrame ctx clickIn ui
      let
        releaseIn =
          clickIn
            { inputMouseDown = False
            , inputMousePressed = False
            , inputMouseReleased = True
            }
      ((_, _, mClicked), _, _, _) <- runFrame ctx releaseIn ui
      case mClicked of
        Just r -> when (not (respClicked r)) $ bump failed
        Nothing -> bump failed
      let
        Rect ox oy ow oh = respRect outside0
        clickOut =
          inp0
            { inputMousePos = V2 (ox + ow / 2) (oy + oh / 2)
            , inputMouseDown = True
            , inputMousePressed = True
            }
      ((outsideHit, _, _), _, _, _) <- runFrame ctx clickOut ui
      when (respClicked outsideHit) $ bump failed
      let
        backdrop =
          inp0
            { inputMousePos = V2 4 4
            , inputMouseDown = True
            , inputMousePressed = True
            }
      ((_, dlg, _), _, _, _) <- runFrame ctx backdrop ui
      when (not (respClicked dlg)) $ bump failed
      let
        esc = inp0 {inputKeys = inputKeysFromList [KeyEscape]}
      ((_, dlgEsc, _), _, _, _) <- runFrame ctx esc ui
      when (not (respClicked dlgEsc)) $ bump failed
      consumed <- overlayConsumesQuit ctx esc
      failWhen failed (not consumed)
      _ <- runFrame ctx esc closedUi
      leftover <- overlayConsumesQuit ctx esc
      when leftover $ bump failed
    Nothing -> bump failed
  let
    tallUi =
      modal True "Tall" $ do
        mapM_ (\i -> label (T.pack ("Row " <> show (i :: Int)))) [1 .. 40]
        button "Close"
  _ <- runFrame ctx inp0 tallUi
  ((dlgTall, _), _, _, _) <- runFrame ctx inp0 tallUi
  let
    Rect _ _ _ mh = respRect dlgTall
  when (mh > 200) $ bump failed

runModalNoPhantomScrollTest :: Context -> IORef Int -> IO ()
runModalNoPhantomScrollTest ctx failed = do
  let
    inp0 = withInput 400 300
    ui =
      modal True "About" $ do
        _ <- label "Immediate-mode GUI for Haskell."
        row
          (defaultLayout {layoutWidth = Grow 1})
          $ do
            _ <- spacer (Grow 1) Fit
            _ <- button "Close"
            pure ()
  (dlg, _) <- warmup2 ctx inp0 ui
  let
    Rect mx my mw mh = respRect dlg
  when (mw <= 0 || mh <= 0) $ bump failed
  off0 <- getScrollOffset ctx (respId dlg)
  let
    wheel =
      inp0
        { inputMousePos = V2 (mx + mw / 2) (my + mh / 2)
        , inputScroll = V2 0 1
        }
  _ <- runFrame ctx wheel ui
  off1 <- getScrollOffset ctx (respId dlg)
  when (off0 /= 0 || off1 /= 0) $ bump failed

runModalCloseDamageTest :: Context -> IORef Int -> IO ()
runModalCloseDamageTest _ failed = do
  ctx <- newContext
  let
    ui = do
      (readOpen, setOpen) <- useFlag True
      open <- readOpen
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
  let
    ui = do
      (readOpen, setOpen) <- useFlag False
      resp <- button "Open"
      onClick resp (setOpen True)
      open <- readOpen
      _ <- modal open "Title" (label "body")
      pure resp
    inp0 = withInputOff 320 240
    idle = inp0 {inputDeltaTime = 1}
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  _ <- runClickRelease ctx inp0 ui (centerOf resp)
  checkIdleFullDamage failed ctx idle idle ui



