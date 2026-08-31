module Cases.ContextMenu
  ( runContextMenuOpenTest
  , runContextMenuDismissTest
  , runContextMenuRightDismissTest
  , runContextMenuPickTest
  , runContextMenuAreaTest
  , runContextMenuSpansTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, eval2Ui, evalUi, withInput)
import NanoUI.Testing.Harness (centerOf, clickPair, warmup2)

menuUi :: NanoUI (Response, Maybe (Response, Response))
menuUi = column defaultLayout $ do
  btn <- button "Target Button"
  mInside <- contextMenu btn $ do
    cut <- menuItem "Cut"
    copy <- menuItem "Copy"
    pure (cut, copy)
  pure (btn, mInside)

openMenu :: Context -> IORef Int -> Input -> IO (Response, Maybe (Response, Response))
openMenu ctx failed inp0 = do
  (btnWarm, _) <- warmup2 ctx inp0 menuUi
  let rightClickPos = centerOf btnWarm
      inpRightDown = inp0 {inputMousePos = rightClickPos, inputMouseRightDown = True, inputMouseRightPressed = True}
      inpRightUp = inp0 {inputMousePos = rightClickPos, inputMouseRightReleased = True}
  ((btnDown, _), _, _, _) <- runFrame ctx inpRightDown menuUi
  assert failed (not (respRightClicked btnDown))
  ((btnUp, mInside), _, _, _) <- runFrame ctx inpRightUp menuUi
  pure (btnUp, mInside)

runContextMenuOpenTest :: Context -> IORef Int -> IO ()
runContextMenuOpenTest ctx failed = do
  let inp0 = withInput 640 480
  (btn0, mInside0) <- evalUi ctx inp0 menuUi
  assert failed (not (respRightClicked btn0))
  assert failed (case mInside0 of Nothing -> True; _ -> False)

  (btnClicked, mInsideOpen) <- openMenu ctx failed inp0
  assert failed (respRightClicked btnClicked)
  assert failed (case mInsideOpen of Just _ -> True; Nothing -> False)

runContextMenuDismissTest :: Context -> IORef Int -> IO ()
runContextMenuDismissTest ctx failed = do
  let inp0 = withInput 640 480
  _ <- openMenu ctx failed inp0
  let (pressOut, releaseOut) = clickPair inp0 (V2 500 400)
  _ <- runFrame ctx pressOut menuUi
  ((_, mInsideAfterDismiss), _, _, _) <- runFrame ctx releaseOut menuUi
  assert failed (case mInsideAfterDismiss of Nothing -> True; Just _ -> False)

runContextMenuRightDismissTest :: Context -> IORef Int -> IO ()
runContextMenuRightDismissTest ctx failed = do
  let inp0 = withInput 640 480
  _ <- openMenu ctx failed inp0
  let inpRightOut =
        inp0
          { inputMousePos = V2 500 400
          , inputMouseRightDown = True
          , inputMouseRightPressed = True
          }
  _ <- runFrame ctx inpRightOut menuUi
  ((_, mInsideAfterDismiss), _, _, _) <- runFrame ctx inp0 menuUi
  assert failed (case mInsideAfterDismiss of Nothing -> True; Just _ -> False)

runContextMenuPickTest :: Context -> IORef Int -> IO ()
runContextMenuPickTest ctx failed = do
  let inp0 = withInput 640 480
  (_, mInsideOpen) <- openMenu ctx failed inp0
  ((_, mPlaced), _, _, _) <- runFrame ctx inp0 menuUi
  let mCut = case mPlaced of
        Just pair -> Just pair
        Nothing -> mInsideOpen
  case mCut of
    Nothing -> assert failed False
    Just (cut, _) -> do
      let (press, release) = clickPair inp0 (centerOf cut)
      _ <- runFrame ctx press menuUi
      _ <- runFrame ctx release menuUi
      ((_, mInsideAfterPick), _, _, _) <- runFrame ctx inp0 menuUi
      assert failed (case mInsideAfterPick of Nothing -> True; Just _ -> False)

runContextMenuAreaTest :: Context -> IORef Int -> IO ()
runContextMenuAreaTest ctx failed = do
  let inp0 = withInput 640 480
      ui = do
        (areaVal, mInside) <- contextMenuArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "Canvas Area") $ \_ -> do
          menuHeader "Actions"
          menuSeparator
          paste <- menuItemWithShortcut "Paste" "Ctrl+V"
          delete <- menuItemWithIcon "[X]" "Delete"
          menuItemDisabled "Export"
          pure (paste, delete)
        pure (areaVal, mInside)

  (_, mInside0) <- evalUi ctx inp0 ui
  assert failed (case mInside0 of Nothing -> True; _ -> False)

runContextMenuSpansTest :: Context -> IORef Int -> IO ()
runContextMenuSpansTest ctx failed = do
  let inp0 = withInput 640 480
      ui = column defaultLayout $ do
        btn <- button "Target Button"
        void $ contextMenu btn $ do
          void $ menuItem "Special Action"
          void $ menuItemWithShortcut "Find" "Ctrl+F"

  (btnWarm) <- eval2Ui ctx inp0 (button "Target Button")
  let rightClickPos = centerOf btnWarm
      inpRightDown = inp0 {inputMousePos = rightClickPos, inputMouseRightDown = True, inputMouseRightPressed = True}
      inpRightUp = inp0 {inputMousePos = rightClickPos, inputMouseRightReleased = True}
  _ <- runFrame ctx inpRightDown ui
  _ <- runFrame ctx inpRightUp ui

  spans <- collectOverlayTextSpans ctx inp0
  assert failed (any (\(_, txt, _, _, _) -> "Special Action" `T.isInfixOf` txt) spans)
  assert failed (any (\(_, txt, _, _, _) -> "Find" `T.isInfixOf` txt) spans)
