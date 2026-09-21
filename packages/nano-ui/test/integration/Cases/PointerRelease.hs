-- | A click belongs to the widget its press went down on. Dragging off a
-- widget and letting go over a neighbour must fire nothing.
module Cases.PointerRelease (tests) where

import Control.Monad (void)
import Data.IORef (IORef, newIORef)
import Data.Maybe (isJust, isNothing)
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, withInput)
import NanoUI.Testing.Harness (centerOf, held, warmup2)
import Spec (Spec, spec)

tests :: [Spec]
tests =
  [ spec "release-elsewhere" runReleaseElsewhereTest
  , spec "right-release-elsewhere" runRightReleaseElsewhereTest
  , spec "release-returns" runReleaseReturnsTest
  , spec "overlap-press" runOverlapPressTest
  ]

-- | Press one widget, drag onto another, release: neither one fires, this
-- frame or the next.
runReleaseElsewhereTest :: Context -> IORef Int -> IO ()
runReleaseElsewhereTest ctx failed = do
  -- The checkbox drives its own state across frames, so a stray toggle sticks
  -- and the value assertions below have something to catch.
  flag <- newIORef False
  let inp0 = withInput 320 240
      ui = column $ do
        a <- button' "Alpha"
        b <- button' "Beta"
        (cb, on) <- held flag (checkbox' "Flag")
        pure (a, b, cb, on)

  (a, b, cb, _) <- warmup2 ctx inp0 ui
  let at r = inp0 {inputMousePos = centerOf r}
      pressOn r = (at r) {inputMouseDown = True, inputMousePressed = True}
      dragTo r p = p {inputMousePos = centerOf r, inputMousePressed = False}
      releaseOn r p = (dragTo r p) {inputMouseDown = False, inputMouseReleased = True}

  -- Button to button.
  _ <- runFrame ctx (pressOn a) ui
  ((_, bDrag, _, _), _, _, _) <- runFrame ctx (dragTo b (pressOn a)) ui
  assert failed (not (respHovered bDrag) && not (respPressed bDrag))
  ((aUp, bUp, _, _), _, _, _) <- runFrame ctx (releaseOn b (pressOn a)) ui
  assert failed (not (respClicked aUp) && not (respClicked bUp))
  ((aNext, bNext, _, _), _, _, _) <- runFrame ctx (at b) ui
  assert failed (not (respClicked aNext) && not (respClicked bNext))

  -- Button to checkbox: the checkbox must not toggle.
  _ <- runFrame ctx (pressOn a) ui
  ((_, _, cbUp, _), _, _, _) <- runFrame ctx (releaseOn cb (pressOn a)) ui
  assert failed (not (respClicked cbUp))
  ((_, _, _, checked), _, _, _) <- runFrame ctx (at cb) ui
  assert failed (not checked)

  -- Checkbox to button: neither fires, and the box stays clear.
  _ <- runFrame ctx (pressOn cb) ui
  ((_, bOver, cbOff, _), _, _, _) <- runFrame ctx (releaseOn b (pressOn cb)) ui
  assert failed (not (respClicked bOver) && not (respClicked cbOff))
  ((_, _, _, stillOff), _, _, _) <- runFrame ctx (at b) ui
  assert failed (not stillOff)

-- | The same rule for the right button: a context menu opens where the right
-- press went down, not where it came up. Also covers the container path, since
-- the menu area's response comes from a container node rather than a leaf.
runRightReleaseElsewhereTest :: Context -> IORef Int -> IO ()
runRightReleaseElsewhereTest ctx failed = do
  let inp0 = withInput 320 240
      ui = column $ do
        a <- button' "Alpha"
        (lbl, menu) <-
          contextMenuArea (fixedH 60 . fillW) (label' "Area") (const (menuItem "Cut"))
        pure (a, lbl, menu)
  (a, lbl, _) <- warmup2 ctx inp0 ui
  let rightPressOn r =
        inp0
          { inputMousePos = centerOf r
          , inputMouseRightDown = True
          , inputMouseRightPressed = True
          }
      rightReleaseOn r p =
        p
          { inputMousePos = centerOf r
          , inputMouseRightPressed = False
          , inputMouseRightDown = False
          , inputMouseRightReleased = True
          }

  -- Right press on the button, release over the menu area: no menu.
  _ <- runFrame ctx (rightPressOn a) ui
  ((aUp, _, menuUp), _, _, _) <- runFrame ctx (rightReleaseOn lbl (rightPressOn a)) ui
  assert failed (not (respRightClicked aUp))
  assert failed (isNothing menuUp)

  -- Right press and release inside the area: the menu opens.
  _ <- runFrame ctx (rightPressOn lbl) ui
  ((_, _, menuSame), _, _, _) <- runFrame ctx (rightReleaseOn lbl (rightPressOn lbl)) ui
  assert failed (isJust menuSame)

-- | Leaving a widget mid-press and coming back still clicks it, and a plain
-- press-release on one widget is unaffected.
runReleaseReturnsTest :: Context -> IORef Int -> IO ()
runReleaseReturnsTest ctx failed = do
  let inp0 = withInput 320 240
      ui = column $ do
        a <- button' "Alpha"
        b <- button' "Beta"
        pure (a, b)
  (a, b) <- warmup2 ctx inp0 ui
  let at r = inp0 {inputMousePos = centerOf r}
      pressOn r = (at r) {inputMouseDown = True, inputMousePressed = True}
      moveTo r p = p {inputMousePos = centerOf r, inputMousePressed = False}
      releaseOn r p = (moveTo r p) {inputMouseDown = False, inputMouseReleased = True}

  -- Straight click.
  _ <- runFrame ctx (pressOn a) ui
  ((aUp, _), _, _, _) <- runFrame ctx (releaseOn a (pressOn a)) ui
  assert failed (respClicked aUp)

  -- Wander off and back before letting go.
  _ <- runFrame ctx (pressOn b) ui
  _ <- runFrame ctx (moveTo a (pressOn b)) ui
  ((_, bBack), _, _, _) <- runFrame ctx (releaseOn b (pressOn b)) ui
  assert failed (respClicked bBack)
  void (runFrame ctx inp0 ui)

-- | Where two widgets overlap, a held press belongs to the one hover lights
-- up: the earlier sibling, which paints on top.
runOverlapPressTest :: Context -> IORef Int -> IO ()
runOverlapPressTest ctx failed = do
  let inp0 = withInput 320 240
      ui = rowWith (gap (-30)) $ do
        a <- buttonWith' (fixedW 80) "Alpha"
        b <- buttonWith' (fixedW 80) "Beta"
        pure (a, b)
  (a, _) <- warmup2 ctx inp0 ui
  let Rect ax ay aw ah = respRect a
      overlap = inp0 {inputMousePos = V2 (ax + aw - 10) (ay + ah / 2)}
      press = overlap {inputMouseDown = True, inputMousePressed = True}
      held' = overlap {inputMouseDown = True}
  _ <- runFrame ctx overlap ui
  hot <- getHotId ctx
  assert failed (hot == respId a)
  _ <- runFrame ctx press ui
  ((aHeld, bHeld), _, _, _) <- runFrame ctx held' ui
  assert failed (respPressed aHeld && not (respPressed bHeld))
  void (runFrame ctx (overlap {inputMouseReleased = True}) ui)
  void (runFrame ctx inp0 ui)
