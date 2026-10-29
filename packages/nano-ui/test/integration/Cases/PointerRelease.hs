{-# OPTIONS_GHC -O0 #-}
-- | A click belongs to the widget its press went down on. Dragging off a
-- widget and letting go over a neighbour must fire nothing.
module Cases.PointerRelease (tests) where

import Spec
import Data.Maybe (isJust)
import Data.Text qualified as T
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "release-elsewhere" runReleaseElsewhereTest
  , spec "release-moved" runReleaseMovedTest
  , spec "release-tap-and-modal" runReleaseTapAndModalTest
  ]

-- | A drag between widgets fires neither, that frame or the next, while one
-- that comes back still clicks; a context menu opens where its right press
-- went down. Of two overlapping widgets the press goes to the one painted on
-- top, and a button clipped by its panel takes no click.
runReleaseElsewhereTest :: Context -> IORef Int -> IO ()
runReleaseElsewhereTest ctx failed = do
  flag <- newVar False
  let ui = column $ do
        a <- button' "Alpha"
        b <- button' "Beta"
        (cb, on) <- held flag (checkbox' "Flag")
        -- The menu area's response comes from a container node, not a leaf.
        (area, menu) <- contextMenuArea (fixedH 60 . fillW) (label' "Area") (const (menuItem "Cut"))
        overlapping <- rowWith (gap (-30)) (mapM (buttonWith' (fixedW 80)) ["Left", "Right"])
        clipped <- panelWith (fixedWH 200 60) (column (mapM button' ["One", "Two", "Three"]))
        pure ([a, b, cb, area] <> overlapping <> drop 2 clipped, on, isJust menu)
  runSim ctx failed (withInput 320 400) ui $ do
    (rs, _, _) <- result
    let on i = On (rs !! i)
        responses (xs, _, _) = xs
    forM_ [(0, 1), (0, 2), (2, 1)] $ \(from, to) -> do
      (over, _, _) <- press (on from) >> moveTo (on to)
      check (not (respHovered (over !! to) || respPressed (over !! to)))
      ups <- sequence [release, frame]
      check (not (or [respClicked r || checked | (rs', checked, _) <- ups, r <- take 3 rs']))
    check . respClicked . (!! 0) . responses =<< click (on 0)
    check . respClicked . (!! 1) . responses =<< (press (on 1) >> moveTo (on 0) >> moveTo (on 1) >> release)
    _ <- pressButton MouseRight (on 0) >> moveTo (on 3)
    (up, _, menuUp) <- releaseButton MouseRight
    check (not (respRightClicked (up !! 0) || menuUp))
    check . (\(_, _, menu) -> menu) =<< rightClick (on 3)
    let Rect lx ly lw lh = respRect (rs !! 4)
        overlap = At (V2 (lx + lw - 10) (ly + lh / 2))
    _ <- moveTo overlap
    checkEq (respId (rs !! 4)) =<< liftIO (getHotId ctx)
    pressed <- map respPressed . take 2 . drop 4 . responses <$> (press overlap >> frame)
    checkEq [True, False] pressed
    _ <- release
    [p] <- map snd <$> liftIO (nodesOfType NodePanel ctx)
    check (not (rectContains p (centerOf (rs !! 6))))
    ups <- sequence [press (on 6), release]
    check (not (or [respHovered t || respClicked t | (xs, _, _) <- ups, t <- drop 6 xs]))

-- | A release where the pressed control moved to on that frame misses it in
-- the view, so the frame posts the click and asks for another to report it.
runReleaseMovedTest :: Context -> IORef Int -> IO ()
runReleaseMovedTest _ failed = do
  sel <- newVar (0 :: Int)
  cur <- newVar (0 :: Int)
  let radioUi = snd <$> held sel (radio' ["one", "two", "three"])
      tabsUi = snd <$> held cur (`tabBar'` [tab 0 "Alpha" (), tab 1 "Beta" ()])
  forM_ [("two", radioUi), ("Beta", tabsUi)] $ \(lbl, body) -> do
    ctx <- newContext
    shift <- newIORef 0
    let ui = column (liftIO (readIORef shift) >>= \d -> columnWith (fixedH d . fillW) (pure ()) >> body)
    runSim ctx failed (withInput 320 240) ui $ do
      V2 px py <- targetPoint (Labelled lbl)
      _ <- press (At (V2 px py))
      liftIO (writeIORef shift 100)
      missed <- frameWith (releaseWith MouseLeft . \i -> i {inputMousePos = V2 px (py + 100)})
      check (missed /= 1)
      check =<< wantsFrame
      checkEq 1 =<< frame

-- | A press and release in one frame clicks the button the user saw, not the
-- keyed one the click moves there. A modal that opens while a page button is
-- held hides it from the release.
runReleaseTapAndModalTest :: Context -> IORef Int -> IO ()
runReleaseTapAndModalTest ctx failed = do
  items <- newVar [1 .. 5 :: Int]
  open <- newVar False
  let ui = column $ do
        xs <- readVar items
        forM_ xs $ \i -> withKey i $
          whenM (button (T.pack ("Delete " <> show i))) (modifyVar items (filter (/= i)))
        modal open "Error" (label "Something failed")
  runSim ctx failed (withInput 320 240) ui $ do
    p <- targetPoint "Delete 3"
    _ <- frameWith (\i -> applyMouseButton MouseLeft False (pressWith MouseLeft i p))
    replicateM_ 2 frame
    checkEq [1, 2, 4, 5] =<< liftIO (readVarIO items)
    _ <- press "Delete 1"
    liftIO (writeVarIO open True)
    _ <- frame >> release >> frame >> frame
    checkEq [1, 2, 4, 5] =<< liftIO (readVarIO items)
