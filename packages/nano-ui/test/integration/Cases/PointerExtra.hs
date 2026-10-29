{-# OPTIONS_GHC -O0 #-}
-- | Middle, side and other mouse buttons. A non-left click is reported
-- separately from a plain click, and a held or clicked button belongs to the
-- widget it was pressed on.
module Cases.PointerExtra (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (findNodeM, getNodeRect, getText)
import NanoUI.Runner (shouldRedrawFrame)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "pointer-button-sets" runButtonSetsTest
  , spec "pointer-held-ownership" runHeldOwnershipTest
  , spec "pointer-blocked" runBlockedTest
  , simSpec "pointer-middle-click" 400 300 middleUi middleClicks
  , spec "pointer-mouse-area" runMouseAreaTest
  ]

-- | Button sets track every button, extras by number. Presses and releases
-- appear only in the frame they happen in. Any held button counts as a held
-- pointer, and a press or release frame redraws.
runButtonSetsTest :: Context -> IORef Int -> IO ()
runButtonSetsTest _ failed = do
  let every = [MouseLeft, MouseRight, MouseMiddle, MouseBack, MouseForward, MouseOther 6, MouseOther 32]
      down = foldr (`applyMouseButton` True) emptyInput every
  assertEq failed (buttonsFromList every) (inputButtonsHeld down)
  assertEq failed (buttonsFromList every) (inputButtonsPressed down)
  -- Lists come back in button-number order; numbers 1 to 5 are the named buttons.
  let ordered = [MouseLeft, MouseMiddle, MouseRight, MouseBack, MouseForward, MouseOther 6]
  assertEq failed (ordered <> [MouseOther 32]) (buttonsToList (inputButtonsHeld down))
  assertEq failed ordered (map mouseButtonNumber [1 .. 6])
  -- A button past 32 is not tracked.
  assert failed (buttonsNull (inputButtonsHeld (applyMouseButton (MouseOther 33) True emptyInput)))
  let up = applyMouseButton (MouseOther 6) False (clearEphemeral down)
  assert failed (releasedIn (MouseOther 6) up && not (heldIn (MouseOther 6) up) && heldIn MouseLeft up)
  assert failed (anyButtonReleased up && not (anyButtonPressed up))
  -- The next frame keeps what is held and drops the presses.
  assertEq failed (inputButtonsHeld down) (inputButtonsHeld (clearEphemeral down))
  assert failed (buttonsNull (inputButtonsPressed (clearEphemeral down)))
  let middle = pressWith MouseMiddle emptyInput (V2 20 20)
      back = pressWith MouseBack emptyInput (V2 20 20)
      backUp = releaseWith MouseBack (clearEphemeral back)
  assertEq failed [True, True, False] (map inputPointerHeld [middle, back, backUp])
  -- Whether an idle loop that already saw the input redraws for it again.
  let redrawsAgain i = newContext >>= \idle -> shouldRedrawFrame idle i i False False False
  assertEq failed [True, True, True] =<< mapM redrawsAgain [middle, back, backUp]

-- | A right, middle or extra-button click on a widget is that widget's alone,
-- and not a plain click. A drag from one button onto another holds and clicks
-- neither, and neither does a release on a widget that moved under the press.
runHeldOwnershipTest :: Context -> IORef Int -> IO ()
runHeldOwnershipTest ctx failed = do
  order <- newIORef ["Alpha", "Beta" :: T.Text]
  let ui = row (liftIO (readIORef order) >>= mapM (\n -> withKey n (button' n)))
      (q, h, c) = ((False, False, False), (True, False, False), (False, True, False))
  runSim ctx failed (withInputOff 400 300) ui $ do
    [a, b] <- map On <$> result
    forM_ [MouseMiddle, MouseRight, MouseOther 7] $ \btn -> do
      let flags = map (\r -> (respHeldWith btn r, respClickedWith btn r, respClicked r || respPressed r))
      rs <- sequence
        [pressButton btn b, releaseButton btn, frame, pressButton btn a, moveTo b, releaseButton btn]
      checkEq [[q, h], [q, c], [q, q], [h, q], [q, q], [q, q]] (map flags rs)
      check (respHovered (rs !! 4 !! 1))
      _ <- pressButton btn a
      _ <- liftIO (writeIORef order ["Beta", "Alpha"]) >> frame
      check . not . any (respClickedWith btn) =<< releaseButton btn
      liftIO (writeIORef order ["Alpha", "Beta"]) >> settle

-- | 'mousePressed', 'mouseHeld' and 'mouseReleased' see a button anywhere on
-- the layer. Inside 'disabledWhen', or on the page beside a modal, neither
-- they nor a widget see a middle or side button.
runBlockedTest :: Context -> IORef Int -> IO ()
runBlockedTest ctx failed = do
  mode <- newIORef (False, False)
  let listen = (,,) <$> mousePressed MouseBack <*> mouseHeld MouseBack <*> mouseReleased MouseBack
      ui = do
        (off, covered) <- liftIO (readIORef mode)
        r <- disabledWhen off ((,) <$> buttonWith' (fixedWH 380 280) "Under" <*> listen)
        r <$ modal' (constVar covered) "Cover" (label "on top")
      untouched (r, heard) =
        not (respHovered r || or [respHeldWith b r || respClickedWith b r | b <- [MouseMiddle, MouseBack]])
          && heard == (False, False, False)
  runSim ctx failed (withInputOff 400 300) ui $ do
    Rect x y _ _ <- respRect . fst <$> result
    let corner = At (V2 (x + 4) (y + 4))
    heard <- map snd <$> sequence [pressButton MouseBack corner, frame, releaseButton MouseBack]
    checkEq [(True, True, False), (False, True, False), (False, False, True)] heard
    forM_ [(True, False), (False, True)] $ \m -> do
      _ <- liftIO (writeIORef mode m) >> settle
      rs <- concat <$> mapM (\b -> sequence [pressButton b corner, releaseButton b]) [MouseMiddle, MouseBack]
      check (all untouched rs)

middleUi :: NanoUI (TabResponse Int, Response)
middleUi = do
  bar <- tabBar' (constVar 0) [closableTab 0 "Alpha" (), closableTab 1 "Beta" (), tab 2 "Gamma" ()]
  (bar,) . fst <$> popup' (constVar True) (defaultPopupConfig (AnchorPoint (V2 40 80))) (label "Popup body")

-- | A middle click on a closable tab's header or close button requests a close
-- without selecting the tab; a tab without a close button stays. A middle
-- press outside a popup dismisses it; one inside does not.
middleClicks :: Sim (TabResponse Int, Response) ()
middleClicks = do
  na <- ctxNodeArena <$> simContext
  let isClose = fmap ("\215" `T.isInfixOf`) . getText na
  Just closeX <- liftIO (traverse (getNodeRect na) =<< findNodeM na isClose)
  targets <- sequence [rectOf "Beta", pure closeX, rectOf "Gamma"]
  closed <- mapM (fmap (\(bar, _) -> (tabClosed bar, tabActive bar)) . clickWith MouseMiddle . Within) targets
  checkEq [(Just 1, 0), (Just 0, 0), (Nothing, 0)] closed
  inside <- On . snd <$> result
  let dismisses t = (respClicked . snd <$> pressButton MouseMiddle t) <* releaseButton MouseMiddle
  checkEq [False, True] =<< mapM dismisses [inside, At (V2 380 280)]

-- | A mouse area reports hover and every button over it and its labels, but
-- not over a child button. Content it reveals on hover stays while the pointer
-- is on it, even with a pinned node elsewhere (which turns on pointer
-- covering). A slider dragged inside one keeps the release.
runMouseAreaTest :: Context -> IORef Int -> IO ()
runMouseAreaTest ctx failed = do
  hoveredRef <- newIORef False
  let ui = columnWith tight $ do
        shown <- liftIO (readIORef hoveredRef)
        ((l, b), area) <- mouseArea (fixedWH 300 80) $ do
          l <- label' "Name"
          b <- if shown then Just <$> button' "Delete" else pure Nothing
          pure (l, b)
        liftIO (writeIORef hoveredRef (respHovered area))
        _ <- buttonWith' (pinAt 320 0 . fixedWH 40 20) "pinned"
        volume <- mouseArea (fixedWH 300 80) (label "Volume" >> withValue 50 (slider' 0 100))
        pure (l, b, area, volume)
  runSim ctx failed (withInputOff 400 300) ui $ do
    (l, _, area0, ((s, _), _)) <- result
    (_, _, areaMid, _) <- clickWith MouseMiddle (On l)
    check (respClickedWith MouseMiddle areaMid && not (respClicked areaMid))
    (_, _, areaRight, _) <- pressButton MouseRight (On l) <* releaseButton MouseRight
    check (respHeldWith MouseRight areaRight && respHovered areaRight)
    let beside = V2 250 60
    check (rectContains (respRect area0) beside)
    (_, _, areaLeft, _) <- click (At beside)
    check (respClicked areaLeft && respClickedWith MouseLeft areaLeft)
    (_, Just b, _, _) <- moveTo (On l) >> settle
    rs <- replicateM 4 (moveTo (On b))
    check (all (\(_, b', a, _) -> respHovered a && maybe False respHovered b') rs)
    (_, bClicked, aClicked, _) <- click (On b)
    check (maybe False respClicked bClicked && not (respClicked aClicked))
    forM_ [MouseRight, MouseMiddle, MouseOther 7] $ \btn -> do
      (_, heldChild, heldArea, _) <- pressButton btn (On b)
      (_, clickedChild, clickedArea, _) <- releaseButton btn
      check (maybe False (respHeldWith btn) heldChild && not (respHeldWith btn heldArea))
      check (maybe False (respClickedWith btn) clickedChild && not (respClickedWith btn clickedArea))
    let V2 x y = centerOf s
    (_, _, _, ((_, v), _)) <- press (At (V2 x y)) >> moveTo (At (V2 (x + 40) y))
    check (v > 50)
    (_, _, _, (_, released)) <- release
    check (not (respClicked released || respClickedWith MouseLeft released))
