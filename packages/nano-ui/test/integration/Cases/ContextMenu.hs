module Cases.ContextMenu (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Maybe (isJust, listToMaybe)
import Data.Text qualified as T
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ simSpec "context-menu-open" 640 480 menuUi contextMenuOpen
  , simSpec "context-menu-scroll-pos" 200 200 scrolledMenuUi contextMenuScrollPos
  , simSpec "context-menu-rows" 640 480 rowsUi contextMenuRows
  , simSpec "context-menu-taller-than-window" 400 300 tallUi contextMenuTall
  , simSpec "menu-button-popup-toggle" 400 300 dropDownUi menuButtonToggle
  , spec "popup-point-flip" runPopupPointFlipTest
  , spec "popup-anchor-moved" runPopupAnchorMovedTest
  ]

-- | Right-click the target and let the menu appear.
openOn :: Target -> Sim a ()
openOn t = rightClick t >> void settle

menuUi :: NanoUI (Response, Maybe (Response, Response))
menuUi = column $ do
  btn <- button' "Target Button"
  mInside <- contextMenu btn ((,) <$> menuItem' "Cut" <*> menuItem' "Copy")
  pure (btn, mInside)

-- | A right click opens the menu on its release; a left click or a right
-- press outside dismisses it.
contextMenuOpen :: Sim (Response, Maybe (Response, Response)) ()
contextMenuOpen = do
  checkEq False . isJust . snd =<< result
  (down, _) <- pressButton MouseRight "Target Button"
  check (not (respRightClicked down))
  (up, inside) <- releaseButton MouseRight
  check (respRightClicked up && isJust inside)
  checkEq False . isJust . snd =<< click (At (V2 500 400))
  openOn "Target Button"
  _ <- pressButton MouseRight (At (V2 500 400))
  checkEq False . isJust . snd =<< releaseButton MouseRight

scrolledMenuUi :: NanoUI (WidgetId, (Response, Maybe Bool))
scrolledMenuUi = scrollArea (fillW . fixedH 80) $ column $ do
  replicateM_ 40 (label "pad line")
  btn <- button' "Menu Target"
  cut <- contextMenu btn (menuItem "Scroll Cut")
  replicateM_ 12 (label "tail line")
  pure (btn, cut)

-- | A menu opened in a scrolled area appears at the pointer, not at the
-- target's unscrolled layout position, and picking a row closes it.
contextMenuScrollPos :: Sim (WidgetId, (Response, Maybe Bool)) ()
contextMenuScrollPos = do
  (sid, _) <- result
  ctx <- simContext
  let scrollToTarget n = do
        (_, (btn, _)) <- result
        Rect _ sy _ sh <- maybe (Rect 0 0 0 0) id <$> liftIO (getPrevRect ctx sid)
        let Rect _ by _ bh = respRect btn
        unless (n == (0 :: Int) || (by >= sy + 4 && by + bh + 8 <= sy + sh)) $
          wheel (OnWidget sid) (V2 0 1) >> settle >> scrollToTarget (n - 1)
  scrollToTarget 200
  (_, (btn, _)) <- result
  off <- liftIO (getScrollOffset ctx sid)
  checkGt off 0
  let p = centerOf btn
  openOn (At p)
  cut <- rectOf "Scroll Cut"
  check (abs (rectY cut - v2Y p) <= 16)
  check (abs (rectY cut - v2Y p) < abs (rectY cut - (rectY (respRect btn) + off)))
  checkEq (Just True) . snd . snd =<< click (Within cut)
  _ <- frame
  expectNot "Scroll Cut"

rowsUi :: NanoUI Response
rowsUi = column $ do
  btn <- button' "Target Button"
  _ <- contextMenu btn $ do
    menuHeader "Row Header"
    _ <- menuItem "Row Cut"
    menuItemDisabled "Row Paste"
    _ <- menuItem "Row Undo"
    menuSeparator
    menuItem "Row Redo"
  pure btn

-- | A disabled row lines up with the enabled rows around it. A click on a
-- header, a disabled row or a separator keeps the menu open, and a click on
-- a row closes it.
contextMenuRows :: Sim Response ()
contextMenuRows = do
  openOn "Target Button"
  [header, cut, paste, undo, redo] <- mapM rectOf ["Row Header", "Row Cut", "Row Paste", "Row Undo", "Row Redo"]
  check (abs (rectX paste - rectX cut) < 0.5)
  check (abs ((rectY paste - rectY cut) - (rectY undo - rectY paste)) < 0.5)
  let stillOpenAfter t = do
        openOn "Target Button"
        _ <- click t
        _ <- frame
        isJust <$> findSpan "Row Redo"
  check =<< stillOpenAfter (Within header)
  check =<< stillOpenAfter (Within paste)
  check =<< stillOpenAfter (At (V2 (rectX redo + 2) ((rectY undo + rectH undo + rectY redo) / 2)))
  check . not =<< stillOpenAfter (Within cut)

tallUi :: NanoUI (Response, Maybe Bool)
tallUi = column $ do
  btn <- button' "Target Button"
  lastPicked <- contextMenu btn (last <$> mapM (\i -> menuItem (T.pack ("Recent " <> show i))) [1 .. 40 :: Int])
  pure (btn, lastPicked)

-- | A menu taller than the window stays inside it and scrolls, so its last
-- row can be reached and picked.
contextMenuTall :: Sim (Response, Maybe Bool) ()
contextMenuTall = do
  openOn "Target Button"
  first <- rectOf "Recent 1"
  replicateM_ 20 (wheel (Within first) (V2 0 3))
  _ <- frame
  lastRow <- rectOf "Recent 40"
  check (rectY lastRow + rectH lastRow <= 300)
  checkEq (Just True) . snd =<< click (Within lastRow)

-- | A menu title toggling its drop-down on 'respClicked'. Returns the title
-- and whether the drop-down was open going into the frame.
dropDownUi :: NanoUI (Response, Bool)
dropDownUi = column $ do
  openVar <- useVar False
  open <- readVar openVar
  btn <- menuButton' "File" open
  when (respClicked btn) (writeVar openVar (not open))
  let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
  (popR, _) <- popup' (constVar open) cfg (menuItem "New")
  when (respClicked popR) (writeVar openVar False)
  pure (btn, open)

-- | Clicking the title of an open drop-down closes it: the press on the title
-- is not a press outside the popup, which would close it for the release to
-- open it again. A click elsewhere still closes it.
menuButtonToggle :: Sim (Response, Bool) ()
menuButtonToggle = do
  title <- On . fst <$> result
  opens <- mapM (\t -> click t >> snd <$> settle) [title, title, title, At (V2 380 280)]
  checkEq [True, False, True, False] opens

-- | A popup placed before or after a point flips to the other side when it
-- would run off the window there and fits on the other side, and it always
-- stays within the window margin.
runPopupPointFlipTest :: Context -> IORef Int -> IO ()
runPopupPointFlipTest _ failed = do
  -- A 100 by 80 popup in an 800 wide window, with a margin of 14.
  let place winH px py = computePopupPosition 800 winH 14 100 80 (AnchorPoint (V2 px py))
  assertEq failed (snd (place 600 400 30 PlacementAbove 4)) 34
  assertEq failed (fst (place 600 30 300 PlacementLeft 4)) 34
  assertEq failed (snd (place 600 400 590 PlacementBelow 20)) 490
  -- Neither side has room: it keeps within the margin rather than flipping.
  assertEq failed (snd (place 200 400 110 PlacementBelow 20)) (200 - 80 - 14)

-- | A popup anchored to a button follows it when a label beside the button
-- grows on a frame nothing else follows up: that frame asks for the one that
-- places the popup at the button's new rect.
runPopupAnchorMovedTest :: Context -> IORef Int -> IO ()
runPopupAnchorMovedTest ctx failed = do
  status <- newIORef ("Loading" :: T.Text)
  let ui = do
        r <- row (liftIO (readIORef status) >>= label >> button' "Filter")
        popup (constVar True) (defaultPopupConfig (AnchorRect (respRect r))) {cfgDismissable = False} (label "menu")
        pure r
      popupX = maybe 0 rectX . listToMaybe . IM.elems <$> liftIO (floatingPanelRects ctx)
  runSim ctx failed (withInputOff 640 300) ui $ do
    before <- result
    x0 <- popupX
    liftIO (writeIORef status "Loaded 1,234 items")
    _ <- frame
    check =<< wantsFrame
    after <- settle
    check . not =<< wantsFrame
    x1 <- popupX
    checkGt (rectX (respRect after)) (rectX (respRect before))
    checkEq (rectX (respRect after) - rectX (respRect before)) (x1 - x0)
