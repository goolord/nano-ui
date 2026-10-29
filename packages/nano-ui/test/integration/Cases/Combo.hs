{-# OPTIONS_GHC -O0 #-}
module Cases.Combo (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Input (onMac)
import NanoUI.Shortcut
import NanoUI.Testing.Sim
import NanoUI.Widgets.Combo (ComboInput (..), ComboState (..), ComboStep (..), comboStep)

tests :: [Spec]
tests =
  [ spec "combo-pick-and-commit" runComboPickTest
  , spec "combo-dropdown-placement" runComboDropdownPlacementTest
  , spec "combo-dropdown-scrolled-away" runComboDropdownScrolledAwayTest
  , spec "combo-escape-owned" runComboEscapeOwnedTest
  , simSpec "combo-scrollbar-drag" 320 300 (withValue "" (comboBox' "Fonts" (famOpts 12))) comboScrollbarDrag
  , spec "combo-track-press-holds" runComboTrackPressHoldsTest
  , simSpec "combo-wheel" 200 360 (withValue "" (comboBox' "Fonts" (famOpts 20 ++ [longOpt]))) comboWheel
  ]

comboOpts :: [T.Text]
comboOpts = ["Alpha Sans", "Beta Serif", "Gamma Mono", "Delta Round", "Straße Sans"]

-- | Options numbered with zero padding, so no name is a substring of another.
famOpts :: Int -> [T.Text]
famOpts n = ["Fam " <> (if i < 10 then "0" else "") <> T.pack (show i) | i <- [1 .. n]]

longOpt :: T.Text
longOpt = "A Very Long Font Family Name That Overflows"

type Combo = (Response, T.Text)

-- | Whether the frame committed, and the text.
commitOf :: Combo -> (Bool, T.Text)
commitOf (r, t) = (respChanged r, t)

-- | Each script drives a fresh combo over 'comboOpts' holding its text.
pickCases :: [(T.Text, Sim Combo ())]
pickCases =
  [ -- Typing filters the list, folding case so "STRASSE" finds "Straße",
    -- and never picks anything on its own: Enter with no highlight leaves
    -- the typed text alone.
    ("", focused >> typeText "ga" >> expect "Gamma Mono" >> expectNot "Alpha Sans" >> enterLeaves "ga")
  , ("", focused >> typeText "STRASSE" >> expect "Straße Sans" >> expectNot "Beta Serif")
  , -- Down from nothing highlights the first row and Enter commits the
    -- highlight.
    ( ""
    , do
        _ <- focused >> pressKey KeyDown >> pressKey KeyDown
        enterCommits (True, "Beta Serif")
        expect "Beta Serif"
    )
  , -- A click on a row commits it and blurs the field, which closes the list.
    ( ""
    , do
        checkEq (True, "Beta Serif") . commitOf =<< (focused >> click "Beta Serif")
        checkEq (WidgetId 0) =<< (simContext >>= liftIO . getFocusId)
        expectNot "Alpha Sans"
    )
  , -- Hovering a row highlights it, with the pointer cursor and the hover
    -- background, but commits nothing. The arrows then move the highlight
    -- from it, a pointer that has not moved does not pull it back, and Enter
    -- commits the row the keys reached.
    ( ""
    , do
        _ <- focused >> moveTo "Beta Serif"
        checkEq (False, "") . commitOf =<< frame
        checkEq UiCursorPointer =<< pointerCursor
        ss <- spans
        let bgFor needle = [bg | (_, txt, _, bg, _) <- ss, needle `T.isInfixOf` txt]
        check (length (bgFor "Beta Serif") == 1 && bgFor "Beta Serif" /= bgFor "Alpha Sans")
        _ <- pressKey KeyDown >> frame
        enterCommits (True, "Gamma Mono")
    )
  , -- Unfocused, the combo is a search field showing its value, with no
    -- list. Escape cancels an edit: the text reverts to the last commit with
    -- no commit pulse, and the field blurs, closing the list.
    ( "Inter"
    , do
        expect "Inter" >> expectNot "Alpha Sans"
        _ <- focused >> typeText "No"
        checkEq (False, "Inter") . commitOf =<< pressKey KeyEscape
        checkEq (WidgetId 0) =<< (simContext >>= liftIO . getFocusId)
        expectNot "Alpha Sans"
    )
  , -- Typing edits the text without committing, word-wise keys
    -- (Ctrl+Backspace, Option+Backspace on macOS) work as in a text input,
    -- and the frame after a blur commits the text.
    ( ""
    , do
        checkEq [(False, "N"), (False, "No")] . map commitOf =<< (focused >> mapM typeText ["N", "o"])
        _ <- typeText " bar" >> chord ((if onMac then alt else ctrl) <> key KeyBackspace)
        checkEq (False, "No ") . commitOf =<< frame
        _ <- pressKey KeyBackspace >> press (At (V2 310 5))
        checkEq [(True, "No"), (False, "No")] . map commitOf =<< sequence [release, frame]
    )
  ]
  where
    focused = pressKey KeyTab >> settle
    enterCommits want = checkEq want . commitOf =<< pressKey KeyEnter
    enterLeaves txt = checkEq txt . snd =<< pressKey KeyEnter

runComboPickTest :: Context -> IORef Int -> IO ()
runComboPickTest _ failed = do
  forM_ pickCases $ \(initial, script) -> do
    ctx <- newContext
    ref <- newVar initial
    runSim ctx failed (withInput 320 200) (column (held ref (comboBox' "Font" comboOpts))) script
  -- A value the application writes after a commit shows, but is no commit.
  ctx <- newContext
  ref <- newVar ""
  runSim ctx failed (withInput 320 200) (column (held ref (comboBox' "Font" comboOpts))) $ do
    check . respChanged . fst =<< (pressKey KeyTab >> pressKey KeyDown >> pressKey KeyEnter)
    liftIO (writeVarIO ref "Reset")
    checkEq (False, "Reset") . commitOf =<< frame

-- | A combo near the window's bottom lists its options above itself, where
-- they fit, and a click there picks the row under it. A list that fits on
-- neither side is not slid back over the field, so the click that focuses
-- the combo commits nothing.
runComboDropdownPlacementTest :: Context -> IORef Int -> IO ()
runComboDropdownPlacementTest _ failed = do
  let below n opts = column (replicateM_ n (label "line") >> withValue "" (comboBox' "Font" opts))
  flips <- newContext
  runSim flips failed (withInput 320 200) (below 6 comboOpts) $ do
    (resp, _) <- pressKey KeyTab >> settle
    beta <- rectOf "Beta Serif"
    check (rectY beta + rectH beta <= rectY (respRect resp))
    checkEq "Beta Serif" . snd =<< click (Within beta)
  tall <- newContext
  runSim tall failed (withInput 320 300) (below 4 (famOpts 12)) $ do
    (resp, _) <- result
    checkEq "" . snd =<< click (On resp)
    checkEq (respId resp) =<< liftIO (getFocusId tall)

-- | A focused combo whose field scrolls out of view shows no list, and the
-- pointer where the list would be highlights no row for Enter to commit.
runComboDropdownScrolledAwayTest :: Context -> IORef Int -> IO ()
runComboDropdownScrolledAwayTest ctx failed = do
  let field = withValue "" (comboBox' "Font" comboOpts) <* replicateM_ 30 (label "line")
  runSim ctx failed (withInput 320 300) (column (label "toolbar" >> scrollArea (fixedWH 300 120) field)) $ do
    (sid, (resp, _)) <- result
    _ <- click (On resp)
    V2 cx cy <- spanCenter <$> rectOf "Beta Serif"
    -- Scrolled just past its field, the list would lie in the window.
    let past = rectH (respRect resp) + 20
    liftIO (setScrollOffset ctx sid past)
    _ <- frame >> frame
    expectNot "Beta Serif"
    _ <- moveTo (At (V2 cx (cy - past)))
    checkEq "" . snd . snd =<< pressKey KeyEnter

-- | Escape on a focused combo showing its list cancels the combo and nothing
-- else, wherever another Escape consumer is declared: a popup's dismiss, a
-- shortcut or a key listener, before it or after it, leaves it alone. So do
-- the listeners while the Escape is held, repeating, and on its release.
runComboEscapeOwnedTest :: Context -> IORef Int -> IO ()
runComboEscapeOwnedTest _ failed =
  forM_ [(w, b) | w <- ["popup", "shortcut", "keyPressed", "keyReleased", "keyHeld"], b <- [True, False]] $
    \(which, before) -> do
      ctx <- newContext
      fired <- newIORef False
      popupOpen <- newVar True
      let listen k = whenM (k KeyEscape) (liftIO (writeIORef fired True))
          other :: NanoUI ()
          other = case which of
            "popup" -> popup popupOpen (defaultPopupConfig (AnchorPoint (V2 300 180))) (label "Panel")
            "shortcut" -> whenM (shortcut (key KeyEscape)) (liftIO (writeIORef fired True))
            "keyPressed" -> listen keyPressed
            "keyReleased" -> listen keyReleased
            _ -> listen keyHeld
          ui = column (when before other >> withValue "" (comboBox' "Font" comboOpts) <* unless before other)
      runSim ctx failed (withInput 320 200) ui $ do
        _ <- pressKey KeyTab >> typeText "a" >> frameWith (chordInp (key KeyEscape))
        _ <- frameWith (keyRepeatInp KeyEscape) >> frameWith (keyUpInp KeyEscape)
        acted <- liftIO ((||) <$> readIORef fired <*> (not <$> readVarIO popupOpen))
        checkEq (which :: T.Text, before, False) (which, before, acted)
        checkEq (WidgetId 0) =<< liftIO (getFocusId ctx)

-- | Dragging the list's scrollbar thumb scrolls it, and releasing the drag
-- over a row commits nothing.
comboScrollbarDrag :: Sim Combo ()
comboScrollbarDrag = do
  (resp, _) <- pressKey KeyTab >> settle
  expect "Fam 01"
  -- The list opens 4 px below the field, its 10 px scrollbar lane at its
  -- right, the thumb at the lane's top.
  let Rect rx ry rw rh = respRect resp
      trackX = rx + rw - 5
      dropY = ry + rh + 4
  checkEq (False, "") . commitOf =<< drag (At (V2 trackX (dropY + 10))) (At (V2 trackX (dropY + 200))) 1
  expectNot "Fam 01" >> expect "Fam 12"

-- | A press on the scrollbar track below the thumb jumps the list there, and
-- holding the button still keeps the list where the jump put it.
runComboTrackPressHoldsTest :: Context -> IORef Int -> IO ()
runComboTrackPressHoldsTest _ failed = do
  let fresh = ComboState (-1) 0 0 50 0 0 0 "" "" True Nothing
      rows = [T.pack (show i) | i <- [1 .. 100 :: Int]]
      step inp = stepState . comboStep (ComboInput True False "" rows 50 (Rect 0 0 200 30) inp)
      -- The list opens 4 px below the field; its scrollbar lane is its
      -- right-most 10 px.
      onTrack = V2 195 (34 + 100)
      pressed = step (pressAt (withInput 400 400) onTrack) fresh
  assertGt failed (csWindow pressed) 0
  assertEq failed (csWindow pressed) (csWindow (step (holdAt (withInput 400 400) onTrack) pressed))

-- | The wheel over the open list scrolls it: a notch three rows, and a
-- trackpad's eighths of a notch add up to as much. A horizontal wheel shifts
-- rows that overflow the list's width, clamped so a huge one keeps them in
-- reach. Scrolled to its end, a wheel moves nothing and asks for no frame.
comboWheel :: Sim Combo ()
comboWheel = do
  _ <- pressKey KeyTab >> settle
  expect "Fam 01" >> expectNot "Fam 09"
  Rect x0 y0 _ h0 <- rectOf "Fam 01"
  let over = At (V2 (x0 + 4) (y0 + h0 / 2))
  _ <- wheel over (V2 0 1)
  expectNot "Fam 01" >> expect "Fam 04" >> expect "Fam 11"
  replicateM_ 8 (frameWith (\i -> i {inputScroll = V2 0 0.125}))
  expectNot "Fam 06" >> expect "Fam 07"
  before <- rectOf "Fam 07"
  _ <- frameWith (\i -> i {inputScroll = V2 5 0})
  checkLt 50 . (rectX before -) . rectX =<< rectOf "Fam 07"
  _ <- frameWith (\i -> i {inputScroll = V2 1000 0})
  expect "Fam 07"
  _ <- frameWith (\i -> i {inputScroll = V2 0 10}) >> frame
  _ <- frameWith (\i -> i {inputScroll = V2 0 1})
  check . not =<< wantsFrame
  expect "Fam 20"
