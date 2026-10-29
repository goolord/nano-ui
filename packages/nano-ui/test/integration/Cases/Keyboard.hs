{-# OPTIONS_GHC -O0 #-}
module Cases.Keyboard (tests) where

import Spec
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "keyboard-disabled" runKeyboardDisabledTest
  , simSpec "keyboard-modal-eligibility" 400 300 modalEligibilityUi keyboardModalEligibility
  , spec "keyboard-focused-controls" runFocusedControlsTest
  , simSpec "keyboard-focus-ring" 300 160 (column (button' "Go")) keyboardFocusRing
  ]

-- | Retaining focus while a control becomes disabled must not bypass the
-- guard pointer interaction uses, through the shared key-navigation hook:
-- Enter, Space, an arrow and typing change nothing.
runKeyboardDisabledTest :: Context -> IORef Int -> IO ()
runKeyboardDisabledTest _ failed =
  forM_ everyControl $ \(name, widget) -> forM_ keys $ \(keyName, pressed) -> do
    ctx <- newContext
    ((resp, before), _, _) <- runFrame ctx inp widget
    setFocus ctx (respId resp)
    ((r, after), _, _) <- runFrame ctx (pressed inp) (disabledWhen True widget)
    assertEq failed (name, keyName, before, False) (name, keyName, after, respChanged r || respClicked r)
  where
    inp = withInputOff 300 160
    keys :: [(String, Input -> Input)]
    keys = [("Enter", keyInp KeyEnter), ("Space", spaceInp), ("Right", keyInp KeyRight), ("x", typed)]
    typed i = i {inputChars = "x"}

modalEligibilityUi :: NanoUI ((Response, Bool), Maybe (Response, Bool))
modalEligibilityUi = column $ do
  outside <- withValue False (checkbox' "Outside")
  (_, inside) <- modal' (constVar True) "Modal" (withValue False (checkbox' "Inside"))
  pure (outside, inside)

-- | Behind a modal a focused checkbox ignores Enter; inside it, it toggles.
keyboardModalEligibility :: Sim ((Response, Bool), Maybe (Response, Bool)) ()
keyboardModalEligibility = do
  ((outside, _), inside) <- result
  focusOn (respId outside)
  check . not . snd . fst =<< pressKey KeyEnter
  checkJust inside $ \(resp, _) -> do
    focusOn (respId resp)
    check . maybe False snd . snd =<< pressKey KeyEnter

-- | A Space press, which also types a space.
spaceInp :: Input -> Input
spaceInp inp = (keyInp KeySpace inp) {inputChars = " "}

-- | Focused by Tab, buttons activate with Enter and Space, checkboxes and
-- toggles flip with them (a toggle on a click too), a slider steps with the
-- arrows, a radio group moves its choice with them, and Enter on a tab
-- header opens its tab.
runFocusedControlsTest :: Context -> IORef Int -> IO ()
runFocusedControlsTest _ failed = do
  let run :: NanoUI a -> Sim a () -> IO ()
      run ui script = do
        ctx <- newPixelContext
        runSim ctx failed (withInputOff 300 160) (column ui) (pressTab 1 >> script)
      space = frameWith spaceInp
      after = fmap (map snd) . mapM pressKey
  run ((,) <$> button "A" <*> button "B") $ do
    checkEq [True, True] . map fst =<< sequence [pressKey KeyEnter, space]
    checkEq True . snd =<< (pressTab 1 >> pressKey KeyEnter)
  checked <- newVar False
  run (held checked (checkbox' "Opt")) $
    checkEq [True, False] . map snd =<< sequence [space, pressKey KeyEnter]
  value <- newVar 50
  run (held value (slider' 0 100)) $ do
    (v : rest) <- after [KeyRight, KeyLeft, KeyDown, KeyUp]
    checkLt 50 v
    checkEq [50, 49, 50] rest
  choice <- newVar 0
  run (held choice (radio' ["A", "B", "C"])) (checkEq [1, 2, 1] =<< after [KeyDown, KeyDown, KeyUp])
  on <- newVar False
  run (held on (toggleSwitchWith' id)) $ do
    (resp, _) <- result
    checkEq [True, False] . map snd =<< sequence [space, pressKey KeyEnter]
    clicks <- replicateM 2 (click (On resp))
    checkEq [(True, True), (True, False)] [(respClicked r, v) | (r, v) <- clicks]
  let headers = [tab (1 :: Int) "Alpha" (label "BodyA"), tab 2 "Beta" (label "BodyB")]
  run (tabActive <$> tabs' (constVar 1) headers) $ do
    checkEq 1 =<< pressKey KeyEnter
    checkEq 2 =<< (pressTab 1 >> pressKey KeyEnter)

-- | Moving focus with Tab shows the focus ring; a pointer press hides it.
keyboardFocusRing :: Sim Response ()
keyboardFocusRing = do
  check . not =<< focusRing
  pressTab 1
  check =<< focusRing
  _ <- click (At (V2 5 150))
  check . not =<< focusRing
