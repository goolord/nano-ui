module Cases.Keyboard (tests) where

import Spec
import Cases.SimExtra (above)
import Data.Hashable (Hashable (..))
import Data.IntMap.Strict qualified as IM
import NanoUI.Internal.Context (Context (..), getFocusVisible, intKey)
import NanoUI.Internal.Store (WidgetStore (..))
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "keyboard-disabled" runKeyboardDisabledTest
  , spec "keyboard-modal-eligibility" runKeyboardModalEligibilityTest
  , spec "keyboard-focused-controls" runFocusedControlsTest
  , simSpec "keyboard-focus-ring" 300 160 (column (button' "Go")) keyboardFocusRing
  ]

-- | Retaining focus while a widget becomes disabled must not bypass the guard
-- pointer interaction uses, through the shared key-navigation hook.
runKeyboardDisabledTest :: Context -> IORef Int -> IO ()
runKeyboardDisabledTest _ failed = do
  let inp = withInputOff 300 160
      check' :: (Eq a, Show a) => NanoUI (Response, a) -> Input -> IO ()
      check' widget pressed = do
        ctx <- newContext
        ((resp, before), _, _) <- runFrame ctx inp widget
        let textOf st = IM.lookup (intKey (respId resp)) (storeText st)
        st <- getStore ctx
        writeIORef (ctxFocusId ctx) (respId resp)
        ((afterResp, after), _, _) <- runFrame ctx pressed (disabledWhen True widget)
        assertEq failed before after
        assert failed (not (respChanged afterResp) && not (respClicked afterResp))
        assertEq failed (textOf st) . textOf =<< getStore ctx
  check' (withValue False (checkbox' "Disabled")) (keyInp KeyEnter inp)
  check' (withValue False (checkbox' "Disabled")) (spaceInp inp)
  check' (withValue 50 (slider' 0 100)) (keyInp KeyRight inp)
  check' (withValue False (toggleSwitchWith' id)) (spaceInp inp)
  check' (withValue "initial" textInput') (inp {inputChars = "x"})
  check' (withValue "initial" textArea') (inp {inputChars = "x"})
  check' (withValue "initial" (searchInput' "Search")) (inp {inputChars = "x"})
  check' (withValue "initial" (comboBox' "Choose" ["initial", "other"])) (inp {inputChars = "x"})
  check' (button' "Disabled" >>= \r -> pure (r, respClicked r)) (keyInp KeyEnter inp)

-- | Behind a modal a focused checkbox ignores Enter; inside it, it toggles.
runKeyboardModalEligibilityTest :: Context -> IORef Int -> IO ()
runKeyboardModalEligibilityTest ctx failed = do
  let ui = column $ do
        outside <- withValue False (checkbox' "Outside")
        (_, inside) <- modal' (constVar True) "Modal" (withValue False (checkbox' "Inside"))
        pure (outside, inside)
  runSim ctx failed (withInputOff 400 300) ui $ do
    ((outside, _), inside) <- result
    focusOn (respId outside)
    check . not . snd . fst =<< pressKey KeyEnter
    checkJust inside $ \(resp, _) -> do
      focusOn (respId resp)
      check . maybe False snd . snd =<< pressKey KeyEnter

-- | A Space press, which also types a space.
spaceInp :: Input -> Input
spaceInp inp = (keyInp KeySpace inp) {inputChars = " "}

data KB = KBA | KBB
  deriving (Eq, Show, Enum)

instance Hashable KB where
  hashWithSalt salt = hashWithSalt salt . fromEnum

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
    above 50 v
    checkEq [50, 49, 50] rest
  choice <- newVar 0
  run (held choice (radio' ["A", "B", "C"])) (checkEq [1, 2, 1] =<< after [KeyDown, KeyDown, KeyUp])
  on <- newVar False
  run (held on (toggleSwitchWith' id)) $ do
    (resp, _) <- result
    checkEq [True, False] . map snd =<< sequence [space, pressKey KeyEnter]
    clicks <- replicateM 2 (click (On resp))
    checkEq [(True, True), (True, False)] [(respClicked r, v) | (r, v) <- clicks]
  let headers = [tab KBA "Alpha" (label "BodyA"), tab KBB "Beta" (label "BodyB")]
  run (tabActive <$> tabs' (constVar KBA) headers) $ do
    checkEq KBA =<< pressKey KeyEnter
    checkEq KBB =<< (pressTab 1 >> pressKey KeyEnter)

-- | Moving focus with Tab shows the focus ring; a pointer press hides it.
keyboardFocusRing :: Sim Response ()
keyboardFocusRing = do
  ctx <- simContext
  let ring = liftIO (getFocusVisible ctx)
  check . not =<< ring
  pressTab 1
  check =<< ring
  _ <- click (At (V2 5 150))
  check . not =<< ring
