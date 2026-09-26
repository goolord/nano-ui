module Cases.Keyboard (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import NanoUI.Internal.Context (Context (..), getFocusVisible, intKey)
import NanoUI.Emit qualified as Emit
import NanoUI.Internal.Store (WidgetStore (..))

tests :: [Spec]
tests =
  [ spec "keyboard-disabled" runKeyboardDisabledTest
  , spec "keyboard-modal-eligibility" runKeyboardModalEligibilityTest
  , spec "keyboard-focus-ring" runKeyboardFocusRingTest
  , spec "keyboard-button" runKeyboardButtonTest
  , spec "keyboard-checkbox" runKeyboardCheckboxTest
  , pixelSpec "keyboard-slider" runKeyboardSliderTest
  , spec "keyboard-radio" runKeyboardRadioTest
  , spec "keyboard-toggle" runKeyboardToggleTest
  , spec "keyboard-tab-header" runKeyboardTabHeaderTest
  ]

-- Retaining focus while a widget becomes disabled must not bypass the same
-- guard used by pointer interaction. Exercise the shared key-navigation hook.
runKeyboardDisabledTest :: Context -> IORef Int -> IO ()
runKeyboardDisabledTest _ctx failed = do
  let inp = withInputOff 300 160
      check :: (Eq a, Show a) => NanoUI (Response, a) -> Input -> IO ()
      check widget pressed = do
        ctx <- newContext
        ((resp, before), _, _, _) <- runFrame ctx inp widget
        let wid = respId resp
        st <- getStore ctx
        writeIORef (ctxFocusId ctx) wid
        ((afterResp, after), _, _, _) <- runFrame ctx pressed (disabledWhen True widget)
        assertEq failed after before
        assert failed (not (respChanged afterResp) && not (respClicked afterResp))
        afterStore <- getStore ctx
        assertEq failed
          (IM.lookup (intKey wid) (storeText st))
          (IM.lookup (intKey wid) (storeText afterStore))
  check (checkbox' "Disabled" False) (keyInp KeyEnter inp)
  check (checkbox' "Disabled" False) (spaceInp inp)
  check (slider' 0 100 50) (keyInp KeyRight inp)
  check (toggleSwitchWith' id False) (spaceInp inp)
  check (textInput' "initial") (inp {inputChars = "x"})
  check (textArea' "initial") (inp {inputChars = "x"})
  check (searchInput' "Search" "initial") (inp {inputChars = "x"})
  check (comboBox' "Choose" ["initial", "other"] "initial") (inp {inputChars = "x"})
  check (do r <- button' "Disabled"; pure (r, respClicked r)) (keyInp KeyEnter inp)

runKeyboardModalEligibilityTest :: Context -> IORef Int -> IO ()
runKeyboardModalEligibilityTest ctx failed = do
  let inp = withInputOff 400 300
      ui = column $ do
        outside <- checkbox' "Outside" False
        (_, inside) <- modal True "Modal" (checkbox' "Inside" False)
        pure (outside, inside)
  ((outside, inside), _, _, _) <- runFrame ctx inp ui
  writeIORef (ctxFocusId ctx) (respId (fst outside))
  (((_, outsideValue), _), _, _, _) <- runFrame ctx (keyInp KeyEnter inp) ui
  assert failed (not outsideValue)
  assertJust failed inside $ \(resp, _) -> do
    writeIORef (ctxFocusId ctx) (respId resp)
    ((_, after), _, _, _) <- runFrame ctx (keyInp KeyEnter inp) ui
    assert failed (maybe False snd after)

-- | A frame pressing Space, which types a space as well.
spaceInp :: Input -> Input
spaceInp inp = (keyInp KeySpace inp) {inputChars = " "}

-- | Plain buttons activate with Enter and Space while focused.
runKeyboardButtonTest :: Context -> IORef Int -> IO ()
runKeyboardButtonTest ctx failed = do
  let inp0 = withInputOff 200 120
      ui = column $ do
        a <- button "A"
        b <- button "B"
        pure (a, b)
  warmupFocused ctx inp0 ui
  ((aEnter, _), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed aEnter
  ((aSpace, _), _, _, _) <- runFrame ctx (spaceInp inp0) ui
  assert failed aSpace
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, bEnter), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed bEnter

-- | A focused checkbox toggles with Space and Enter, and 'Emit.emitChanged'
-- emits its new value on keyboard activation.
runKeyboardCheckboxTest :: Context -> IORef Int -> IO ()
runKeyboardCheckboxTest ctx failed = do
  checkedRef <- newIORef False
  let
    inp0 = withInputOff 200 100
    ui = column (held checkedRef (checkbox' "Opt"))
  warmupFocused ctx inp0 ui
  ((_, checked1), _, _, _) <- runFrame ctx (spaceInp inp0) ui
  assert failed checked1
  ((_, checked2), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed (not checked2)
  let
    emitUi = do
      wid <- currentId
      Emit.emitChanged (checkbox "Emit") False id
      pure wid
  (wid, _, _, _) <- runFrame ctx inp0 emitUi
  writeIORef (ctxFocusId ctx) wid
  (_, messages, _, _) <- runFrame ctx (keyInp KeyEnter inp0) emitUi
  assertEq failed [True] (decodeMessages messages :: [Bool])

-- | A focused slider steps with the arrow keys.
runKeyboardSliderTest :: Context -> IORef Int -> IO ()
runKeyboardSliderTest ctx failed = do
  valueRef <- newIORef 50
  let inp0 = withInputOff 300 80
      ui = column (held valueRef (slider' 0 100))
  (_, v0) <- warmup2 ctx inp0 ui
  assertEq failed v0 50
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, v1), _, _, _) <- runFrame ctx (keyInp KeyRight inp0) ui
  assertGt failed v1 50
  ((_, v2), _, _, _) <- runFrame ctx (keyInp KeyLeft inp0) ui
  assertEq failed v2 50
  ((_, v3), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed v3 49
  ((_, v4), _, _, _) <- runFrame ctx (keyInp KeyUp inp0) ui
  assertEq failed v4 50

-- | A focused radio group changes selection with the arrow keys.
runKeyboardRadioTest :: Context -> IORef Int -> IO ()
runKeyboardRadioTest ctx failed = do
  selectedRef <- newIORef 0
  let inp0 = withInputOff 200 160
      ui = column (held selectedRef (radio' ["A", "B", "C"]))
  (_, sel0) <- warmup2 ctx inp0 ui
  assertEq failed sel0 0
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, sel1), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed sel1 1
  ((_, sel2), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed sel2 2
  ((_, sel3), _, _, _) <- runFrame ctx (keyInp KeyUp inp0) ui
  assertEq failed sel3 1

-- | A toggle switch flips with Space and Enter while focused, and on click.
runKeyboardToggleTest :: Context -> IORef Int -> IO ()
runKeyboardToggleTest ctx failed = do
  onRef <- newIORef False
  let inp0 = withInputOff 200 100
      ui = column (held onRef (toggleSwitchWith' id))
  (resp0, v0) <- warmup2 ctx inp0 ui
  assert failed (not v0)
  _ <- runFrame ctx (tabInp inp0) ui
  ((_, v1), _, _, _) <- runFrame ctx (spaceInp inp0) ui
  assert failed v1
  ((_, v2), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed (not v2)
  let (press, release) = clickPair inp0 (centerOf resp0)
  _ <- runFrame ctx press ui
  ((clicked, v3), _, _, _) <- runFrame ctx release ui
  assert failed (respClicked clicked && v3)
  _ <- runFrame ctx press ui
  ((clicked2, v4), _, _, _) <- runFrame ctx release ui
  assert failed (respClicked clicked2 && not v4)

data KB = KBA | KBB
  deriving (Eq, Show)

-- | Tab headers are focusable and switch the active tab with Enter.
runKeyboardTabHeaderTest :: Context -> IORef Int -> IO ()
runKeyboardTabHeaderTest ctx failed = do
  let inp0 = withInputOff 300 100
      ui cur =
        tabs cur
          [ tab KBA "Alpha" (label "BodyA")
          , tab KBB "Beta" (label "BodyB")
          ]
  warmupFocused ctx inp0 (ui KBA)
  (active1, _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) (ui KBA)
  assertEq failed active1 KBA
  _ <- runFrame ctx (tabInp inp0) (ui KBA)
  (active2, _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) (ui KBA)
  assertEq failed active2 KBB

-- Moving focus with Tab shows the focus ring; a pointer press hides it.
runKeyboardFocusRingTest :: Context -> IORef Int -> IO ()
runKeyboardFocusRingTest ctx failed = do
  let inp = withInputOff 300 160
      ui = column (button' "Go")
  _ <- warmup2 ctx inp ui
  assert failed . not =<< getFocusVisible ctx
  _ <- runFrame ctx (tabInp inp) ui
  assert failed =<< getFocusVisible ctx
  let (press, release) = clickPair inp (V2 5 150)
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  assert failed . not =<< getFocusVisible ctx
