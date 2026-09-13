module Cases.State (runControlledStateTest, runCheckboxEmitKeyboardTest, runHookStateTest) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import NanoUI
import NanoUI.Context (Context (..), getStore, intKey)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Testing (clearDirty, decodeMessages, isDirty, runFrame)
import NanoUI.Testing.Assert (assertEq)
import NanoUI.Testing.Harness (withInputOff)

runControlledStateTest :: Context -> IORef Int -> IO ()
runControlledStateTest ctx failed = do
  callbacks <- newIORef []
  let
    inp = withInputOff 300 200
    ui checked text value = column $ do
      expectedId <- currentId
      check <-
        checkboxControlled
          "Controlled"
          checked
          (\v -> uiIO (modifyIORef' callbacks (<> [v])))
      field <- textInputControlled text (const (pure ()))
      range <- sliderControlled 0 100 value (const (pure ()))
      pure (expectedId, check, field, range)
  _ <- runFrame ctx inp (ui True "initial" 25)
  ((expectedId, check, field, range), _, _, _) <-
    runFrame ctx inp (ui False "replacement" 75)
  assertEq failed expectedId (respId check)
  store <- getStore ctx
  assertEq failed (Just 0) (IM.lookup (intKey (respId check)) (storeInt store))
  assertEq
    failed
    (Just "replacement")
    (IM.lookup (intKey (respId field)) (storeText store))
  assertEq failed (Just 75) (IM.lookup (intKey (respId range)) (storeFloat store))
  assertEq failed [] =<< readIORef callbacks

  -- Keyboard activation notifies the owner, which may decline the change.
  writeIORef (ctxFocusId ctx) (respId check)
  _ <-
    runFrame
      ctx
      (inp {inputKeys = inputKeysFromList [KeyEnter]})
      (ui False "replacement" 75)
  assertEq failed [True] =<< readIORef callbacks
  _ <- runFrame ctx inp (ui False "replacement" 75)
  settled <- getStore ctx
  assertEq failed (Just 0) (IM.lookup (intKey (respId check)) (storeInt settled))
  assertEq failed [True] =<< readIORef callbacks

runCheckboxEmitKeyboardTest :: Context -> IORef Int -> IO ()
runCheckboxEmitKeyboardTest ctx failed = do
  let
    inp = withInputOff 300 100
    ui = checkboxEmit "Emit" False id
  (resp, _, _, _) <- runFrame ctx inp ui
  writeIORef (ctxFocusId ctx) (respId resp)
  (_, messages, _, _) <-
    runFrame ctx (inp {inputKeys = inputKeysFromList [KeyEnter]}) ui
  assertEq failed [True] (decodeMessages messages :: [Bool])

runHookStateTest :: Context -> IORef Int -> IO ()
runHookStateTest ctx failed = do
  let
    inp = withInputOff 300 100
    check :: Eq a => Text -> NanoUI (a, a -> NanoUI ()) -> a -> a -> IO ()
    check key hook initial changed = do
      let
        evaluate = runNanoUI ctx inp (withKey key hook)
      (value, setValue) <- evaluate
      assertEq failed initial value
      clearDirty ctx
      before <- getStore ctx
      runNanoUI ctx inp (setValue initial)
      assertEq failed False =<< isDirty ctx
      unchanged <- getStore ctx
      assertEq failed (storeMirrorGen before) (storeMirrorGen unchanged)

      runNanoUI ctx inp (setValue changed)
      assertEq failed True =<< isDirty ctx
      assertEq failed changed . fst =<< evaluate
      after <- getStore ctx
      assertEq failed (storeMirrorGen before + 1) (storeMirrorGen after)

      -- Reuse the original setter: comparing against its captured initial
      -- value would incorrectly discard this update back to the initial.
      runNanoUI ctx inp (setValue initial)
      assertEq failed initial . fst =<< evaluate
  check "int" (useInt 0) 0 12
  check "float" (useFloat 0) 0 1.5
  check "text" (useText "initial") "initial" "changed"
  check "flag" (useFlag False) False True
  check "enum" (useEnum LT) LT GT
  check "dynamic" (useState (0 :: Int, False)) (0, False) (12, True)
