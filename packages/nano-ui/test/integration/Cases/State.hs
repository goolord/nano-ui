module Cases.State (runControlledStateTest, runHookStateTest, runCollectionApiTest) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import NanoUI
import NanoUI.Context (Context (..), getStore, intKey, registerImages, lookupImageUv)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Testing (clearDirty, isDirty, runFrame)
import NanoUI.Testing.Assert (assertEq)
import NanoUI.Testing.Harness (withInputOff)

runCollectionApiTest :: Context -> IORef Int -> IO ()
runCollectionApiTest ctx failed = do
  seen <- newIORef []
  _ <- runFrame ctx (withInputOff 300 100) $
    hstack (V.fromList [uiIO (modifyIORef' seen (key :)) | key <- [7, 2, 9 :: Int]])
  assertEq failed [9, 2, 7] =<< readIORef seen
  ((emptySelect, emptyRadio, combo), _, _, _) <- runFrame ctx (withInputOff 300 200) $
    withKey ("collection-options" :: Text) $ column $ do
      (_, selectIndex) <- select (V.empty :: V.Vector Text) 5
      (_, radioIndex) <- radioFieldset (Seq.empty :: Seq.Seq Text) (-1)
      (_, comboValue) <- comboBox "Choose" (Seq.fromList ["Alpha", "Beta"]) "Beta"
      pure (selectIndex, radioIndex, comboValue)
  assertEq failed 0 emptySelect
  assertEq failed 0 emptyRadio
  assertEq failed "Beta" combo
  -- A failed image must not prevent later registrations in traversal order.
  ok <- registerImages ctx (Seq.fromList [(ImageId 0, 1, 1, BS.replicate 4 255), (ImageId 42, 1, 1, BS.replicate 4 255)])
  assertEq failed False ok
  registered <- lookupImageUv ctx (ImageId 42)
  assertEq failed True (case registered of Just _ -> True; Nothing -> False)

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

runHookStateTest :: Context -> IORef Int -> IO ()
runHookStateTest ctx failed = do
  let
    inp = withInputOff 300 100
    check :: (Eq a, Show a) => Text -> NanoUI (a, a -> NanoUI ()) -> a -> a -> IO ()
    check key hook initial changed = do
      let
        evaluate = runNanoUI ctx inp (withKey key hook)
      (value, setValue) <- evaluate
      assertEq failed initial value
      clearDirty ctx
      runNanoUI ctx inp (setValue initial)
      assertEq failed False =<< isDirty ctx

      runNanoUI ctx inp (setValue changed)
      assertEq failed True =<< isDirty ctx
      assertEq failed changed . fst =<< evaluate

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
  check "table-sort" (useTableSort (SortCol 0 SortAsc)) (SortCol 0 SortAsc) (SortCol 2 SortDesc)
