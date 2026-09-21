module Cases.State
  ( runControlledInputsTest
  , runControlledStateTest
  , runHookStateTest
  , runCollectionApiTest
  ) where

import Control.Monad (forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Sequence qualified as Seq
import Data.Primitive.SmallArray qualified as SA
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Context (Context (..), getStore, intKey, registerImages, lookupImageUv)
import NanoUI.Internal.Store (WidgetStore (..))
import NanoUI.Testing (clearDirty, collectTextSpans, isDirty, runFrame)
import NanoUI.Testing.Assert (assert, assertEq)
import NanoUI.Testing.Harness (keyInp, tabInp, warmup2, withInputOff)

runCollectionApiTest :: Context -> IORef Int -> IO ()
runCollectionApiTest ctx failed = do
  seen <- newIORef []
  _ <- runFrame ctx (withInputOff 300 100) $
    hstack (SA.smallArrayFromList [uiIO (modifyIORef' seen (key :)) | key <- [7, 2, 9 :: Int]])
  assertEq failed [9, 2, 7] =<< readIORef seen
  ((emptySelect, emptyRadio, combo), _, _, _) <- runFrame ctx (withInputOff 300 200) $
    withKey ("collection-options" :: Text) $ column $ do
      selectIndex <- select (SA.emptySmallArray :: SA.SmallArray Text) 5
      radioIndex <- radio (Seq.empty :: Seq.Seq Text) (-1)
      comboValue <- comboBox "Choose" (Seq.fromList ["Alpha", "Beta"]) "Beta"
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
      (check, checked') <- checkbox' "Controlled" checked
      when (respChanged check) (uiIO (modifyIORef' callbacks (<> [checked'])))
      (field, _) <- textInput' text
      (range, _) <- slider' 0 100 value
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
      (keyInp KeyEnter inp)
      (ui False "replacement" 75)
  assertEq failed [True] =<< readIORef callbacks
  _ <- runFrame ctx inp (ui False "replacement" 75)
  settled <- getStore ctx
  assertEq failed (Just 0) (IM.lookup (intKey (respId check)) (storeInt settled))
  assertEq failed [True] =<< readIORef callbacks

-- | Inputs show the value the caller passes: a value the caller changes
-- between frames is shown, a user edit the caller passes back is kept, and
-- one it ignores is undone on the next frame. 'runControlledStateTest' covers
-- a declined checkbox toggle.
runControlledInputsTest :: Context -> IORef Int -> IO ()
runControlledInputsTest ctx failed = do
  let
    inp = withInputOff 300 200
    ui (checked, text) = column $ do
      checked' <- checkbox "Opt" checked
      text' <- textInput text
      pure (checked', text')
  _ <- warmup2 ctx inp (ui (False, "one"))
  forM_
    [ -- The caller's new values, with no input.
      (inp, (True, "two"), (True, "two"))
    , -- Tab focuses the checkbox; Space toggles it and the caller keeps it.
      (tabInp inp, (True, "two"), (True, "two"))
    , (inp {inputChars = " "}, (True, "two"), (False, "two"))
    , (tabInp inp, (False, "two"), (False, "two"))
    , -- Tab moved focus to the field. A kept edit stays.
      (inp {inputChars = "x"}, (False, "two"), (False, "twox"))
    , (inp, (False, "twox"), (False, "twox"))
    , -- An ignored edit is undone on the following frame.
      (inp {inputChars = "y"}, (False, "twox"), (False, "twoxy"))
    , (inp, (False, "twox"), (False, "twox"))
    ]
    $ \(input, value, expected) -> do
      (result, _, _, _) <- runFrame ctx input (ui value)
      assertEq failed expected result
      spans <- collectTextSpans ctx
      assert failed (any (\(_, txt, _, _, _) -> txt == snd expected) spans)

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
